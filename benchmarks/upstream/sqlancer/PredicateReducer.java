package sqlancer;

import java.io.IOException;
import java.lang.reflect.Proxy;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.sql.Connection;
import java.sql.Statement;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.TimeUnit;

import sqlancer.common.log.Loggable;
import sqlancer.common.log.LoggableFactory;
import sqlancer.common.query.ExpectedErrors;
import sqlancer.common.query.Query;
import sqlancer.common.query.SQLQueryAdapter;
import sqlancer.common.schema.AbstractSchema;

/** Runs SQLancer's upstream statement and AST reducers with an external SQL predicate. */
public final class PredicateReducer {
    private PredicateReducer() {
    }

    public static void main(String[] args) throws Exception {
        Path input = null;
        Path output = null;
        Path calls = null;
        Path metrics = null;
        long timeoutMillis = 0;
        long budgetMillis = 0;
        int separator = -1;
        for (int i = 0; i < args.length; i++) {
            switch (args[i]) {
            case "--input":
                input = Paths.get(args[++i]);
                break;
            case "--output":
                output = Paths.get(args[++i]);
                break;
            case "--calls":
                calls = Paths.get(args[++i]);
                break;
            case "--metrics":
                metrics = Paths.get(args[++i]);
                break;
            case "--timeout-ms":
                timeoutMillis = Long.parseLong(args[++i]);
                break;
            case "--budget-ms":
                budgetMillis = Long.parseLong(args[++i]);
                break;
            case "--":
                separator = i;
                i = args.length;
                break;
            default:
                throw new IllegalArgumentException("Unknown argument: " + args[i]);
            }
        }
        if (input == null || output == null || calls == null || metrics == null || timeoutMillis < 1
                || budgetMillis < 1 || separator < 0 || separator + 1 >= args.length) {
            throw new IllegalArgumentException(
                    "Usage: PredicateReducer --input SQL --output SQL --calls JSONL --metrics JSON --timeout-ms N --budget-ms N -- CHECKER [ARGS...]");
        }

        List<String> sourceStatements = splitStatements(Files.readString(input, StandardCharsets.UTF_8));
        List<Query<?>> statements = new ArrayList<>();
        for (String statement : sourceStatements) {
            statements.add(new SQLQueryAdapter(statement, new ExpectedErrors(), true, false));
        }
        if (statements.isEmpty()) {
            throw new IllegalArgumentException("Input contains no SQL statements");
        }
        int astUnsupported = countAstUnsupported(sourceStatements);
        if (astUnsupported > 0) {
            System.err.println("JSQLParser could not parse " + astUnsupported + " of " + sourceStatements.size()
                    + " statements; SQLancer's statement reducer still runs, AST reductions skip those statements.");
        }
        writeCandidate(output, statements);

        List<String> oraclePrefix = new ArrayList<>();
        for (int i = separator + 1; i < args.length; i++) {
            oraclePrefix.add(args[i]);
        }
        PredicateReproducer reproducer = new PredicateReproducer(oraclePrefix, calls, output, timeoutMillis, budgetMillis);
        BenchProvider provider = new BenchProvider();
        BenchOptions dbmsOptions = new BenchOptions();
        MainOptions options = new MainOptions();
        long reduceSeconds = Math.max(1, (budgetMillis + 999) / 1000);
        setOption(options, "useReducer", true);
        setOption(options, "logEachSelect", false);
        setOption(options, "logExecutionTime", false);
        setOption(options, "maxStatementReduceTime", reduceSeconds);
        setOption(options, "maxASTReduceTime", reduceSeconds);

        StateToReproduce state = new StateToReproduce("occam", provider);
        state.setStatements(statements);
        BenchState original = newState(provider, dbmsOptions, options, state);
        BenchState reduced = newState(provider, dbmsOptions, options, state);

        long started = System.nanoTime();
        Runtime.getRuntime().addShutdownHook(new Thread(reproducer::cancelActiveChecker));
        if (reproducer.bugStillTriggers(original)) {
            new StatementReducer<>(provider).reduce(original, reproducer, reduced);
            if (reproducer.stopCode == 0 && !reproducer.budgetExpired()) {
                new ASTBasedReducer<>(provider).reduce(original, reproducer, reduced);
            }
        } else if (reproducer.stopCode == 0) {
            reproducer.stopCode = 77;
        }
        if (reproducer.stopCode == 0 && reproducer.budgetExpired()) {
            reproducer.stopCode = 124;
        }
        long elapsed = System.nanoTime() - started;
        writeMetrics(metrics, reproducer.checks, astUnsupported, sourceStatements.size(), elapsed, reproducer.stopCode);
        if (reproducer.stopCode != 0) {
            System.exit(reproducer.stopCode);
        }
    }

    private static int countAstUnsupported(List<String> statements) {
        int unsupported = 0;
        for (String statement : statements) {
            try {
                net.sf.jsqlparser.parser.CCJSqlParserUtil.parse(statement);
            } catch (Exception e) {
                unsupported++;
            }
        }
        return unsupported;
    }

    private static void writeMetrics(Path metrics, long callbacks, int astUnsupported, int statementCount,
            long elapsedNanos, int exitCode) throws IOException {
        Path parent = metrics.toAbsolutePath().getParent();
        Files.createDirectories(parent);
        String json = "{\"reducer_callbacks\":" + callbacks + ",\"ast_unsupported_statements\":" + astUnsupported
                + ",\"statement_count\":" + statementCount + ",\"elapsed_sec\":"
                + (elapsedNanos / 1_000_000_000.0) + ",\"exit_code\":" + exitCode + "}\n";
        Files.writeString(metrics, json, StandardCharsets.UTF_8);
    }

    private static BenchState newState(BenchProvider provider, BenchOptions dbmsOptions, MainOptions options,
            StateToReproduce state) {
        BenchState result = new BenchState();
        result.setState(state);
        result.setRandomly(new Randomly(0));
        result.setDatabaseName("occam");
        result.setMainOptions(options);
        result.setDbmsSpecificOptions(dbmsOptions);
        result.setStateLogger(new Main.StateLogger("occam", provider, options));
        return result;
    }

    private static void setOption(MainOptions options, String name, Object value) throws ReflectiveOperationException {
        java.lang.reflect.Field field = MainOptions.class.getDeclaredField(name);
        field.setAccessible(true);
        field.set(options, value);
    }

    private static void writeCandidate(Path output, List<? extends Query<?>> statements) throws IOException {
        StringBuilder sql = new StringBuilder();
        for (Query<?> statement : statements) {
            sql.append(statement.getQueryString()).append(System.lineSeparator());
        }
        Path parent = output.toAbsolutePath().getParent();
        Files.createDirectories(parent);
        Path temporary = Files.createTempFile(parent, "sqlancer-candidate-", ".sql");
        try {
            Files.writeString(temporary, sql, StandardCharsets.UTF_8);
            Files.move(temporary, output, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
        } catch (IOException e) {
            Files.deleteIfExists(temporary);
            throw e;
        }
    }

    private static List<String> splitStatements(String sql) {
        List<String> result = new ArrayList<>();
        int start = 0;
        int blockDepth = 0;
        char quote = 0;
        String dollarTag = null;
        for (int i = 0; i < sql.length(); i++) {
            char c = sql.charAt(i);
            char next = i + 1 < sql.length() ? sql.charAt(i + 1) : 0;
            if (blockDepth > 0) {
                if (c == '/' && next == '*') {
                    blockDepth++;
                    i++;
                } else if (c == '*' && next == '/') {
                    blockDepth--;
                    i++;
                }
                continue;
            }
            if (dollarTag != null) {
                if (sql.startsWith(dollarTag, i)) {
                    i += dollarTag.length() - 1;
                    dollarTag = null;
                }
                continue;
            }
            if (quote != 0) {
                if (c == '\\' && quote == '\'' && i + 1 < sql.length()) {
                    i++;
                } else if (c == quote) {
                    if (next == quote) {
                        i++;
                    } else {
                        quote = 0;
                    }
                }
                continue;
            }
            if (c == '-' && next == '-') {
                i += 2;
                while (i < sql.length() && sql.charAt(i) != '\n') {
                    i++;
                }
            } else if (c == '/' && next == '*') {
                blockDepth = 1;
                i++;
            } else if (c == '\'' || c == '"' || c == '`' || c == '[') {
                quote = c == '[' ? ']' : c;
            } else if (c == '$') {
                int end = sql.indexOf('$', i + 1);
                if (end >= 0) {
                    String tag = sql.substring(i + 1, end);
                    if (tag.isEmpty() || tag.matches("[A-Za-z_][A-Za-z_0-9]*")) {
                        dollarTag = sql.substring(i, end + 1);
                        i = end;
                    }
                }
            } else if (c == ';') {
                addStatement(result, sql.substring(start, i));
                start = i + 1;
            }
        }
        addStatement(result, sql.substring(start));
        return result;
    }

    private static void addStatement(List<String> statements, String statement) {
        String trimmed = statement.trim();
        if (!trimmed.isEmpty() && !trimmed.matches("(?s)(?:--[^\\n]*(?:\\n|$)|/\\*.*?\\*/|\\s)+")) {
            statements.add(trimmed + (trimmed.endsWith(";") ? "" : ";"));
        }
    }

    private static Object defaultValue(Class<?> type) {
        if (!type.isPrimitive()) {
            return null;
        }
        if (type == boolean.class) {
            return false;
        }
        if (type == char.class) {
            return '\0';
        }
        if (type == byte.class || type == short.class || type == int.class || type == long.class || type == float.class
                || type == double.class) {
            return 0;
        }
        return null;
    }

    private static Connection noOpConnection() {
        Statement statement = (Statement) Proxy.newProxyInstance(PredicateReducer.class.getClassLoader(),
                new Class<?>[] { Statement.class }, (proxy, method, methodArgs) -> defaultValue(method.getReturnType()));
        return (Connection) Proxy.newProxyInstance(PredicateReducer.class.getClassLoader(),
                new Class<?>[] { Connection.class }, (proxy, method, methodArgs) -> {
                    if (method.getName().equals("createStatement")) {
                        return statement;
                    }
                    return defaultValue(method.getReturnType());
                });
    }

    private static final class PredicateReproducer implements Reproducer<BenchState> {
        private final List<String> command;
        private final Path calls;
        private final Path output;
        private final long timeoutMillis;
        private final long deadlineNanos;
        private long checks;
        private int stopCode;
        private volatile Process activeChecker;

        void cancelActiveChecker() {
            Process process = activeChecker;
            if (process == null) {
                return;
            }
            List<ProcessHandle> descendants = process.descendants().collect(java.util.stream.Collectors.toList());
            process.destroy();
            try {
                if (!process.waitFor(250, TimeUnit.MILLISECONDS)) {
                    process.destroyForcibly();
                }
                for (ProcessHandle child : descendants) {
                    if (child.isAlive()) {
                        child.destroyForcibly();
                    }
                }
                process.waitFor();
            } catch (InterruptedException e) {
                descendants.forEach(ProcessHandle::destroyForcibly);
                process.destroyForcibly();
                Thread.currentThread().interrupt();
            }
        }

        PredicateReproducer(List<String> command, Path calls, Path output, long timeoutMillis, long budgetMillis) {
            this.command = command;
            this.calls = calls;
            this.output = output;
            this.timeoutMillis = timeoutMillis;
            this.deadlineNanos = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(budgetMillis);
        }

        boolean budgetExpired() {
            return System.nanoTime() >= deadlineNanos;
        }

        @Override
        public boolean bugStillTriggers(BenchState state) {
            if (stopCode != 0) {
                return false;
            }
            Path candidate = null;
            long started = System.nanoTime();
            String status = "tool-error";
            try {
                long remainingMillis = TimeUnit.NANOSECONDS.toMillis(deadlineNanos - started);
                if (remainingMillis < 1) {
                    status = "timeout";
                    stopCode = 124;
                    return false;
                }
                candidate = Files.createTempFile(output.toAbsolutePath().getParent(), "sqlancer-oracle-", ".sql");
                writeCandidate(candidate, state.getState().getStatements());
                List<String> invocation = new ArrayList<>(command);
                invocation.add(candidate.toString());
                Process process = new ProcessBuilder(invocation).directory(output.toAbsolutePath().getParent().toFile())
                        .redirectOutput(ProcessBuilder.Redirect.DISCARD).redirectError(ProcessBuilder.Redirect.DISCARD)
                        .start();
                activeChecker = process;
                boolean finished = process.waitFor(Math.min(timeoutMillis, remainingMillis), TimeUnit.MILLISECONDS);
                if (!finished) {
                    status = "timeout";
                    cancelActiveChecker();
                } else {
                    int exitCode = process.exitValue();
                    status = exitCode == 0 ? "accepted" : exitCode == 1 ? "rejected"
                            : exitCode == 124 ? "timeout" : exitCode == 126 ? "execution-limit" : "tool-error";
                    if (exitCode == 125 || (exitCode != 0 && exitCode != 1 && exitCode != 124 && exitCode != 126)) {
                        stopCode = 125;
                    } else if (exitCode == 126) {
                        stopCode = 126;
                    }
                    if (exitCode == 0) {
                        writeCandidate(output, state.getState().getStatements());
                    }
                }
                if (status.equals("timeout") && (checks == 0 || budgetExpired())) {
                    stopCode = 124;
                }
                return status.equals("accepted");
            } catch (Exception e) {
                status = "tool-error";
                stopCode = 125;
                return false;
            } finally {
                if (activeChecker != null && activeChecker.isAlive()) {
                    cancelActiveChecker();
                }
                activeChecker = null;
                checks++;
                long elapsed = System.nanoTime() - started;
                String line = "{\"call\":" + checks + ",\"status\":\"" + status + "\",\"duration_sec\":"
                        + (elapsed / 1_000_000_000.0) + "}\n";
                try {
                    Files.writeString(calls, line, StandardCharsets.UTF_8, StandardOpenOption.CREATE,
                            StandardOpenOption.APPEND);
                    if (candidate != null) {
                        Files.deleteIfExists(candidate);
                    }
                } catch (IOException ignored) {
                }
            }
        }
    }

    private static final class BenchOptions implements DBMSSpecificOptions<OracleFactory<BenchState>> {
        @Override
        public List<OracleFactory<BenchState>> getTestOracleFactory() {
            return Collections.emptyList();
        }
    }

    private static final class BenchState
            extends GlobalState<BenchOptions, AbstractSchema<?, ?>, SQLConnection> {
        @Override
        protected void executeEpilogue(Query<?> query, boolean success, ExecutionTimer timer) {
        }

        @Override
        protected AbstractSchema<?, ?> readSchema() {
            return null;
        }
    }

    private static final class BenchProvider implements DatabaseProvider<BenchState, BenchOptions, SQLConnection> {
        private final LoggableFactory loggableFactory = new LoggableFactory() {
            @Override
            protected Loggable createLoggable(String input, String suffix) {
                return () -> input + suffix;
            }

            @Override
            public Query<?> getQueryForStateToReproduce(String queryString) {
                return new SQLQueryAdapter(queryString, true);
            }

            @Override
            public Query<?> commentOutQuery(Query<?> query) {
                return new SQLQueryAdapter("-- " + query.getQueryString(), true);
            }

            @Override
            protected Loggable infoToLoggable(String time, String databaseName, String databaseVersion, long seedValue) {
                return () -> time + " " + databaseName + " " + databaseVersion + " " + seedValue;
            }

            @Override
            public Loggable convertStacktraceToLoggable(Throwable throwable) {
                return () -> throwable.toString();
            }
        };

        @Override
        public Class<BenchState> getGlobalStateClass() {
            return BenchState.class;
        }

        @Override
        public Class<BenchOptions> getOptionClass() {
            return BenchOptions.class;
        }

        @Override
        public Reproducer<BenchState> generateAndTestDatabase(BenchState state) {
            return null;
        }

        @Override
        public void generateAndTestDatabaseWithQueryPlanGuidance(BenchState state) {
        }

        @Override
        public SQLConnection createDatabase(BenchState state) {
            return new SQLConnection(noOpConnection());
        }

        @Override
        public String getDBMSName() {
            return "occam-crash-adapter";
        }

        @Override
        public LoggableFactory getLoggableFactory() {
            return loggableFactory;
        }

        @Override
        public StateToReproduce getStateToReproduce(String databaseName) {
            return new StateToReproduce(databaseName, this);
        }
    }
}
