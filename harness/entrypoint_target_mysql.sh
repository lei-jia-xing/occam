#!/usr/bin/env bash
# entrypoint_target_mysql.sh - Lifecycle manager and crash supervisor for MySQL 8.0 debug container
set -eo pipefail

export PATH="/usr/local/mysql/bin:/usr/sbin:/usr/bin:${PATH}"
DATADIR="${MYSQL_DATADIR:-/var/lib/mysql}"
TEMPLATE_DIR="/var/lib/mysql_template"
LOG_DIR="/var/log/mysql"
ERROR_LOG="${LOG_DIR}/error.log"
RUN_DIR="/var/run/mysqld"

# Ensure runtime directories exist with mysql ownership
mkdir -p "$DATADIR" "$LOG_DIR" "$RUN_DIR"
touch "$ERROR_LOG"
chown -R mysql:mysql "$DATADIR" "$LOG_DIR" "$RUN_DIR"
chmod 777 "$RUN_DIR"

# Restore from template if datadir is empty (e.g. fresh volume or tmpfs)
if [ ! -d "${DATADIR}/mysql" ]; then
    if [ -d "$TEMPLATE_DIR" ]; then
        echo "[entrypoint_target_mysql] Restoring pristine datadir from template..."
        cp -a "${TEMPLATE_DIR}/." "${DATADIR}/"
        chown -R mysql:mysql "$DATADIR"
    else
        echo "[entrypoint_target_mysql] Initializing new datadir with --initialize-insecure..."
        mysqld-debug --initialize-insecure --user=mysql --datadir="$DATADIR"
        chown -R mysql:mysql "$DATADIR"
    fi
fi

# Graceful stop handling for container teardown
stop_requested=0
child_pid=""

cleanup() {
    echo "[entrypoint_target_mysql] Stopping mysqld-debug..."
    stop_requested=1
    if [ -n "$child_pid" ]; then
        kill -TERM "$child_pid" 2>/dev/null || true
        wait "$child_pid" 2>/dev/null || true
    fi
    exit 0
}

trap cleanup SIGTERM SIGINT

# Supervisor loop: automatically relaunches mysqld-debug after assertion aborts/crashes
echo "[entrypoint_target_mysql] Starting mysqld-debug supervisor..."
while [ "$stop_requested" -eq 0 ]; do
    gosu mysql mysqld-debug \
        --user=mysql \
        --datadir="$DATADIR" \
        --log-error="$ERROR_LOG" \
        --log-error-verbosity=3 \
        --core-file \
        --bind-address=0.0.0.0 \
        --port=3306 &
    child_pid=$!

    # Wait for server to either crash or stop
    wait "$child_pid" || true
    exit_status=$?

    if [ "$stop_requested" -eq 1 ]; then
        break
    fi

    echo "[entrypoint_target_mysql] mysqld-debug terminated (exit status: $exit_status). Restarting server for next test..."
    sleep 0.3
done
