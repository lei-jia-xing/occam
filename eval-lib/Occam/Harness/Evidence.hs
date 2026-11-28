-- | Pure DBMS server-log and process-stderr crash evidence extractors (anti-spoofing).
module Occam.Harness.Evidence (
    clickHouseConnectionLostPhrases,
    connectionLostPhrasesForDbms,
    extractClickHouseEvidence,
    extractDbmsEvidence,
    extractDuckDbEvidence,
    extractMariadbEvidence,
    extractMysqlEvidence,
    extractPostgresEvidence,
    extractSqliteEvidence,
    hasPsqlMetaCommand,
    looksLikeBacktraceLine,
    mariadbConnectionLostPhrases,
    mysqlConnectionLostPhrases,
    pgConnectionLostPhrases,
) where

import Data.Text (Text)
import Occam.Harness.Evidence.ClickHouse (extractClickHouseEvidence)
import Occam.Harness.Evidence.Common (
    clickHouseConnectionLostPhrases,
    connectionLostPhrasesForDbms,
    hasPsqlMetaCommand,
    looksLikeBacktraceLine,
    mariadbConnectionLostPhrases,
    mysqlConnectionLostPhrases,
    pgConnectionLostPhrases,
 )
import Occam.Harness.Evidence.DuckDB (extractDuckDbEvidence)
import Occam.Harness.Evidence.Mysql (extractMariadbEvidence, extractMysqlEvidence)
import Occam.Harness.Evidence.Postgres (extractPostgresEvidence)
import Occam.Harness.Evidence.Sqlite (extractSqliteEvidence)
import Occam.Harness.Signature (DbmsKind (..), EvidenceResult)

-- | Dispatch evidence extraction to the dialect-specific anti-spoofing extractor.
extractDbmsEvidence :: DbmsKind -> Text -> Text -> Int -> EvidenceResult
extractDbmsEvidence dbms serverLogText stderrText retcode = case dbms of
    DbmsPostgres -> extractPostgresEvidence serverLogText stderrText
    DbmsMySql -> extractMysqlEvidence serverLogText stderrText
    DbmsMariaDb -> extractMariadbEvidence serverLogText stderrText
    DbmsClickHouse -> extractClickHouseEvidence serverLogText stderrText
    DbmsSqlite -> extractSqliteEvidence stderrText retcode
    DbmsDuckDb -> extractDuckDbEvidence stderrText retcode
