-- Single source of truth for all six supported target DBMS targets.
let T = ./Types.dhall

let specs
    : List T.DbmsSpec
    = [ { id = T.DbmsId.postgres
        , occam_dialect = T.Dialect.postgresql
        , server_based = True
        , container_image = "occam-target-pg"
        , default_port = Some 5432
        , container_log = Some "/var/log/postgresql/postgresql.log"
        , default_host = Some "127.0.0.1"
        , default_user = Some "postgres"
        , default_db = "occam_test"
        , sqlancer_subcommand = Some "postgres"
        , sqlancer_port = Some 5432
        , squirrel_db = Some "postgresql"
        }
      , { id = T.DbmsId.mysql
        , occam_dialect = T.Dialect.mysql
        , server_based = True
        , container_image = "occam-target-mysql"
        , default_port = Some 3306
        , container_log = Some "/var/log/mysql/error.log"
        , default_host = Some "127.0.0.1"
        , default_user = Some "root"
        , default_db = "occam_test"
        , sqlancer_subcommand = Some "mysql"
        , sqlancer_port = Some 3306
        , squirrel_db = Some "mysql"
        }
      , { id = T.DbmsId.mariadb
        , occam_dialect = T.Dialect.mariadb
        , server_based = True
        , container_image = "occam-target-mariadb"
        , default_port = Some 3306
        , container_log = Some "/var/log/mysql/error.log"
        , default_host = Some "127.0.0.1"
        , default_user = Some "root"
        , default_db = "occam_test"
        , sqlancer_subcommand = Some "mariadb"
        , sqlancer_port = Some 3306
        , squirrel_db = Some "mysql"
        }
      , { id = T.DbmsId.clickhouse
        , occam_dialect = T.Dialect.clickhouse
        , server_based = True
        , container_image = "occam-target-clickhouse"
        , default_port = Some 9000
        , container_log =
            Some "/var/log/clickhouse-server/clickhouse-server.err.log"
        , default_host = Some "127.0.0.1"
        , default_user = Some "default"
        , default_db = "occam_test"
        , sqlancer_subcommand = Some "clickhouse"
        , sqlancer_port = Some 8123
        , squirrel_db = None Text
        }
      , { id = T.DbmsId.sqlite
        , occam_dialect = T.Dialect.sqlite
        , server_based = False
        , container_image = "occam-target-sqlite"
        , default_port = None Natural
        , container_log = None Text
        , default_host = None Text
        , default_user = None Text
        , default_db = ":memory:"
        , sqlancer_subcommand = Some "sqlite3"
        , sqlancer_port = None Natural
        , squirrel_db = Some "sqlite"
        }
      , { id = T.DbmsId.duckdb
        , occam_dialect = T.Dialect.duckdb
        , server_based = False
        , container_image = "occam-target-duckdb"
        , default_port = None Natural
        , container_log = None Text
        , default_host = None Text
        , default_user = None Text
        , default_db = ":memory:"
        , sqlancer_subcommand = Some "duckdb"
        , sqlancer_port = None Natural
        , squirrel_db = None Text
        }
      ]

in  specs
