-- Typed SQUIRREL fuzzer profile definitions shared with harness/squirrel_driver.sh.
let T = ./Types.dhall

let mkMysqlSquirrel =
      \(dbmsName : Text) ->
        { dbms = dbmsName
        , mutator_dbms = "mysql"
        , default_input_subdir = "mysql_input"
        , target_bin_rel = "build/db_driver"
        , db = "mysql"
        , required =
            [ "init_lib"
            , "data_lib"
            , "db"
            , "passwd"
            , "user_name"
            , "host"
            , "sock_path"
            , "db_prefix"
            , "startup_cmd"
            ]
        , should_exist = [ "init_lib", "data_lib" ]
        , init_lib_rel = "data/fuzz_root/mysql_init_lib"
        , data_lib_rel = Some "data/fuzz_root/global_data_lib_mysql"
        , pragma_rel = None Text
        , default_host = Some "127.0.0.1"
        , default_port = Some 3306
        , default_user = Some "root"
        , default_db = Some "test"
        , default_socket = Some "/tmp/mysql.sock"
        }

let profiles
    : List T.SquirrelProfile
    = [ { dbms = "sqlite"
        , mutator_dbms = "sqlite"
        , default_input_subdir = "sqlite_input"
        , target_bin_rel = "bin/sqlite_ossfuzz"
        , db = "sqlite"
        , required = [ "init_lib", "pragma" ]
        , should_exist = [ "init_lib", "pragma" ]
        , init_lib_rel = "data/fuzz_root/init_lib"
        , data_lib_rel = None Text
        , pragma_rel = Some "data/fuzz_root/pragma"
        , default_host = None Text
        , default_port = None Natural
        , default_user = None Text
        , default_db = None Text
        , default_socket = None Text
        }
      , { dbms = "postgresql"
        , mutator_dbms = "postgresql"
        , default_input_subdir = "pqsql_input"
        , target_bin_rel = "build/db_driver"
        , db = "postgresql"
        , required =
            [ "init_lib"
            , "data_lib"
            , "db"
            , "passwd"
            , "user_name"
            , "host"
            , "db_name"
            , "startup_cmd"
            ]
        , should_exist = [ "init_lib", "data_lib" ]
        , init_lib_rel = "data/fuzz_root/pqsql_init_lib"
        , data_lib_rel = Some "data/fuzz_root/global_data_lib_pqsql"
        , pragma_rel = None Text
        , default_host = Some "127.0.0.1"
        , default_port = Some 5432
        , default_user = Some "postgres"
        , default_db = Some "postgres"
        , default_socket = None Text
        }
      , mkMysqlSquirrel "mysql"
      , mkMysqlSquirrel "mariadb"
      ]

in  profiles
