#!/usr/bin/env bash
set -eo pipefail

# 1. Start PostgreSQL service
PG_VER=$(ls /usr/lib/postgresql 2>/dev/null | sort -V | tail -n 1 || true)
if [ -n "$PG_VER" ]; then
    mkdir -p /var/run/postgresql /tmp
    chown -R postgres:postgres /var/run/postgresql /var/lib/postgresql 2>/dev/null || true
    if [ ! -d "/var/lib/postgresql/$PG_VER/main" ]; then
        pg_dropcluster "$PG_VER" main 2>/dev/null || true
        pg_createcluster "$PG_VER" main
    fi
    if [ -f "/etc/postgresql/$PG_VER/main/postgresql.conf" ]; then
        sed -i "s|#*unix_socket_directories = .*|unix_socket_directories = '/var/run/postgresql, /tmp'|" "/etc/postgresql/$PG_VER/main/postgresql.conf" 2>/dev/null || true
    fi
    if [ -f "/etc/postgresql/$PG_VER/main/pg_hba.conf" ]; then
        sed -i -E 's/(local\s+all\s+all\s+)peer/\1trust/' "/etc/postgresql/$PG_VER/main/pg_hba.conf" 2>/dev/null || true
    fi
fi

service postgresql start

for i in {1..30}; do
    if pg_isready -h /var/run/postgresql 2>/dev/null || pg_isready -h /tmp 2>/dev/null; then
        break
    fi
    sleep 0.2
done

if [ -S /var/run/postgresql/.s.PGSQL.5432 ] && [ ! -S /tmp/.s.PGSQL.5432 ]; then
    ln -sf /var/run/postgresql/.s.PGSQL.5432 /tmp/.s.PGSQL.5432 2>/dev/null || true
fi

su - postgres -c "psql -v ON_ERROR_STOP=0" <<'EOF' 2>/dev/null || true
CREATE ROLE root WITH SUPERUSER LOGIN;
CREATE DATABASE template_occam;
ALTER DATABASE template_occam WITH is_template = true;
CREATE DATABASE occam_test TEMPLATE template_occam;
GRANT ALL PRIVILEGES ON DATABASE template_occam TO PUBLIC;
GRANT ALL PRIVILEGES ON DATABASE occam_test TO PUBLIC;
EOF

# 2. Start MySQL service
mkdir -p /var/run/mysqld /var/lib/mysql
chown -R mysql:mysql /var/run/mysqld /var/lib/mysql
if [ ! -d "/var/lib/mysql/mysql" ]; then
    mysqld --initialize-insecure --user=mysql
fi

service mysql start

for i in {1..30}; do
    if mysqladmin ping --silent 2>/dev/null || mysqladmin ping -h 127.0.0.1 --silent 2>/dev/null; then
        break
    fi
    sleep 0.2
done

mysql -u root <<'EOF' 2>/dev/null || true
CREATE USER IF NOT EXISTS 'root'@'127.0.0.1' IDENTIFIED BY '';
CREATE USER IF NOT EXISTS 'root'@'%' IDENTIFIED BY '';
GRANT ALL PRIVILEGES ON *.* TO 'root'@'localhost' WITH GRANT OPTION;
GRANT ALL PRIVILEGES ON *.* TO 'root'@'127.0.0.1' WITH GRANT OPTION;
GRANT ALL PRIVILEGES ON *.* TO 'root'@'%' WITH GRANT OPTION;
CREATE DATABASE IF NOT EXISTS template_occam;
CREATE DATABASE IF NOT EXISTS occam_test;
FLUSH PRIVILEGES;
EOF

# 3. Dispatch execution
if [ $# -gt 0 ]; then
    exec "$@"
else
    exec ./benchmarks/run_all.sh
fi
