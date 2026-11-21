#!/usr/bin/env bash
set -eo pipefail

PGDATA="${PGDATA:-/usr/local/pgsql/data}"
PGLOG_DIR="${PGLOG_DIR:-/var/log/postgresql}"
PGLOG="${PGLOG:-${PGLOG_DIR}/postgresql.log}"

# Ensure runtime directories exist
mkdir -p /var/run/postgresql /tmp "${PGLOG_DIR}" "${PGDATA}"

# If PGDATA is empty (e.g. mounted as tmpfs or volume), populate from template_data or initdb
if [ ! -s "${PGDATA}/PG_VERSION" ]; then
    if [ -d /usr/local/pgsql/template_data ]; then
        cp -a /usr/local/pgsql/template_data/. "${PGDATA}/"
    else
        gosu postgres initdb -D "${PGDATA}" --auth=trust --auth-local=trust
        cat <<'EOF' >> "${PGDATA}/postgresql.conf"
listen_addresses = '*'
port = 5432
unix_socket_directories = '/var/run/postgresql, /tmp'
logging_collector = on
log_destination = 'stderr'
log_directory = '/var/log/postgresql'
log_filename = 'postgresql.log'
log_truncate_on_rotation = off
log_file_mode = 0644
log_statement = 'all'
log_min_messages = 'warning'
log_min_error_statement = 'error'
log_line_prefix = '%m [%p] %q%u@%d '
EOF
    fi
fi

# PostgreSQL requires PGDATA to have 0700 permissions and belong to postgres.
chown -R postgres:postgres /var/run/postgresql "${PGLOG_DIR}" "${PGDATA}"
chmod 1777 /tmp /var/run/postgresql
chmod 700 "${PGDATA}"

# Function to verify/setup occam databases
setup_occam_dbs() {
    for _ in {1..50}; do
        if pg_isready -h /var/run/postgresql 2>/dev/null; then
            break
        fi
        sleep 0.1
    done

    # Ensure root role exists (convenient when containers execute as root)
    gosu postgres psql -h /var/run/postgresql -v ON_ERROR_STOP=0 -c "CREATE ROLE root WITH SUPERUSER LOGIN;" 2>/dev/null || true

    # Ensure template_occam exists
    if ! gosu postgres psql -h /var/run/postgresql -tAc "SELECT 1 FROM pg_database WHERE datname='template_occam'" 2>/dev/null | grep -q 1; then
        gosu postgres psql -h /var/run/postgresql -v ON_ERROR_STOP=0 <<'EOF' 2>/dev/null || true
CREATE DATABASE template_occam;
ALTER DATABASE template_occam WITH is_template = true;
GRANT ALL PRIVILEGES ON DATABASE template_occam TO PUBLIC;
EOF
    fi

    # Ensure occam_test exists
    if ! gosu postgres psql -h /var/run/postgresql -tAc "SELECT 1 FROM pg_database WHERE datname='occam_test'" 2>/dev/null | grep -q 1; then
        gosu postgres psql -h /var/run/postgresql -v ON_ERROR_STOP=0 <<'EOF' 2>/dev/null || true
CREATE DATABASE occam_test TEMPLATE template_occam;
GRANT ALL PRIVILEGES ON DATABASE occam_test TO PUBLIC;
EOF
    fi
}

if [ "$1" = "postgres" ]; then
    (
        setup_occam_dbs
    ) &
    exec gosu postgres postgres -D "${PGDATA}"
elif [ $# -gt 0 ]; then
    gosu postgres pg_ctl -D "${PGDATA}" -l "${PGLOG}" -w start
    setup_occam_dbs
    exec "$@"
else
    (
        setup_occam_dbs
    ) &
    exec gosu postgres postgres -D "${PGDATA}"
fi
