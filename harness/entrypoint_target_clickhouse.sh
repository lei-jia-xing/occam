#!/usr/bin/env bash
# entrypoint_target_clickhouse.sh - Lifecycle manager and crash supervisor for ClickHouse target container
set -eo pipefail

DATADIR="/var/lib/clickhouse"
TEMPLATE_DIR="/var/lib/clickhouse_template"
LOG_DIR="/var/log/clickhouse-server"
ERR_LOG="${LOG_DIR}/clickhouse-server.err.log"
MAIN_LOG="${LOG_DIR}/clickhouse-server.log"
RUN_DIR="/var/run/clickhouse-server"
CONFIG="/etc/clickhouse-server/config.xml"

mkdir -p "$DATADIR" "$LOG_DIR" "$RUN_DIR"
touch "$ERR_LOG" "$MAIN_LOG"
chown -R clickhouse:clickhouse "$DATADIR" "$LOG_DIR" "$RUN_DIR"
chmod 0644 "$ERR_LOG" "$MAIN_LOG"

if [ "${1:-clickhouse-server}" != "clickhouse-server" ]; then
    exec "$@"
fi

# Restore from template if datadir is empty (e.g. fresh volume or tmpfs)
if [ ! -d "${DATADIR}/metadata" ]; then
    echo "[entrypoint_target_clickhouse] Restoring pristine datadir from template..."
    cp -a "${TEMPLATE_DIR}/." "${DATADIR}/"
    chown -R clickhouse:clickhouse "$DATADIR"
fi

stop_requested=0
child_pid=""

cleanup() {
    echo "[entrypoint_target_clickhouse] Stopping clickhouse-server..."
    stop_requested=1
    if [ -n "$child_pid" ]; then
        kill -TERM "$child_pid" 2>/dev/null || true
        wait "$child_pid" 2>/dev/null || true
    fi
    exit 0
}

trap cleanup SIGTERM SIGINT

# Supervisor loop: automatically relaunch clickhouse-server after crashes or aborts.
echo "[entrypoint_target_clickhouse] Starting clickhouse-server supervisor..."
while [ "$stop_requested" -eq 0 ]; do
    gosu clickhouse clickhouse-server --config-file="$CONFIG" &
    child_pid=$!

    if wait "$child_pid"; then
        exit_status=0
    else
        exit_status=$?
    fi

    if [ "$stop_requested" -eq 1 ]; then
        break
    fi

    echo "[entrypoint_target_clickhouse] clickhouse-server terminated (exit status: $exit_status). Restarting server for next test..." | tee -a "$ERR_LOG"
    sleep 0.3
done
