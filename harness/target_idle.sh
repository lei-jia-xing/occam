#!/usr/bin/env bash
# Entrypoint for embedded-engine target images.
#
# Without arguments, keep the container alive so candidates can run through
# `docker exec -i CONTAINER occam-exec`, and exit promptly on stop. With
# arguments, run them instead, e.g. `docker run --rm -i IMAGE occam-exec`.
set -u

if [ "$#" -gt 0 ]; then
    exec "$@"
fi

trap 'exit 0' TERM INT
sleep infinity &
wait
