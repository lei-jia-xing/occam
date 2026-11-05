#!/usr/bin/env bash
set -eo pipefail

# Detect container runtime: podman or docker
if command -v docker >/dev/null 2>&1; then
    CONTAINER_ENGINE="docker"
elif command -v podman >/dev/null 2>&1; then
    CONTAINER_ENGINE="podman"
else
    echo "Error: neither docker nor podman found in PATH." >&2
    exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

IMAGE_NAME="occam-benchmark:latest"

cd "$ROOT_DIR"

# Build image if not present
if ! "$CONTAINER_ENGINE" image inspect "$IMAGE_NAME" >/dev/null 2>&1; then
    echo "Image '$IMAGE_NAME' not found. Building with $CONTAINER_ENGINE..."
    "$CONTAINER_ENGINE" build -t "$IMAGE_NAME" -f "$ROOT_DIR/Dockerfile.benchmark" "$ROOT_DIR"
fi

# Run container
exec "$CONTAINER_ENGINE" run --rm -it \
    -v "$(pwd):/workspace" \
    --tmpfs /var/lib/postgresql:rw,size=1G \
    --tmpfs /var/lib/mysql:rw,size=1G \
    "$IMAGE_NAME" "$@"
