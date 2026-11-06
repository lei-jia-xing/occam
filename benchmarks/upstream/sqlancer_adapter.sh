#!/usr/bin/env bash
set -euo pipefail

PIN="a7d60ee450e6a22ca38599c14e984a49773f4745"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SOURCE="${SQLANCER_SOURCE:-${XDG_CACHE_HOME:-$HOME/.cache}/occam/sqlancer-$PIN}"
REPO="https://github.com/sqlancer/sqlancer.git"

unsupported() {
    echo "unsupported: $*" >&2
    exit 78
}

command -v java >/dev/null 2>&1 || unsupported "SQLancer adapter requires Java 11+"
command -v javac >/dev/null 2>&1 || unsupported "SQLancer adapter requires a JDK to compile the Reproducer adapter"
command -v mvn >/dev/null 2>&1 || unsupported "SQLancer adapter requires Maven"
command -v git >/dev/null 2>&1 || unsupported "SQLancer adapter requires git to fetch the pinned source"

if [[ ! -d "$SOURCE/.git" ]]; then
    mkdir -p "$(dirname "$SOURCE")"
    git init -q "$SOURCE"
    git -C "$SOURCE" remote add origin "$REPO"
    git -C "$SOURCE" fetch -q --depth 1 origin "$PIN" || unsupported "cannot fetch SQLancer pin $PIN"
    git -C "$SOURCE" checkout -q --detach FETCH_HEAD
fi
actual_pin=$(git -C "$SOURCE" rev-parse HEAD 2>/dev/null || true)
[[ "$actual_pin" == "$PIN" ]] || unsupported "SQLANCER_SOURCE must be exactly $PIN (found ${actual_pin:-not a git checkout})"

mvn -q -DskipTests package -f "$SOURCE/pom.xml" || exit 1
JAR="$SOURCE/target/sqlancer-2.0.0.jar"
[[ -f "$JAR" ]] || exit 1

classes=$(mktemp -d)
trap 'rm -rf "$classes"' EXIT
classpath="$JAR:$SOURCE/target/lib/*"
javac -cp "$classpath" -d "$classes" "$SCRIPT_DIR/sqlancer/PredicateReducer.java"
java -cp "$classes:$classpath" sqlancer.PredicateReducer "$@"
