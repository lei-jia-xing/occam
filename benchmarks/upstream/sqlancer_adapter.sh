#!/usr/bin/env bash
set -euo pipefail

PIN="a7d60ee450e6a22ca38599c14e984a49773f4745"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SOURCE="${SQLANCER_SOURCE:-${XDG_CACHE_HOME:-$HOME/.cache}/occam/sqlancer-$PIN}"

unsupported() {
    echo "unsupported: $*" >&2
    exit 78
}

command -v java >/dev/null 2>&1 || unsupported "SQLancer adapter requires Java 11+"
command -v javac >/dev/null 2>&1 || unsupported "SQLancer adapter requires a JDK to compile the Reproducer adapter"
command -v git >/dev/null 2>&1 || unsupported "SQLancer adapter requires git to verify the prepared source pin"

[[ -d "$SOURCE/.git" ]] || unsupported "prepare a Git checkout at $PIN before benchmarking (SQLANCER_SOURCE=$SOURCE)"
actual_pin=$(git -C "$SOURCE" rev-parse HEAD 2>/dev/null || true)
[[ "$actual_pin" == "$PIN" ]] || unsupported "SQLANCER_SOURCE must be exactly $PIN (found ${actual_pin:-not a git checkout})"

JAR="$SOURCE/target/sqlancer-2.0.0.jar"
[[ -f "$JAR" && -d "$SOURCE/target/lib" ]] || unsupported "build the pinned checkout before benchmarking: mvn -q -Dmaven.test.skip=true package -f $SOURCE/pom.xml"

classes=$(mktemp -d)
trap 'rm -rf "$classes"' EXIT
classpath="$JAR:$SOURCE/target/lib/*"
javac -cp "$classpath" -d "$classes" "$SCRIPT_DIR/sqlancer/PredicateReducer.java"
java -cp "$classes:$classpath" sqlancer.PredicateReducer "$@"
