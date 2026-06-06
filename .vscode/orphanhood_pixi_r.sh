#!/bin/bash
# R wrapper script for the orphanhood-burden-mexico pixi environment.
cd "$(dirname "$0")/.." || exit 1

# Prefer radian for a better REPL; fall back to base R.
if pixi run which radian >/dev/null 2>&1; then
	exec pixi run radian "$@"
else
	exec pixi run R "$@"
fi
