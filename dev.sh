#!/bin/sh

# Ensure dependencies
. ./install-deps.sh

export DYLD_FRAMEWORK_PATH="$(pwd)/lib/macos"

./gambit/bin/gsi -:dar,~~bin=./gambit/bin,~~include=./gambit/include,~~lib=./gambit/lib,~~userlib=./lib src/ build.scm --dev "$@"
