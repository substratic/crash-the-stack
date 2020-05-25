#!/bin/sh

export DYLD_FRAMEWORK_PATH="$(pwd)/lib/macos"

# Ensure dependencies
./install-deps.sh

# Don't enable debugging on CI
debugarg="dar-,"
if [ "$CI" = "true" ]; then
    debugarg=""
fi

./gambit/bin/gsi -:$debugarg~~bin=./gambit/bin,~~lib=./gambit/lib,~~userlib=./lib src/ lib/ build.scm --test
