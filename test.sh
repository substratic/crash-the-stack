#!/bin/sh

export DYLD_FRAMEWORK_PATH="$(pwd)/lib/macos"

# Don't enable debugging on CI
debugarg="-:dar- "
if [ "$CI" = "true" ]; then
    debugarg=""
fi

./gambit/bin/gsi $debugarg src/ lib/ build.scm --test
