#!/bin/sh

# Ensure dependencies
./install-deps.sh

# Run the build script
./gambit/bin/gsi -:~~lib=./gambit/lib,~~userlib=./lib src/ build.scm
