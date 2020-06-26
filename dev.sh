#!/bin/sh

# Ensure dependencies
. ./install-deps.sh

./gambit/bin/gsi -:d3ar,~~userlib=./lib src/ build.scm --dev "$@"
