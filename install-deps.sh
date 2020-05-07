#!/bin/sh

SUBSTRATIC_VERSION=

# Ensure substratic modules are installed
[[ ! -d lib/github.com/substratic/sdl2 ]] && ./gambit/bin/gsi -:~~lib=./gambit/lib,~~userlib=./lib -install github.com/substratic/sdl2
[[ ! -d lib/github.com/substratic/build ]] && ./gambit/bin/gsi -:~~lib=./gambit/lib,~~userlib=./lib -install github.com/substratic/build
[[ ! -d lib/github.com/substratic/engine ]] && ./gambit/bin/gsi -:~~lib=./gambit/lib,~~userlib=./lib -install github.com/substratic/engine
