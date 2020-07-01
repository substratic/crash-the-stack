#!/usr/bin/env bash

SUBSTRATIC_VERSION=

# Ensure substratic modules are installed
[[ ! -d lib/github.com/substratic/sdl2 ]] && ./gambit/bin/gsi -:~~bin=./gambit/bin,~~lib=./gambit/lib,~~userlib=./lib -install github.com/substratic/sdl2
[[ ! -d lib/github.com/substratic/build ]] && ./gambit/bin/gsi -:~~bin=./gambit/bin,~~lib=./gambit/lib,~~userlib=./lib -install github.com/substratic/build
[[ ! -d lib/github.com/substratic/forge ]] && ./gambit/bin/gsi -:~~bin=./gambit/bin,~~lib=./gambit/lib,~~userlib=./lib -install github.com/substratic/forge
[[ ! -d lib/github.com/substratic/engine ]] && ./gambit/bin/gsi -:~~bin=./gambit/bin,~~lib=./gambit/lib,~~userlib=./lib -install github.com/substratic/engine
