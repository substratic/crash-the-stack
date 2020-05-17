#!/bin/sh

# Create the output folder structure
mkdir -p dist/crash-the-stack.app/Contents/MacOS
mkdir -p dist/crash-the-stack.app/Contents/Resources
mkdir -p dist/crash-the-stack.app/Contents/Frameworks

# Populate Info.plist and Resources
cp scripts/macos/Info.plist dist/crash-the-stack.app/Contents/Info.plist
cp -R dist/assets dist/crash-the-stack.app/Contents/Resources

# Copy SDL2 frameworks
cp -R lib/macos/* dist/crash-the-stack.app/Contents/Frameworks
