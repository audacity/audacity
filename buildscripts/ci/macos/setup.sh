#!/usr/bin/env bash
echo "Setup MacOS build environment"

trap 'echo Setup failed; exit 1' ERR
SKIP_ERR_FLAG=true

BUILD_TOOLS=$HOME/build_tools
ENV_FILE=$BUILD_TOOLS/environment.sh

mkdir -p $BUILD_TOOLS

# Let's remove the file with environment variables to recreate it
rm -f $ENV_FILE

echo "echo 'Setup build environment'" >> $ENV_FILE

export MACOSX_DEPLOYMENT_TARGET=10.15

# fixing install python 3.9 error (it is a dependency for ninja)
rm '/usr/local/bin/2to3'
brew install ninja pkg-config

brew install cmake

# Dump syms
wget -q --show-progress -O dump_syms.7z "https://s3.amazonaws.com/utils.musescore.org/breakpad/macos/x86-64/dump_syms.7z"
7z x -y dump_syms.7z -o"$BUILD_TOOLS/breakpad"

# VST SDK
wget -q --show-progress -O vst_sdk.7z "https://s3.amazonaws.com/utils.musescore.org/VST3_SDK_379.7z"
7z x -y vst_sdk.7z -o"$BUILD_TOOLS/vst"
echo "export VST3_SDK_PATH=$VST3_SDK_PATH" >> $ENV_FILE


echo "Setup script done"
