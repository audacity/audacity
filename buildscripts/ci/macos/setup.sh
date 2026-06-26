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
if ! command -v cmake >/dev/null 2>&1
then
    brew install cmake ninja pkg-config --formula --quiet
fi

echo "Setup script done"
