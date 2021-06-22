#!/usr/bin/env bash

set -xe

function extractDSym()
{
    local lib=$1
    local libfile=$(basename $lib)
    local libname="${libfile%.*}"
    local targetdir=$(dirname $lib)

    if [[ ! -L "$lib" && "$lib" != "{}" && $lib != *"dSYM"* ]]; then
        echo "Extracting dSYMs from ${libfile} to ${libname}.dSYM"
        dsymutil "$lib" -o "${targetdir}/${libname}.dSYM"
    fi
}

export -f extractDSym

find $CONAN_USER_HOME -name "*.dylib" | xargs -I {} bash -c 'extractDSym "{}"'
