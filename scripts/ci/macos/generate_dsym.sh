#!/usr/bin/env bash

set -xe

function extractDSym()
{
    local lib=$1
    local libfile=$(basename $lib)
    local libname="${libfile%.*}"
    local targetdir=$2
    local dsymfile=${targetdir}/${libname}.dSYM

    if [[ -d "$dsymfile" ]]; then
        echo "Skipping dSYM generation for $libfile: dSYM exists"
    elif [[ ! -L "$lib" && "$lib" != "{}" && $lib != *"dSYM"* ]]; then
        echo "Extracting dSYMs from $libfile to $dsymfile"
        dsymutil "$lib" -o "$dsymfile"
    fi
}

export -f extractDSym

mkdir -p "$CONAN_USER_HOME/dsyms"

find $CONAN_USER_HOME -name "*.dylib" | xargs -I {} bash -c "extractDSym \"{}\" \"$CONAN_USER_HOME/dsyms\""
