#!/usr/bin/env bash

set -euo pipefail

buildDirectory=$1
buildType=$2

scriptLocation=$(dirname "$(readlink -f "$0")")
sourceLocation=$(readlink -f "$scriptLocation/../../..")

cmake -B "${buildDirectory}/x64" -S "${sourceLocation}" -DCMAKE_BUILD_TYPE="${buildType}" -DCMAKE_CONFIGURATION_TYPES="${buildType}" -DMACOS_ARCHITECTURE=x86_64 -G Xcode
cmake --build "${buildDirectory}/x64" --config "${buildType}"

cmake -B "${buildDirectory}/arm64" -S "${sourceLocation}" -DCMAKE_BUILD_TYPE="${buildType}" -DCMAKE_CONFIGURATION_TYPES="${buildType}" -DMACOS_ARCHITECTURE=arm64 -G Xcode -DIMAGE_COMPILER_EXECUTABLE="${buildDirectory}/x64/utils/${buildType}/image-compiler"
cmake --build "${buildDirectory}/arm64" --config "${buildType}"

rm -Rf "${buildDirectory}/Audacity.app"
cp -R "${buildDirectory}/x64/bin/${buildType}" "${buildDirectory}"

lipo_dir() {
    local dir="$1"
    local filter="$2"

    local x64dir="${buildDirectory}/x64/bin/${buildType}/Audacity.app/Contents/${dir}"
    local armdir="${buildDirectory}/arm64/bin/${buildType}/Audacity.app/Contents/${dir}"
    local outdir="${buildDirectory}/${buildType}/Audacity.app/Contents/${dir}"

    for filename in ${outdir}/${filter}; do
        [ -e "${filename}" ] || continue
        local baseName=$(basename "${filename}")
        echo "Processing ${baseName}"
        lipo -create "${x64dir}/${baseName}" "${armdir}/${baseName}" -output "${outdir}/${baseName}"
    done
}

lipo_dir "MacOS" "*"
lipo_dir "Frameworks" "*.dylib"
lipo_dir "modules" "*.so"
