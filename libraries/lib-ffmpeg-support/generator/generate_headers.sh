#!/usr/bin/env bash

set -euxo pipefail

scriptLocation=$(dirname "$(readlink -f "$0")")

version="$1"
dirName="ffmpeg-${version}"
archiveName="${dirName}.tar.gz"

echo "Donwnloading FFMPEG ${version}..."
wget "https://www.ffmpeg.org/releases/${archiveName}"

echo "Unpacking FFMPEG..."
tar xf "${archiveName}"

echo "Generating headers..."

python3 "${scriptLocation}/generate_headers.py" \
   --input generator.cpp \
   --output "${scriptLocation}/../impl/ffmpeg-${version}-single-header.h" \
   --include-dir "${dirName}"

rm -R "${dirName}"
rm "${archiveName}"
