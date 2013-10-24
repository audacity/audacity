#!/bin/sh -e

# This test script should exit on the first failure.

./test_libFLAC.sh
./test_libFLAC++.sh
./test_grabbag.sh
./test_flac.sh
./test_metaflac.sh
./test_seeking.sh
./test_streams.sh
