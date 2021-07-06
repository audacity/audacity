#!/bin/sh

lib="${0%/*}/lib/sneedacity"
share="${0%/*}/share/sneedacity"

export LD_LIBRARY_PATH="${lib}:${LD_LIBRARY_PATH}"
export SNEEDACITY_MODULES_PATH="${SNEEDACITY_MODULES_PATH}:${lib}/modules"
export SNEEDACITY_PATH="${SNEEDACITY_PATH}:${share}"

exec "${0%/*}/bin/sneedacity" "$@"
