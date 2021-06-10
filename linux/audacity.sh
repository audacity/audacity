#!/bin/sh

lib="${0%/*}/lib/audacity"

export LD_LIBRARY_PATH="${lib}:${LD_LIBRARY_PATH}"
export AUDACITY_MODULES_PATH="${lib}/modules"

exec "${0%/*}/bin/audacity" "$@"
