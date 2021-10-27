#!/usr/bin/env bash

((${BASH_VERSION%%.*} >= 4)) || { echo >&2 "$0: Error: Please upgrade Bash."; exit 1; }

set -euxo pipefail

cd build

if [[ "${OSTYPE}" == msys* ]]; then # Windows
   if [[ ${GIT_BRANCH} == release* ]]; then
      cmake --build . --target innosetup --config "${AUDACITY_BUILD_TYPE}"
   fi

   # Chocolatey went wild again (6 years passed!) and added cpack.exe alias to choco pack
   # This breaks the GitHub actions
   cmake --build . --target package --config "${AUDACITY_BUILD_TYPE}"
else # Linux & others
   cpack -C "${AUDACITY_BUILD_TYPE}" --verbose
fi

# Remove the temporary directory
rm -Rf package/_CPack_Packages
