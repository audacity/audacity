#!/usr/bin/env bash
# The AppImage runtime sets some special environment variables. We provide
# default values here in case the user tries to run this script outside an
# AppImage where the variables would otherwise be undefined.
if [[ ! "${APPIMAGE}" || ! "${APPDIR}" ]]; then
    export APPIMAGE="$(readlink -f "${0}")"
    export APPDIR="$(dirname "${APPIMAGE}")"
fi

# Check system libraries and load a fallback if necessary
fallback_libs="" # start empty
for fb_dir in "${APPDIR}/fallback"/*; do
  if [[ -d "${fb_dir}" ]]; then
    library="${fb_dir##*/}" # library named like directory
    if ! "${APPDIR}/bin/findlib" "${library}" >&2; then
      echo "${APPIMAGE}: Using fallback for library '${library}'" >&2
      fallback_libs="${fallback_libs}:${fb_dir}" # append path
    fi
  fi
done

export LD_LIBRARY_PATH="${APPDIR}/lib:${LD_LIBRARY_PATH}${fallback_libs}"

export AUDACITY_PATH="${AUDACITY_PATH}:${APPDIR}/share/audacity"
export AUDACITY_MODULES_PATH="${AUDACITY_MODULES_PATH}:${APPDIR}/lib/modules"
export UBUNTU_MENUPROXY=0

function help()
{
    # Normal audacity help
    "${APPDIR}/bin/audacity" --help
    # Special options handled by this script
    cat >&2 <<EOF
  --readme              display README
  --license             display LICENSE
  --man[ual|page]       display audacity(1) manual page
  --check-dependencies  check library dependency fulfillment (developer tool)

EOF
    # Blank line then special options handled by the AppImage runtime
    "${APPIMAGE}" --appimage-help
}

# Intercept command line arguments
case "$1" in
-h|--help )
    help
    ;;
--readme )
    exec less "${APPDIR}/share/doc/audacity/README.md"
    ;;
--license )
    exec less "${APPDIR}/share/doc/audacity/LICENSE.txt"
    ;;
--man|--manual|--manpage )
    exec man "${APPDIR}/share/man/man1/audacity.1"
    ;;
--check-depends|--check-dependencies )
    exec bash "${APPDIR}/bin/check_dependencies"
    ;;
* )
    # Other arguments go to Audacity
    exec "${APPDIR}/bin/audacity" "$@"
    ;;
esac
