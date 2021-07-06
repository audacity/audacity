#!/usr/bin/env bash

version="@CPACK_PACKAGE_NAME@ v@CPACK_PACKAGE_VERSION@ @CPACK_AUDACITY_ARCH_LABEL@"

export LC_ALL=C # Using `sort` a lot. Order depends on locale so override it.

# PROBLEM: We want to check dependencies provided by the system, but `ldd`
# looks in the current directory first so will show other package libraries.
# SOLUTION: Copy library to temporary folder and test it there.
function check_file()
{
    counter=$(($(cat "$2/.counter")+1))
    echo ${counter} > "$2/.counter"
    printf >&2 "Done ${counter} of $3.\r"
    cp "$1" "$2"
    file="$(basename "$1")"
    LANG=C LD_LIBRARY_PATH="" "${APPDIR}/bin/ldd_recursive" -uniq "$2/${file}" 2>/dev/null
    rm "$2/${file}" # delete library before we test the next one
}
export -f check_file # make function available in Bash subprocesses

function prep_result()
{
    sed -n "s|$2||p" "$1" | sort -f | tee "$3" | wc -l
}

if ! which less >/dev/null; then
    function less() { cat "$@"; } # use `cat` if `less` is unavailable
fi

tmp="$(mktemp -d)"
trap "rm -rf '${tmp}'" EXIT

cd "${APPDIR}"

find . -executable -type f \! -name "lib*.so*" > "${tmp}/exes.txt"
find . -name "lib*.so*"    \! -type l          > "${tmp}/libs.txt"

num_exes="$(<"${tmp}/exes.txt" xargs -n1 basename 2>/dev/null | tee "${tmp}/exes2.txt" | wc -l)"
num_libs="$(<"${tmp}/libs.txt" xargs -n1 basename 2>/dev/null | tee "${tmp}/libs2.txt" | wc -l)"

echo >&2 "AppImage contains ${num_exes} executables and ${num_libs} libraries."

echo >&2 "Checking dependencies for executables and libraries..."
include_libs="${tmp}/libs.txt"
num_includes="$((${num_libs}+${num_exes}))"

# Check dependencies against system. See 'check_file' function.
echo 0 > "${tmp}/.counter"
cat "${tmp}/exes.txt" "${include_libs}" | xargs -n1 -I '%%%' bash -c \
'check_file "${0}" "${1}" "${2}"' "%%%" "${tmp}" "${num_includes}" \; \
| sort | uniq > "${tmp}/deps.txt"
echo >&2 "Processing results."

mv "${tmp}/libs2.txt" "${tmp}/libs.txt"
mv "${tmp}/exes2.txt" "${tmp}/exes.txt"

# Have only checked system libraries. Now consider those in package:
<"${tmp}/libs.txt" xargs -n1 -I '%%%' sed -i 's|^%%% => not found$|%%% => package|' "${tmp}/deps.txt"
<"${tmp}/libs.txt" xargs -n1 -I '%%%' sed -i 's|%%%$|%%% => both|' "${tmp}/deps.txt"

# Remaining dependencies must be system:
sed -ri 's/^(.*[^(not found|package|both)])$/\1 => system/' "${tmp}/deps.txt"

num_package=$(prep_result "${tmp}/deps.txt" ' => package$'   "${tmp}/package.txt")
num_system=$(prep_result  "${tmp}/deps.txt" ' => system$'    "${tmp}/system.txt")
num_both=$(prep_result    "${tmp}/deps.txt" ' => both$'      "${tmp}/both.txt")
num_neither=$(prep_result "${tmp}/deps.txt" ' => not found$' "${tmp}/neither.txt")

# Any libraries included in package that don't appear in 'deps.txt' **might**
# not actually be needed. Careful: they might be needed by a plugin!
num_extra=$(<"${tmp}/libs.txt" xargs -n1 -I '%%%' sh -c \
    "grep -q '%%%' \"${tmp}/deps.txt\" || echo '%%%'" \
    | sort -f | tee "${tmp}/extra.txt" | wc -l)

less <<EOF
# Package: ${version}
# System: $(uname -srmo)
$(cat /etc/*release*)

# In package only: ${num_package}
$(cat "${tmp}/package.txt")

# System only: ${num_system}
$(cat "${tmp}/system.txt")

# Provided by both: ${num_both}
$(cat "${tmp}/both.txt")

# Provided by neither: ${num_neither}
$(cat "${tmp}/neither.txt")

# Extra: ${num_extra} (In package but unlinked. Possibly needed by plugins.)
$(cat "${tmp}/extra.txt")
EOF
