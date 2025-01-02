#!/bin/bash

set -e

SRC_DIR="src"
UNTIDY_FILE=".untidy"
FILE_EXTENSIONS=("cpp" "c" "cc" "hpp" "h")

while [[ $# -gt 0 ]]; do
    case $1 in
        --src-dir)
            SRC_DIR="$2"
            shift 2
            ;;
        --untidy-file)
            UNTIDY_FILE="$2"
            shift 2
            ;;
        --extensions)
            IFS="," read -r -a FILE_EXTENSIONS <<< "$2"
            shift 2
            ;;
        *)
            echo "Unknown argument: $1"
            exit 1
            ;;
    esac
done

echo "Checking formatting in: ${SRC_DIR}"
echo "Skipping directories containing: ${UNTIDY_FILE}"
echo "Checking files with extensions: ${FILE_EXTENSIONS[*]}"

gather_files() {
    local dir="$1"

    local exts=$(printf " -name *.%s -o" "${FILE_EXTENSIONS[@]}")
    exts="${exts% -o}" # remove trailing -o

    find "${dir}" -type d -exec test -e "{}/${UNTIDY_FILE}" \; -prune -o -type f \( ${exts} \) -print
}

check_format() {
    local file="$1"
    if ! clang-format --dry-run --Werror "${file}" ; then
        return 1
    fi
    return 0
}

FILES_TO_CHECK=$(gather_files "${SRC_DIR}")

STATUS=0
for file in ${FILES_TO_CHECK}; do
    if ! check_format "${file}"; then
        STATUS=1
    fi
done

if [[ ${STATUS} -ne 0 ]]; then
    echo "Formatting check failed. Please format your code with clang-format."
    exit 1
else
    echo "All files are formatted correctly."
    exit 0
fi
