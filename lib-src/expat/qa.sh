#! /bin/bash
# Copyright (C) 2016 Sebastian Pipping <sebastian@pipping.org>
# Licensed under MIT license

set -o nounset

: ${GCC_CC:=gcc}
: ${GCC_CXX:=g++}
: ${CLANG_CC:=clang}
: ${CLANG_CXX:=clang++}

: ${AR:=ar}
: ${CC:="${CLANG_CC}"}
: ${CXX:="${CLANG_CXX}"}
: ${LD:=ld}
: ${MAKE:=make}

: ${BASE_FLAGS:="-pipe -Wall -Wextra -pedantic -Wno-overlength-strings"}

ANNOUNCE() {
    local open='\e[1m'
    local close='\e[0m'

    echo -e -n "${open}"
    echo -n "# $*"
    echo -e "${close}"
}

RUN() {
    ANNOUNCE "$@"
    env "$@"
}

main() {
    local mode="${1:-}"
    shift

    local RUNENV
    local BASE_FLAGS="${BASE_FLAGS}"

    case "${mode}" in
    address)
        # http://clang.llvm.org/docs/AddressSanitizer.html
        local CC="${GCC_CC}"
        local CXX="${GCC_CXX}"
        BASE_FLAGS+=" -g -fsanitize=address -fno-omit-frame-pointer"
        ;;
    coverage | lib-coverage | app-coverage)
        local CC="${GCC_CC}"
        local CXX="${GCC_CXX}"
        BASE_FLAGS+=" --coverage --no-inline"
        ;;
    egypt)
        BASE_FLAGS+=" -fdump-rtl-expand"
        ;;
    memory)
        # http://clang.llvm.org/docs/MemorySanitizer.html
        BASE_FLAGS+=" -fsanitize=memory -fno-omit-frame-pointer -g -O2 -fsanitize-memory-track-origins -fsanitize-blacklist=memory-sanitizer-blacklist.txt"
        ;;
    ncc)
        # http://students.ceid.upatras.gr/~sxanth/ncc/
        local CC="ncc -ncgcc -ncld -ncfabs"
        local AR=nccar
        local LD=nccld
        BASE_FLAGS+=" -fPIC"
        ;;
    undefined)
        # http://clang.llvm.org/docs/UndefinedBehaviorSanitizer.html
        BASE_FLAGS+=" -fsanitize=undefined"
        export UBSAN_OPTIONS=print_stacktrace=1
        ;;
    *)
        echo "Usage:" 1>&2
        echo "  ${0##*/} (address|coverage|lib-coverage|app-coverage|egypt|memory|ncc|undefined)" 1>&2
        exit 1
        ;;
    esac

    local CFLAGS="-std=c89 ${BASE_FLAGS} ${CFLAGS:-}"
    local CXXFLAGS="-std=c++98 ${BASE_FLAGS} ${CXXFLAGS:-}"

    (
        set -e

        RUN CC="${CC}" CFLAGS="${CFLAGS}" \
                CXX="${CXX}" CXXFLAGS="${CXXFLAGS}" \
                AR="${AR}" \
                LD="${LD}" \
                ./configure "$@"

        RUN "${MAKE}" clean all

        case "${mode}" in
        egypt|ncc)
            ;;
        *)
            RUN "${MAKE}" check run-xmltest
            ;;
        esac
    )
    [[ $? -ne 0 ]] && exit 1

    case "${mode}" in
    coverage)
        find -name '*.gcda' | sort | xargs gcov
        ;;
    lib-coverage)
        find lib -name '*.gcda' | sort | xargs gcov
        ;;
    app-coverage)
        find lib xmlwf -name '*.gcda' | sort | xargs gcov
        ;;
    egypt)
        local DOT_FORMAT="${DOT_FORMAT:-svg}"
        local o="callgraph.${DOT_FORMAT}"
        ANNOUNCE "egypt ...... | dot ...... > ${o}"
        find -name '*.expand' \
                | sort \
                | xargs -r egypt \
                | unflatten -c 20 \
                | dot -T${DOT_FORMAT} -Grankdir=LR \
                > "${o}"
        ;;
    ncc)
        RUN nccnav ./.libs/libexpat.a.nccout
        ;;
    esac
}

main "$@"
