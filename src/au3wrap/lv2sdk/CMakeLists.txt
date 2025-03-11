#
# Audacity: A Digital Audio Editor
#
declare_module(lv2sdk)

message(STATUS "AU_MODULE_LV2_LV2SDK_PATH: ${AU_MODULE_LV2_LV2SDK_PATH}")

if(AU_MODULE_LV2_LV2SDK_PATH)
    set(LV2SDK_PATH ${AU_MODULE_LV2_LV2SDK_PATH})
else()

    # If not set MUSE_MODULE_VST_LV2SDK_PATH
    # download lv2sdk source

    set(REMOTE_ROOT_URL https://raw.githubusercontent.com/musescore/muse_deps/main)
    set(remote_url "${REMOTE_ROOT_URL}/lv2sdk/lv2sdk_0.24.26")
    set(local_path ${PROJECT_BINARY_DIR}/_deps/lv2sdk)
    if (NOT EXISTS ${local_path}/lv2sdk.cmake)
        file(MAKE_DIRECTORY ${local_path})
        file(DOWNLOAD ${remote_url}/lv2sdk.cmake ${local_path}/lv2sdk.cmake
            HTTPHEADER "Cache-Control: no-cache"
        )
    endif()

    include(${local_path}/lv2sdk.cmake)

    # func from ${name}.cmake)
    cmake_language(CALL lv2sdk_Populate ${remote_url} ${local_path} "source" "" "")

    set(LV2SDK_PATH ${local_path})

endif()

include(GetPlatformInfo)

# lv2
set(LV2_PATH ${LV2SDK_PATH}/lv2)

# lilv
set(LILV_PATH ${LV2SDK_PATH}/lilv)
set(LILV_SRC
    ${LILV_PATH}/src/collections.c
    ${LILV_PATH}/src/dylib.c
    ${LILV_PATH}/src/dylib.h
    ${LILV_PATH}/src/instance.c
    ${LILV_PATH}/src/lib.c
    ${LILV_PATH}/src/lilv_config.h
    ${LILV_PATH}/src/lilv_internal.h
    ${LILV_PATH}/src/node.c
    ${LILV_PATH}/src/plugin.c
    ${LILV_PATH}/src/pluginclass.c
    ${LILV_PATH}/src/port.c
    ${LILV_PATH}/src/query.c
    ${LILV_PATH}/src/scalepoint.c
    ${LILV_PATH}/src/state.c
    ${LILV_PATH}/src/ui.c
    ${LILV_PATH}/src/util.c
    ${LILV_PATH}/src/world.c
    )

# serd
set(SERD_PATH ${LV2SDK_PATH}/serd)
set(SERD_SRC
    ${SERD_PATH}/src/attributes.h
    ${SERD_PATH}/src/base64.c
    ${SERD_PATH}/src/base64.h
    ${SERD_PATH}/src/byte_sink.h
    ${SERD_PATH}/src/byte_source.c
    ${SERD_PATH}/src/byte_source.h
    ${SERD_PATH}/src/env.c
    ${SERD_PATH}/src/n3.c
    ${SERD_PATH}/src/node.c
    ${SERD_PATH}/src/node.h
    ${SERD_PATH}/src/reader.c
    ${SERD_PATH}/src/reader.h
    ${SERD_PATH}/src/serd_config.h
    ${SERD_PATH}/src/serdi.c
    ${SERD_PATH}/src/serd_internal.h
    ${SERD_PATH}/src/stack.h
    ${SERD_PATH}/src/string.c
    ${SERD_PATH}/src/string_utils.h
    ${SERD_PATH}/src/system.c
    ${SERD_PATH}/src/system.h
    ${SERD_PATH}/src/try.h
    ${SERD_PATH}/src/uri.c
    ${SERD_PATH}/src/uri_utils.h
    ${SERD_PATH}/src/warnings.h
    ${SERD_PATH}/src/writer.c
)

# sord
set(SORD_PATH ${LV2SDK_PATH}/sord)
set(SORD_SRC
    ${SORD_PATH}/src/sord.c
    ${SORD_PATH}/src/sord_config.h
    ${SORD_PATH}/src/sordi.c
    ${SORD_PATH}/src/sord_internal.h
    ${SORD_PATH}/src/sordmm_test.cpp
    ${SORD_PATH}/src/sord_validate.c
    ${SORD_PATH}/src/syntax.c
)

# zix
set(ZIX_PATH ${LV2SDK_PATH}/zix)
set(ZIX_SRC
    ${ZIX_PATH}/src/allocator.c
    ${ZIX_PATH}/src/btree.c
    ${ZIX_PATH}/src/bump_allocator.c
    ${ZIX_PATH}/src/digest.c
    ${ZIX_PATH}/src/errno_status.c
    ${ZIX_PATH}/src/errno_status.h
    ${ZIX_PATH}/src/filesystem.c
    ${ZIX_PATH}/src/hash.c
    ${ZIX_PATH}/src/index_range.h
    ${ZIX_PATH}/src/path.c
    ${ZIX_PATH}/src/path_iter.h
    ${ZIX_PATH}/src/ring.c
    ${ZIX_PATH}/src/status.c
    ${ZIX_PATH}/src/string_view.c
    ${ZIX_PATH}/src/system.c
    ${ZIX_PATH}/src/system.h
    ${ZIX_PATH}/src/tree.c
    ${ZIX_PATH}/src/zix_config.h
)

if (OS_IS_WIN)
    set(ZIX_SRC ${ZIX_SRC}
        ${ZIX_PATH}/src/win32/environment_win32.c
        ${ZIX_PATH}/src/win32/filesystem_win32.c
        ${ZIX_PATH}/src/win32/sem_win32.c
        ${ZIX_PATH}/src/win32/system_win32.c
        ${ZIX_PATH}/src/win32/thread_win32.c
        ${ZIX_PATH}/src/win32/win32_util.c
        ${ZIX_PATH}/src/win32/win32_util.h
        )
elseif(OS_IS_LIN)
    set(ZIX_SRC ${ZIX_SRC}
        ${ZIX_PATH}/src/posix/environment_posix.c
        ${ZIX_PATH}/src/posix/filesystem_posix.c
        ${ZIX_PATH}/src/posix/sem_posix.c
        ${ZIX_PATH}/src/posix/system_posix.c
        ${ZIX_PATH}/src/posix/thread_posix.c
        )
elseif(OS_IS_MAC)
    set(ZIX_SRC ${ZIX_SRC}
        ${ZIX_PATH}/src/darwin/sem_darwin.c
        )
endif()

# sratom
set(SRATOM_PATH ${LV2SDK_PATH}/sratom)
set(SRATOM_SRC
    ${SRATOM_PATH}/src/sratom.c
)

set(MODULE_SRC
    ${LILV_SRC}
    ${SERD_SRC}
    ${SORD_SRC}
    ${ZIX_SRC}
    ${SRATOM_SRC}
    )

set(MODULE_INCLUDE
    ${LV2_PATH}/include
    ${LILV_PATH}/include
    ${SERD_PATH}/include
    ${SORD_PATH}/include
    ${ZIX_PATH}/include
    ${SRATOM_PATH}/include
    )

set(MODULE_DEF
    -D_GNU_SOURCE # for /zix/src/posix/filesystem_posix.c
)

set(MODULE_USE_PCH OFF)
set(MODULE_USE_UNITY OFF)

setup_module()

target_no_warning(${MODULE} -w)
