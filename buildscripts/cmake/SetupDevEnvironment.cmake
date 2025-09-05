message(STATUS "Setup development environment")

# Config
include(GetPlatformInfo)
include(GetBuildType)

set(LIB_OS )
if (OS_IS_WIN)
    set(LIB_OS "windows")
elseif(OS_IS_LIN)
    set(LIB_OS "linux")
elseif(OS_IS_FBSD)
    set(LIB_OS "linux")
elseif(OS_IS_MAC)
    set(LIB_OS "macos")
endif()

set(REMOTE_ROOT_URL https://raw.githubusercontent.com/musescore/muse_deps/main)
set(LOCAL_ROOT_PATH ${CMAKE_SOURCE_DIR}/.tools/)

function(populate name remote_suffix)
    set(remote_url ${REMOTE_ROOT_URL}/${remote_suffix})
    set(local_path ${LOCAL_ROOT_PATH}/${name})

    if (NOT EXISTS ${local_path}/${name}.cmake)
        file(MAKE_DIRECTORY ${local_path})
        file(DOWNLOAD ${remote_url}/${name}.cmake ${local_path}/${name}.cmake
            HTTPHEADER "Cache-Control: no-cache"
        )
    endif()

    include(${local_path}/${name}.cmake)

    # func from ${name}.cmake)
    cmake_language(CALL ${name}_Populate ${remote_url} ${local_path} ${LIB_OS})
endfunction()

populate(qmlformat "qmlformat/6.10")

configure_file(
    ${CMAKE_SOURCE_DIR}/muse_framework/buildscripts/ci/checkcodestyle/uncrustify_muse.cfg
    ${CMAKE_SOURCE_DIR}/uncrustify.cfg
    COPYONLY
)
