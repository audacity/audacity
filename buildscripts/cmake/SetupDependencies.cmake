

message(STATUS "Setup dependencies")

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

set(LIB_ARCH ${ARCH})

if (BUILD_IS_RELEASE)
    set(LIB_BUILD_TYPE "release")
else()
    set(LIB_BUILD_TYPE "debug")
endif()

set(REMOTE_ROOT_URL https://raw.githubusercontent.com/musescore/muse_deps/main)
set(LOCAL_ROOT_PATH ${FETCHCONTENT_BASE_DIR})

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
    cmake_language(CALL ${name}_Populate ${remote_url} ${local_path} ${LIB_OS} ${LIB_ARCH} ${LIB_BUILD_TYPE})

    get_property(include_dirs GLOBAL PROPERTY ${name}_INCLUDE_DIRS)
    get_property(libraries GLOBAL PROPERTY ${name}_LIBRARIES)
    get_property(instal_libraries GLOBAL PROPERTY ${name}_INSTALL_LIBRARIES)

    set(${name}_INCLUDE_DIRS ${include_dirs} PARENT_SCOPE)
    set(${name}_LIBRARIES ${libraries} PARENT_SCOPE)
    set(${name}_INSTALL_LIBRARIES ${instal_libraries} PARENT_SCOPE)

    if (OS_IS_MAC)
        install(FILES ${instal_libraries} DESTINATION "audacity.app/Contents/Frameworks")
    elseif(OS_IS_WIN)
        install(FILES ${instal_libraries} TYPE BIN)
    else()
        install(FILES ${instal_libraries} TYPE LIB)
    endif()

endfunction()

populate(wxwidgets "wxwidgets/3.1.3.6")
populate(expat "expat/2.0.5")
populate(portaudio "portaudio/19.7.0")

if (NOT OS_IS_LIN)
    populate(zlib "zlib/1.2.13")
    populate(libjpeg-turbo "libjpeg-turbo/2.1.5")
    populate(libpng "libpng/1.6.39")
endif()

