
message(STATUS "CI build")

# Options
set(BUILD_TYPE "debug" CACHE STRING "Build type")
# Possible values:
# - debug       - debug build

set(BUILD_MODE "dev" CACHE STRING "Build mode")
# Possible values:
# - dev         - dev build

set(BUILD_CONFIGURATION "app" CACHE STRING "Build configuration")
# Possible values:
# - app         - app

set(SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR} CACHE STRING "Source dir")
set(INSTALL_DIR "../build.install" CACHE STRING "Build install dir")
set(INSTALL_SUFFIX "" CACHE STRING "Install suffix")
set(BUILD_NUMBER "12345678" CACHE STRING "Build number")
set(BUILD_REVISION "" CACHE STRING "Build revision")
set(BUILD_USE_UNITY "" CACHE STRING "Build use unity")
set(BUILD_ENABLE_CODE_COVERAGE "" CACHE STRING "Build with code coverage")

option(SKIP_RPATH "Skip rpath" OFF)

# CPUS
cmake_host_system_information(RESULT CPUS QUERY NUMBER_OF_LOGICAL_CORES)
if(NOT "${CPUS}" GREATER "0")
    include(ProcessorCount)
    ProcessorCount(CPUS)
endif()


# AU4_RUN_LRELEASE=${AU4_RUN_LRELEASE:-"ON"}
# AU4_CRASHREPORT_URL=${AU4_CRASHREPORT_URL:-""}
# AU4_BUILD_CRASHPAD_CLIENT=${AU4_BUILD_CRASHPAD_CLIENT:-"ON"}
# AU4_DEBUGLEVEL_ENABLED="OFF"
# AU4_VST3_SDK_PATH=${AU4_VST3_SDK_PATH:-""}
# AU4_BUILD_UNIT_TESTS=${AU4_BUILD_UNIT_TESTS:-"OFF"}
# AU4_NO_RPATH=${AU4_NO_RPATH:-"OFF"}
# AU4_MODULE_UPDATE=${AU4_MODULE_UPDATEE:-"ON"}
# AU4_BUILD_VST_MODULE=${AU4_BUILD_VST_MODULE:-"OFF"}

string(TOUPPER ${BUILD_TYPE} BUILD_TYPE)

message(STATUS "CPUS=${CPUS}")
message(STATUS "BUILD_TYPE=${BUILD_TYPE}")
message(STATUS "BUILD_MODE=${BUILD_MODE}")
message(STATUS "BUILD_CONFIGURATION=${BUILD_CONFIGURATION}")
message(STATUS "INSTALL_DIR=${INSTALL_DIR}")
message(STATUS "INSTALL_SUFFIX=${INSTALL_SUFFIX}")
message(STATUS "BUILD_NUMBER=${BUILD_NUMBER}")
message(STATUS "BUILD_REVISION=${BUILD_REVISION}")
message(STATUS "BUILD_USE_UNITY=${BUILD_USE_UNITY}")
message(STATUS "BUILD_ENABLE_CODE_COVERAGE=${BUILD_ENABLE_CODE_COVERAGE}")

macro(do_build build_type build_dir)

    set(CONFIGURE_ARGS -GNinja
        -DAU4_BUILD_MODE=${BUILD_MODE}
        -DAU4_BUILD_CONFIGURATION=${BUILD_CONFIGURATION}
        -DCMAKE_INSTALL_PREFIX=${INSTALL_DIR}
        -DMUSE_APP_INSTALL_SUFFIX=${INSTALL_SUFFIX}
        -DCMAKE_BUILD_NUMBER=${BUILD_NUMBER}
        -DAU4_REVISION=${BUILD_REVISION}
        -DMUE_COMPILE_USE_UNITY=${BUILD_USE_UNITY}
        -DCMAKE_SKIP_RPATH=${SKIP_RPATH}
        -DMUSE_ENABLE_UNIT_TESTS_CODE_COVERAGE=${BUILD_ENABLE_CODE_COVERAGE}
    )

    message(STATUS "========= Begin configure =========")
    file(MAKE_DIRECTORY ${build_dir})

    execute_process(
        COMMAND cmake ${SOURCE_DIR} ${CONFIGURE_ARGS} -DCMAKE_BUILD_TYPE=${build_type}
        WORKING_DIRECTORY ${build_dir}
        RESULT_VARIABLE CMAKE_RESULT
    )
    if (CMAKE_RESULT GREATER 0)
        message(FATAL_ERROR "========= Failed configure =========")
    else()
        message(STATUS "========= Success configure =========")
    endif()

    message(STATUS "========= Begin build =========")
    execute_process(
        COMMAND ninja -j ${CPUS}
        WORKING_DIRECTORY ${build_dir}
        RESULT_VARIABLE NINJA_RESULT
    )
    if (NINJA_RESULT GREATER 0)
        message(FATAL_ERROR "========= Failed build =========")
    else()
        message(STATUS "========= Success build =========")
    endif()


    # cmake .. -GNinja \
    #     -DCMAKE_BUILD_TYPE="${build_type}" \

    #     -DMUE_RUN_LRELEASE="${AU4_RUN_LRELEASE}" \
    #     -DMUE_BUILD_VIDEOEXPORT_MODULE="${AU4_BUILD_VIDEOEXPORT_MODULE}" \
    #     -DMUSE_MODULE_UPDATE="${AU4_MODULE_UPDATE}" \
    #     -DMUSE_ENABLE_UNIT_TESTS="${AU4_BUILD_UNIT_TESTS}" \
    #     -DMUSE_MODULE_DIAGNOSTICS_CRASHPAD_CLIENT="${AU4_BUILD_CRASHPAD_CLIENT}" \
    #     -DMUSE_MODULE_DIAGNOSTICS_CRASHREPORT_URL="${AU4_CRASHREPORT_URL}" \
    #     -DMUSE_MODULE_GLOBAL_LOGGER_DEBUGLEVEL="${AU4_DEBUGLEVEL_ENABLED}" \
    #     -DMUSE_MODULE_VST="${AU4_BUILD_VST_MODULE}" \
    #     -DMUSE_MODULE_VST_VST3_SDK_PATH="${AU4_VST3_SDK_PATH}" \
    #     -DCMAKE_SKIP_RPATH="${AU4_NO_RPATH}" \


    # ninja -j $JOBS

endmacro()

macro(do_install build_dir)
    message(STATUS "========= Begin install =========")
    execute_process(
        COMMAND cmake --install ${build_dir}
        RESULT_VARIABLE INSTALL_RESULT
    )
    if (INSTALL_RESULT GREATER 0)
        message(FATAL_ERROR "========= Failed install =========")
    else()
        message(STATUS "========= Success install =========")
    endif()

endmacro()

# Configure and build
if(BUILD_TYPE STREQUAL "DEBUG")

    do_build(Debug build.debug)

elseif(BUILD_TYPE STREQUAL "DEBUG_INSTALL")

    set(BUILD_DIR build.debug)
    do_build(Debug ${BUILD_DIR})
    do_install(${BUILD_DIR})

elseif(BUILD_TYPE STREQUAL "RELEASE_INSTALL")

    set(BUILD_DIR build.release)
    do_build(RelWithDebInfo ${BUILD_DIR})
    do_install(${BUILD_DIR})

elseif(BUILD_TYPE STREQUAL "APPIMAGE")

    set(INSTALL_SUFFIX "4portable${INSTALL_SUFFIX}") # e.g. "4portable" or "4portablenightly"
    set(SKIP_RPATH ON)
    set(BUILD_DIR build.release)

    do_build(Release ${BUILD_DIR})
    do_install(${BUILD_DIR})

    file (STRINGS "${BUILD_DIR}/PREFIX.txt" INSTALL_DIR)

    execute_process (
        COMMAND ln -sf . usr
        WORKING_DIRECTORY ${INSTALL_DIR}
    )

    file(COPY
        ${BUILD_DIR}/install_manifest.txt
        DESTINATION ${INSTALL_DIR}
    )

    file(COPY
        ${BUILD_DIR}/org.musescore.MuseScore${INSTALL_SUFFIX}.desktop
        DESTINATION ${INSTALL_DIR}
    )

    file(COPY
        ${CMAKE_CURRENT_LIST_DIR}/buildscripts/packaging/Linux+BSD/audacity.svg
        DESTINATION ${INSTALL_DIR}
    )

    # mscore="mscore${MUSE_APP_INSTALL_SUFFIX}"
    # desktop="org.musescore.MuseScore${MUSE_APP_INSTALL_SUFFIX}.desktop"
    # icon="${mscore}.png"
    # mani="install_manifest.txt"
    # cp "share/applications/${desktop}" "${desktop}"
    # cp "share/icons/hicolor/128x128/apps/${icon}" "${icon}"
    # <"$build_dir/${mani}" >"${mani}" sed -rn 's/.*(share\/)(applications|icons|man|metainfo|mime)(.*)/\1\2\3/p'
    # ;;


endif()
