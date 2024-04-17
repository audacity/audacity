
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

if (NOT BUILD_REVISION)
    execute_process(
        COMMAND git rev-parse --short=7 HEAD
        WORKING_DIRECTORY ${SOURCE_DIR}
        OUTPUT_VARIABLE BUILD_REVISION
    )
    string(STRIP ${BUILD_REVISION} BUILD_REVISION)
endif()

message(STATUS "CPUS=${CPUS}")
message(STATUS "BUILD_TYPE=${BUILD_TYPE}")
message(STATUS "BUILD_MODE=${BUILD_MODE}")
message(STATUS "BUILD_CONFIGURATION=${BUILD_CONFIGURATION}")
message(STATUS "INSTALL_DIR=${INSTALL_DIR}")
message(STATUS "INSTALL_SUFFIX=${INSTALL_SUFFIX}")
message(STATUS "BUILD_NUMBER=${BUILD_NUMBER}")
message(STATUS "BUILD_REVISION=${BUILD_REVISION}")

macro(do_build build_type build_dir)

    set(CONFIGURE_ARGS -GNinja
        -DAU4_BUILD_MODE=${BUILD_MODE}
        -DAU4_BUILD_CONFIGURATION=${BUILD_CONFIGURATION}
        -DCMAKE_INSTALL_PREFIX=${INSTALL_DIR}
        -DAU4_INSTALL_SUFFIX=${INSTALL_SUFFIX}
        -DCMAKE_BUILD_NUMBER=${BUILD_NUMBER}
        -DAU4_REVISION=${BUILD_REVISION}
    )

    message(STATUS "========= Begin configure =========")
    file(MAKE_DIRECTORY ${build_dir})

    execute_process(
        COMMAND cmake ${SOURCE_DIR} ${CONFIGURE_ARGS} -DCMAKE_BUILD_TYPE=${build_type}
        WORKING_DIRECTORY ${build_dir}
        RESULT_VARIABLE CMAKE_RESULT
    )
    message(STATUS "========= End configure: ${CMAKE_RESULT}")

    message(STATUS "========= Begin build =========")
    execute_process(
        COMMAND ninja -j ${CPUS}
        WORKING_DIRECTORY ${build_dir}
        RESULT_VARIABLE NINJA_RESULT
    )
    message(STATUS "========= End build: ${NINJA_RESULT}")

    message(STATUS "========= Begin install =========")
    execute_process(
        COMMAND ninja install
        WORKING_DIRECTORY ${build_dir}
        RESULT_VARIABLE NINJA_RESULT
    )
    message(STATUS "========= End install: ${NINJA_RESULT}")


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

# Configure and build
if(BUILD_TYPE STREQUAL "DEBUG")
    do_build(Debug build.debug)
endif()
