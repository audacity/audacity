if(NOT DEFINED CMAKE_SCRIPT_MODE_FILE)
    message(FATAL_ERROR "This file is a script")
endif()

set(SCAN_DIR ${CMAKE_ARGV3})

if(NOT SCAN_DIR)
    message(FATAL_ERROR "Usage: cmake -P check_qml_codestyle.cmake <scan_dir>")
endif()

if(CMAKE_HOST_SYSTEM_NAME STREQUAL "Windows")
    set(LIB_OS "windows")
    set(EXE_SUFFIX ".exe")
elseif(CMAKE_HOST_SYSTEM_NAME STREQUAL "Darwin")
    set(LIB_OS "macos")
    set(EXE_SUFFIX "")
else()
    set(LIB_OS "linux")
    set(EXE_SUFFIX "")
endif()

# Same qmlformat package as SetupDevEnvironment.cmake
set(REMOTE_URL https://raw.githubusercontent.com/musescore/muse_deps/main/qmlformat/6.10)
set(LOCAL_PATH ${CMAKE_CURRENT_LIST_DIR}/../../../.tools/qmlformat)

if(NOT EXISTS ${LOCAL_PATH}/qmlformat.cmake)
    file(MAKE_DIRECTORY ${LOCAL_PATH})
    file(DOWNLOAD ${REMOTE_URL}/qmlformat.cmake ${LOCAL_PATH}/qmlformat.cmake
        HTTPHEADER "Cache-Control: no-cache"
    )
endif()

include(${LOCAL_PATH}/qmlformat.cmake)
qmlformat_Populate(${REMOTE_URL} ${LOCAL_PATH} ${LIB_OS})

set(QMLFORMAT_BIN ${LOCAL_PATH}/bin/qmlformat${EXE_SUFFIX})
if(NOT EXISTS ${QMLFORMAT_BIN})
    message(FATAL_ERROR "qmlformat executable not found: ${QMLFORMAT_BIN}")
endif()

file(GLOB_RECURSE QML_FILES ${SCAN_DIR}/*.qml)
list(LENGTH QML_FILES QML_FILES_COUNT)
message(STATUS "Checking QML code style of ${QML_FILES_COUNT} files in ${SCAN_DIR}")

set(FILES_LIST ${LOCAL_PATH}/qml_files_list.txt)
string(REPLACE ";" "\n" QML_FILES_LINES "${QML_FILES}")
file(WRITE ${FILES_LIST} "${QML_FILES_LINES}\n")

execute_process(
    COMMAND ${QMLFORMAT_BIN} -F ${FILES_LIST} --semicolon-rule=essential -l unix
    RESULT_VARIABLE QMLFORMAT_RESULT
)

if(NOT QMLFORMAT_RESULT EQUAL 0)
    message(FATAL_ERROR "qmlformat failed, please check log for details")
endif()

execute_process(
    COMMAND git diff --name-only -- *.qml
    OUTPUT_VARIABLE GIT_DIFF_OUT
)

if(GIT_DIFF_OUT)

    set(MSG
        "Error: QML code style is incorrect in these files:

        ${GIT_DIFF_OUT}

        Please run qmlformat on these files and then amend your commit:

        $ cmake -P buildscripts/ci/checkcodestyle/check_qml_codestyle.cmake ./src/
        $ git add -u
        $ git commit --amend --no-edit

        Or format a single file:

        $ .tools/qmlformat/bin/qmlformat -i --semicolon-rule=essential -l unix <file.qml>

        The required changes are...
    ")

    message(${MSG})

    execute_process(
        COMMAND git diff -- *.qml
        OUTPUT_VARIABLE GIT_DIFF_OUT
    )

    message(FATAL_ERROR "${GIT_DIFF_OUT}")
endif()
