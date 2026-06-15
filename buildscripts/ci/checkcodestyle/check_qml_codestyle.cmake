if(NOT DEFINED CMAKE_SCRIPT_MODE_FILE)
    message(FATAL_ERROR "This file is a script")
endif()

set(SCAN_DIR ${CMAKE_ARGV3})

if(NOT SCAN_DIR)
    message(FATAL_ERROR "Usage: cmake -P check_qml_codestyle.cmake <scan_dir>")
endif()

# Use the qmlformat bundled with Qt (>= 6.10, which carries the formatting
# fixes we rely on) rather than a separately downloaded prebuilt binary.
# It is found via PATH or the Qt install pointed to by QT_ROOT_DIR.
find_program(QMLFORMAT_BIN
    NAMES qmlformat
    HINTS ENV QT_ROOT_DIR
    PATH_SUFFIXES bin
)

if(NOT QMLFORMAT_BIN)
    message(FATAL_ERROR
        "qmlformat not found. Install Qt 6.10+ and ensure its bin/ is on PATH.")
endif()

execute_process(
    COMMAND ${QMLFORMAT_BIN} --version
    OUTPUT_VARIABLE QMLFORMAT_VERSION
    OUTPUT_STRIP_TRAILING_WHITESPACE
)
message(STATUS "Using ${QMLFORMAT_BIN} (${QMLFORMAT_VERSION})")

file(GLOB_RECURSE QML_FILES ${SCAN_DIR}/*.qml)
list(LENGTH QML_FILES QML_FILES_COUNT)
message(STATUS "Checking QML code style of ${QML_FILES_COUNT} files in ${SCAN_DIR}")

# qmlformat reads the file list (one path per line) so we stay well clear of
# command-line length limits on large trees.
set(FILES_LIST ${CMAKE_CURRENT_LIST_DIR}/qml_files_list.txt)
string(REPLACE ";" "\n" QML_FILES_LINES "${QML_FILES}")
file(WRITE ${FILES_LIST} "${QML_FILES_LINES}\n")

execute_process(
    COMMAND ${QMLFORMAT_BIN} -F ${FILES_LIST} --semicolon-rule=essential -l unix
    RESULT_VARIABLE QMLFORMAT_RESULT
)

file(REMOVE ${FILES_LIST})

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

        $ qmlformat -i --semicolon-rule=essential -l unix <file.qml>

        The required changes are...
    ")

    message(${MSG})

    execute_process(
        COMMAND git diff -- *.qml
        OUTPUT_VARIABLE GIT_DIFF_OUT
    )

    message(FATAL_ERROR "${GIT_DIFF_OUT}")
endif()
