# This file contains generic functions that are useful for any CMake project.
# Function names start with "fn__" so you can recognise them in other files.
#
# Unless otherwise stated, all relative file paths are assumed to be relative
# to ${CMAKE_CURRENT_BINARY_DIR}. If your file is elsewhere then you must use
# an absolute path, e.g. by prepending ${CMAKE_CURRENT_SOURCE_DIR}.
#
# Some of the functions create custom commands. Remember, a custom command
# will not run unless its OUTPUT is depended on by a custom target or another
# custom command. The same applies to the other custom command, so there must
# be a custom target involved somewhere.
#
# For example, if you write this in a CMake file then nothing will happen:
#
#     fn__copy_during_build("my_source_file.txt" "my_dest_file.txt")
#
# One way to make sure the file is copied would be to follow that line with:
#
#     add_custom_target("my_target_name" ALL DEPENDS "my_dest_file.txt")

# Where are we? Need to set these variables for use inside functions.
set(_FUNCTIONS_FILE "${CMAKE_CURRENT_LIST_FILE}") # path to this file
set(_FUNCTIONS_DIR "${CMAKE_CURRENT_LIST_DIR}") # path to this directory

# Set a variable to a value only if the variable is currently undefined.
function(fn__set_default
  NAMEV # Name of the variable to set.
  VALUE # Value to assign to the variable.
)
  if(NOT DEFINED "${NAMEV}")
    set("${NAMEV}" "${VALUE}" PARENT_SCOPE)
  endif()
endfunction()

# Set a variable to value of an option from a list of command line arguments.
# The variable will only receive a value if the relevant option was provided.
function(fn__get_option
  VAR_NAME # Name of the variable to set.
  OPT_NAME # Name of the option to search for (e.g. "-foo").
  # ARGN...  The command line arguments.
)
  list(LENGTH ARGN LEN)
  set(i "0")
  while(i LESS "${LEN}")
    list(GET ARGN "${i}" ARG)
    if(ARG MATCHES "^${OPT_NAME}")
      if(ARG STREQUAL "${OPT_NAME}")
        # Option and value in separate arguments ("-o VALUE" or "--option VALUE")
        math(EXPR i "${i} + 1") # next argument (the value)
        list(GET ARGN "${i}" OPT_VAL)
        set("${VAR_NAME}" "${OPT_VAL}" PARENT_SCOPE)
      elseif("${OPT_NAME}" MATCHES "^-[a-zA-Z0-9]$")
        # Short option without equals sign before value ("-oVALUE")
        string(REGEX REPLACE "^${OPT_NAME}(.*)$" "\\1" OPT_VAL "${ARG}")
        set("${VAR_NAME}" "${OPT_VAL}" PARENT_SCOPE)
      elseif(ARG MATCHES "^${OPT_NAME}=")
        # Long option with equals sign before option value ("--option=VALUE")
        string(REGEX REPLACE "^${OPT_NAME}=(.*)$" "\\1" OPT_VAL "${ARG}")
        set("${VAR_NAME}" "${OPT_VAL}" PARENT_SCOPE)
      endif()
    endif()
    math(EXPR i "${i} + 1") # next argument
  endwhile()
endfunction()

# Turn a list of command line arguments into an escaped string
function(fn__command_string
  VAR_NAME # The variable to store the final string.
  # ARGN ... The command line arguments.
)
  foreach(ARG IN LISTS ARGN)
    if(ARG MATCHES " ")
        set(ARG "\"${ARG}\"")
    endif()
    set("${VAR_NAME}" "${${VAR_NAME}} ${ARG}")
  endforeach()
  set("${VAR_NAME}" "${${VAR_NAME}}" PARENT_SCOPE)
endfunction()

function(fn__require_program # Ensure that a build dependency is installed.
  PATHV # Location of the program returned in this variable.
  PRETTY_NAME # Program name that users will recognize (e.g. "ImageMagick").
  TEST_ARG # Argument to run if program is found (e.g. "--version").
  HELP_URL # Where to get help if program is not found.
  COMMAND # The name of the program on the command line (e.g. "magick").
  # ARGN: Any alternative names for the command (e.g. "convert", "mogrify").
)
  # Only check for program if not already found and cached.
  if(NOT ${PATHV})
    set(CMD_NAMES ${COMMAND} ${ARGN})
    set(CMAKE_FIND_APPBUNDLE "NEVER") # macOS: don't search for .app bundles
    find_program(${PATHV} NAMES ${CMD_NAMES} DOC "${HELP_URL}")
    set(CMD "${${PATHV}}")
    string(REPLACE ";" ", " CMD_NAMES "${CMD_NAMES}")
    if(CMD)
      # Program found so test to make sure it works.
      execute_process(
        COMMAND "${CMD}" ${TEST_ARG}
        TIMEOUT 5
        RESULT_VARIABLE RETVAL
      )
      if(NOT ${RETVAL} EQUAL 0)
        # Test failed.
        unset(${PATHV} CACHE) # make sure we check again next time
        message(FATAL_ERROR
          "\n"
          "Error running ${PRETTY_NAME} with command:"
          "\n"
          "  ${CMD} ${TEST_ARG}"
          "\n"
          "Console output directly above may provide more information about the error."
          "\n"
          "Do you have the correct version of ${PRETTY_NAME} installed? Is it "
          "available in your PATH environment variable?"
          "\n"
          "See ${HELP_URL}"
          "\n"
        )
      endif(NOT ${RETVAL} EQUAL 0)
    else(CMD)
      # Program not found.
      message(FATAL_ERROR
        "\n"
        "Unable to find ${PRETTY_NAME} (cmd: ${CMD_NAMES})"
        "\n"
        "Please make sure ${PRETTY_NAME} is installed and "
        "available in your PATH environment variable."
        "\n"
        "See ${HELP_URL}"
        "\n"
      )
    endif(CMD)
  endif(NOT ${PATHV})
endfunction(fn__require_program)

# Get information about Qt based on the path to qmake executable
function(fn__set_qt_variables
  QMAKE # Path to qmake executable
)
  get_filename_component(dir "${QMAKE}" DIRECTORY)
  get_filename_component(dir "${dir}" DIRECTORY)
  set(ENV{QTDIR} "${dir}")
  get_filename_component(QT_COMPILER "${dir}" NAME)
  get_filename_component(dir "${dir}" DIRECTORY)
  get_filename_component(QT_VERSION "${dir}" NAME)
  get_filename_component(dir "${dir}" DIRECTORY)
  set(QT_LOCATION "${dir}" PARENT_SCOPE)
  set(QT_VERSION "${QT_VERSION}" PARENT_SCOPE)
  set(QT_COMPILER "${QT_COMPILER}" PARENT_SCOPE)
endfunction()

function(fn__copy_during_build # copy a file at build time
  SOURCE_FILE # relative or absolute path to file being copied
  DEST_FILE # relative or absolute path to the new copy
  )
  # need absolute path for use with add_custom_command's DEPENDS argument
  get_filename_component(
    SOURCE_FILE_ABS # equals SOURCE_FILE if SOURCE_FILE is already absolute
    "${SOURCE_FILE}"
    ABSOLUTE
    BASE_DIR "${CMAKE_CURRENT_BINARY_DIR}"
    )
  add_custom_command(
    OUTPUT "${DEST_FILE}"
    DEPENDS "${SOURCE_FILE_ABS}" # absolute path required for DEPENDS
    COMMAND "${CMAKE_COMMAND}" -E copy_if_different "${SOURCE_FILE}" "${DEST_FILE}"
    COMMENT "Copying '${SOURCE_FILE}' to '${DEST_FILE}'"
    VERBATIM
    )
endfunction(fn__copy_during_build)

function(fn__build_zip # create zip archive at build time
  PATH_OUT # final path to the archive
  DIR_IN # the directory that all input file paths are relative to
  FILE_IN # one file to go in the archive, with path relative to DIR_IN
  # ARGN remaining arguments are more files to go in the archive
  )
  # need absolute path for use with add_custom_command's DEPENDS argument
  get_filename_component(
    DIR_IN_ABS
    "${DIR_IN}"
    ABSOLUTE
    BASE_DIR "${CMAKE_CURRENT_BINARY_DIR}"
    )
  set(FILES_IN "${FILE_IN};${ARGN}")
  set(FILES_IN_ABS "")
  foreach(FILE IN LISTS FILES_IN)
    list(APPEND FILES_IN_ABS "${DIR_IN_ABS}/${FILE}")
  endforeach(FILE)
  # we're about to change directory, so a relative PATH_OUT would not work
  get_filename_component(
    PATH_OUT_ABS
    "${PATH_OUT}"
    ABSOLUTE
    BASE_DIR "${CMAKE_CURRENT_BINARY_DIR}"
    )
  add_custom_command(
    OUTPUT "${PATH_OUT}" # still relative to CMAKE_CURRENT_BINARY_DIR
    DEPENDS ${FILES_IN_ABS}  # absolute paths required for DEPENDS
    COMMAND "${CMAKE_COMMAND}" -E tar cf "${PATH_OUT_ABS}" --format=zip -- ${FILES_IN}
    WORKING_DIRECTORY "${DIR_IN}" # run command in this directory
    COMMENT "Compressing '${PATH_OUT}'"
    VERBATIM
    )
endfunction(fn__build_zip)
