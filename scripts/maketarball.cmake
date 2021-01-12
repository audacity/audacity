
set( TARGET minsrc )

# list command no longer ignores empty elements.
cmake_policy( SET CMP0007 NEW )

# Check to make sure the source tree has no uncommitted changes
execute_process(
   COMMAND
      ${GIT_EXECUTABLE} status -s --untracked-files=no
   WORKING_DIRECTORY
      ${TARGET_ROOT}
   OUTPUT_VARIABLE
      output
)

if( output )
#   message( FATAL_ERROR "You have uncommitted changes\n${output}" )
endif()

# Get the list of files in the repo
execute_process(
   COMMAND
      ${GIT_EXECUTABLE} ls-tree -r --name-only HEAD ${TARGET_ROOT}
   WORKING_DIRECTORY
      ${TARGET_ROOT}
   OUTPUT_VARIABLE
      output
)

# Convert the output to a list
string( REPLACE "\n" ";" output "${output}" )

# Convert excludes to regular expressions
string( REPLACE " " ".*$|^" EXCLUDES "${EXCLUDES}" )

# Remove unwanted files from the list
list( FILTER output EXCLUDE REGEX "^${EXCLUDES}.*$|^$" )

# Add our TLD to each filename
list( TRANSFORM output PREPEND "${TLD}/" )

message( STATUS "Creating the minsrc archive at:" )
message( STATUS )
message( STATUS "  ${TARBALL}" )
message( STATUS )

# Write the list to a file to circumvent command line length limits
set( filelist "${CMAKE_CURRENT_BINARY_DIR}/filelist" )
string( REPLACE ";" "\n" output "${output}" )
file( WRITE "${filelist}" ${output} )

# Create a symlink to provide a TLD
execute_process(
   COMMAND
      ${CMAKE_COMMAND} -E create_symlink ${TARGET_ROOT} ${TLD}
)

# Create the tarball
execute_process(
   COMMAND
      ${CMAKE_COMMAND} -E tar cfJ ${TARBALL} --files-from=${filelist}
)

