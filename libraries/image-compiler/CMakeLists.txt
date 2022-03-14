#[[
A small program that is run at build time of the application.  For each built-in
theme it generates a header file from image files.  The images are part of the
source code tree but not deployed with the program.
]]#

if( NOT IMAGE_COMPILER_EXECUTABLE )
   add_executable( image-compiler imageCompilerMain.cpp )
   set( OPTIONS )
   audacity_append_common_compiler_options( OPTIONS yes )
   target_compile_options( image-compiler PRIVATE "${OPTIONS}" )
   target_link_libraries( image-compiler PRIVATE lib-theme wxwidgets::wxwidgets )

   set_target_properties(
      image-compiler
      PROPERTIES
      RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/utils"
   )

   # Copy the required wxWidgets libs into the executable's directory

   add_custom_command(
      TARGET image-compiler
      COMMAND
         ${CMAKE_COMMAND} 
                        -D SRC="$<TARGET_FILE:image-compiler>"
                        -D DST="$<TARGET_FILE_DIR:image-compiler>"
                        -D WXWIN="${_SHARED_PROXY_BASE_PATH}/$<CONFIG>/"
                        -P ${AUDACITY_MODULE_PATH}/CopyLibs.cmake
      POST_BUILD
   )
else()
   add_executable( image-compiler IMPORTED GLOBAL )
   set_property(TARGET image-compiler  PROPERTY IMPORTED_LOCATION "${IMAGE_COMPILER_EXECUTABLE}") 
endif()