#[[
A small program that is run at build time of the application.  For each built-in
theme it generates a header file from image files.  The images are part of the
source code tree but not deployed with the program.
]]

if( NOT IMAGE_COMPILER_EXECUTABLE )
   add_executable( image-compiler imageCompilerMain.cpp )
   set( OPTIONS )
   audacity_append_common_compiler_options( OPTIONS yes )
   target_compile_options( image-compiler PRIVATE "${OPTIONS}" )
   target_link_libraries( image-compiler PRIVATE lib-theme wxwidgets::wxwidgets )
   if(NOT CMAKE_SYSTEM_NAME MATCHES "Darwin")
      set_target_property_all( image-compiler RUNTIME_OUTPUT_DIRECTORY "${_DESTDIR}/${_EXEDIR}" )
   else()
      set_target_property_all( image-compiler RUNTIME_OUTPUT_DIRECTORY "${_DESTDIR}/${_APPDIR}/image-compiler" )
   endif()
   fix_bundle( image-compiler )
else()
   add_executable( image-compiler IMPORTED GLOBAL )
   set_property(TARGET image-compiler PROPERTY IMPORTED_LOCATION "${IMAGE_COMPILER_EXECUTABLE}")
endif()
