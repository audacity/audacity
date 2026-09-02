#[[
AU3 import/export format plugins, built as static libraries for AU4.

The au3-import-export-modules library holds only the registration entry
points (RegisterExportPlugins / RegisterImportPlugins, AU4-specific files);
each format lives in its own au3-mod-* library. src/importexport calls the
registration functions explicitly, which pulls in every format plugin
through the ordinary static link.

Some files in these directories are not compiled in AU4: Audacity 3 builds
the formats as dynamically loaded modules, and mod-cl, mod-lof, mod-aup,
the DEFINE_MODULE_ENTRIES module-entry stubs and the wx preference pages
only make sense in that setup (or need the wx GUI application).

The codec targets are PUBLIC on the mod libraries because the mod headers
expose codec types; slimming the headers so codecs can go PRIVATE is tracked
in https://github.com/audacity/audacity/issues/11803
]]

add_subdirectory( mod-ffmpeg/lib-ffmpeg-support au3-ffmpeg-support )
add_subdirectory( mod-mp3 au3-mod-mp3 )
add_subdirectory( mod-mpg123 au3-mod-mpg123 )
add_subdirectory( mod-wavpack au3-mod-wavpack )
add_subdirectory( mod-pcm au3-mod-pcm )
add_subdirectory( mod-ogg au3-mod-ogg )
add_subdirectory( mod-flac au3-mod-flac )
add_subdirectory( mod-opus au3-mod-opus )
add_subdirectory( mod-mp2 au3-mod-mp2 )
add_subdirectory( mod-ffmpeg au3-mod-ffmpeg )

set( TARGET au3-import-export-modules )

set( SOURCES
   RegisterExportPlugins.cpp
   RegisterExportPlugins.h
   RegisterImportPlugins.cpp
   RegisterImportPlugins.h
)

set( LIBRARIES
PRIVATE
   au3-mod-mp3
   au3-mod-mpg123
   au3-mod-wavpack
   au3-mod-pcm
   au3-mod-ogg
   au3-mod-flac
   au3-mod-opus
   au3-mod-mp2
   au3-mod-ffmpeg
   au3-import-export
   wxBase
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)

# Consumers include "RegisterExportPlugins.h" / "RegisterImportPlugins.h"
# relative to this directory.
target_include_directories( ${TARGET} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR} )
