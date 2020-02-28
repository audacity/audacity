
set( TARGET minsrc )
set( TARGET_ROOT ${topdir} )

message( STATUS "========== Configuring ${TARGET} ==========" )

def_vars()

# Add regular expressions of files/directories to exclude from the tarball
#
# NOTE: Each item will be wrapped with "^" and ".*$"
list( APPEND EXCLUDES
   .git
   .gitattributes
   .gitignore
   lib-src/expat
   lib-src/libid3tag
   lib-src/libmad
   lib-src/libogg
   lib-src/libscorealign
   lib-src/portaudio-v19/bindings
   lib-src/portaudio-v19/doc
   lib-src/portaudio-v19/testcvs
   lib-src/portmidi/pm_csharp
   lib-src/portmidi/pm_dylib
   lib-src/portmidi/pm_java
   lib-src/portmidi/pm_mingw
   lib-src/portmidi/pm_python
   lib-src/portmidi/pm_qt
   lib-src/portmidi/pm_test
   lib-src/portmidi/portmidi_cdt.zip
   lib-src/soundtouch
   lib-src/libvorbis
   mac
   plug-ins/analyze.ny
   plug-ins/fadein.ny
   plug-ins/fadeout.ny
   plug-ins/undcbias.ny
   qa
   scripts
   tests/ProjectCheckTests
   todo.txt
   win
)

set( TARBALL "${PROJECT_BINARY_DIR}/audacity-minsrc-${AUDACITY_DIST_VERSION}${AUDACITY_SUFFIX}.tar.xz" )
   
add_custom_target( ${TARGET}
   COMMAND
      ${CMAKE_COMMAND} -D GIT_EXECUTABLE="${GIT_EXECUTABLE}"
                       -D TARGET_ROOT="${TARGET_ROOT}"
                       -D EXCLUDES="${EXCLUDES}"
                       -D TARBALL="${TARBALL}"
                       -P "${_SRCDIR}/maketarball.cmake"
)

