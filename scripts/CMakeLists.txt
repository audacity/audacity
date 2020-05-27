
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
   .github
   .gitignore
   .travis.yml
   appveyor.yml
#   lib-src/expat
#   lib-src/libid3tag
#   lib-src/libmad
#   lib-src/libogg
#   lib-src/libscorealign
#   lib-src/libvorbis
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
#   lib-src/soundtouch
   mac
   plug-ins/analyze.ny
   plug-ins/fadein.ny
   plug-ins/fadeout.ny
   plug-ins/undcbias.ny
   qa
   tests/ProjectCheckTests
   tests/samples
   todo.txt
   win
)

set( TLD "audacity-minsrc-${AUDACITY_DIST_VERSION}${AUDACITY_SUFFIX}" )
set( TARBALL "${PROJECT_BINARY_DIR}/${TLD}.tar.xz" )
   
add_custom_target( ${TARGET}
   COMMAND
      ${CMAKE_COMMAND} -D GIT_EXECUTABLE="${GIT_EXECUTABLE}"
                       -D TARGET_ROOT="${TARGET_ROOT}"
                       -D EXCLUDES="${EXCLUDES}"
                       -D TARBALL="${TARBALL}"
                       -D TLD="${TLD}"
                       -P "${_SRCDIR}/maketarball.cmake"
)

