
add_library( ${TARGET} STATIC )

def_vars()

if( CMAKE_SYSTEM_NAME MATCHES "Linux|FreeBSD" )
   if( ${_OPT}use_pa_alsa )
      find_package( ALSA )
   endif()
endif()

list( APPEND SOURCES
   PRIVATE
      ${TARGET_ROOT}/pm_common/portmidi.c
      ${TARGET_ROOT}/pm_common/pmutil.c
      ${TARGET_ROOT}/porttime/porttime.c

      $<$<PLATFORM_ID:Windows>:
         ${TARGET_ROOT}/pm_win/pmwin.c
         ${TARGET_ROOT}/pm_win/pmwinmm.c
         ${TARGET_ROOT}/porttime/ptwinmm.c
      >

      $<$<PLATFORM_ID:Darwin>:
         ${TARGET_ROOT}/pm_mac/finddefault.c
         ${TARGET_ROOT}/pm_mac/pmmac.c
         ${TARGET_ROOT}/pm_mac/pmmacosxcm.c
         ${TARGET_ROOT}/pm_mac/readbinaryplist.c
         ${TARGET_ROOT}/porttime/ptmacosx_mach.c
      >

      $<$<PLATFORM_ID:Linux,FreeBSD,CYGWIN>:
         ${TARGET_ROOT}/pm_linux/finddefault.c
         ${TARGET_ROOT}/pm_linux/pmlinux.c
         ${TARGET_ROOT}/porttime/ptlinux.c
      >

      $<$<BOOL:${ALSA_FOUND}>:
         ${TARGET_ROOT}/pm_linux/pmlinuxalsa.c
      >
)

list( APPEND INCLUDES
   PRIVATE
      ${TARGET_ROOT}/pm_common
      ${TARGET_ROOT}/porttime

      $<$<PLATFORM_ID:Windows>:
         ${TARGET_ROOT}/pm_win
      >

      $<$<PLATFORM_ID:Darwin>:
         ${TARGET_ROOT}/pm_mac
      >

      $<$<PLATFORM_ID:Linux,FreeBSD>:
         ${TARGET_ROOT}/pm_linux
      >
)

list( APPEND DEFINES
   PRIVATE
      $<$<BOOL:${ALSA_FOUND}>:
         PMALSA=1
      >
)

list( APPEND OPTIONS
   PRIVATE
      $<$<C_COMPILER_ID:AppleClang,Clang,GNU>:
         -Wno-pointer-to-int-cast
         -Wno-int-to-pointer-cast
         -Wno-implicit-function-declaration
      >
)

list( APPEND LIBRARIES
   PUBLIC
      $<$<PLATFORM_ID:Darwin>:
         "-framework CoreMIDI"
      >
      $<$<PLATFORM_ID:FreeBSD>:
         compat
      >
)

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_compile_options( ${TARGET} PRIVATE ${OPTIONS} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_libraries( ${TARGET} INTERFACE ${LIBRARIES} )

