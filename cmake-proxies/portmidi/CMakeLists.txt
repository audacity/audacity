
add_library( ${TARGET} STATIC )

list( APPEND SOURCES
   PRIVATE
      ${TARGET_ROOT}/pm_common/portmidi.c
      ${TARGET_ROOT}/pm_common/pmutil.c
      ${TARGET_ROOT}/porttime/porttime.c
      $<$<PLATFORM_ID:Windows>:${TARGET_ROOT}/pm_win/pmwin.c>
      $<$<PLATFORM_ID:Windows>:${TARGET_ROOT}/pm_win/pmwinmm.c>
      $<$<PLATFORM_ID:Windows>:${TARGET_ROOT}/porttime/ptwinmm.c>
      $<$<PLATFORM_ID:Darwin>:${TARGET_ROOT}/pm_mac/finddefault.c>
      $<$<PLATFORM_ID:Darwin>:${TARGET_ROOT}/pm_mac/pmmac.c>
      $<$<PLATFORM_ID:Darwin>:${TARGET_ROOT}/pm_mac/pmmacosxcm.c>
      $<$<PLATFORM_ID:Darwin>:${TARGET_ROOT}/pm_mac/readbinaryplist.c>
      $<$<PLATFORM_ID:Darwin>:${TARGET_ROOT}/porttime/ptmacosx_mach.c>
      $<$<PLATFORM_ID:Linux>:${TARGET_ROOT}/pm_linux/finddefault.c>
      $<$<PLATFORM_ID:Linux>:${TARGET_ROOT}/pm_linux/pmlinux.c>
      $<$<PLATFORM_ID:Linux>:${TARGET_ROOT}/pm_linux/pmlinuxalsa.c>
      $<$<PLATFORM_ID:Linux>:${TARGET_ROOT}/porttime/ptlinux.c>
)

list( APPEND INCLUDES
   PRIVATE
      ${TARGET_ROOT}/pm_common
      ${TARGET_ROOT}/porttime
      $<$<PLATFORM_ID:Windows>:${TARGET_ROOT}/pm_win>
      $<$<PLATFORM_ID:Linux>:${TARGET_ROOT}/pm_linux>
      $<$<PLATFORM_ID:Darwin>:${TARGET_ROOT}/pm_win>
)

list( APPEND DEFINES
   PRIVATE
      $<$<PLATFORM_ID:Linux>:PMALSA=1>
)

list( APPEND OPTIONS
   PRIVATE
      $<$<PLATFORM_ID:Linux>:
         -Wno-pointer-to-int-cast
         -Wno-int-to-pointer-cast
         -Wno-implicit-function-declaration
      >
)

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_compile_features( ${TARGET} PRIVATE ${FEATURES} )
target_compile_options( ${TARGET} PRIVATE ${OPTIONS} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )

