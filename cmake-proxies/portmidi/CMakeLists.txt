#directory cmake-proxies/portmidi
set( TARGET portmidi )
set( TARGET_SOURCE ${LIB_SRC_DIRECTORY}${TARGET} )
project( ${TARGET} )

set( SOURCES 
#${LIB_SRC_DIRECTORY}portmidi/pm_csharp/pm_managed/AssemblyInfo.cpp
#${LIB_SRC_DIRECTORY}portmidi/pm_csharp/pm_managed/pm_managed.cpp
#${LIB_SRC_DIRECTORY}portmidi/pm_csharp/pm_managed/Stdafx.cpp

${LIB_SRC_DIRECTORY}portmidi/pm_common/pmutil.c
${LIB_SRC_DIRECTORY}portmidi/pm_common/portmidi.c
#${LIB_SRC_DIRECTORY}portmidi/pm_java/pmjni/pmjni.c
#${LIB_SRC_DIRECTORY}portmidi/pm_linux/finddefault.c
#${LIB_SRC_DIRECTORY}portmidi/pm_linux/pmlinux.c
#${LIB_SRC_DIRECTORY}portmidi/pm_linux/pmlinuxalsa.c
#${LIB_SRC_DIRECTORY}portmidi/pm_mac/finddefault.c
#${LIB_SRC_DIRECTORY}portmidi/pm_mac/pmmac.c
#${LIB_SRC_DIRECTORY}portmidi/pm_mac/pmmacosxcm.c
#${LIB_SRC_DIRECTORY}portmidi/pm_mac/readbinaryplist.c
#${LIB_SRC_DIRECTORY}portmidi/pm_python/pyportmidi/_pyportmidi.c
#${LIB_SRC_DIRECTORY}portmidi/pm_test/latency.c
#${LIB_SRC_DIRECTORY}portmidi/pm_test/midiclock.c
#${LIB_SRC_DIRECTORY}portmidi/pm_test/midithread.c
#${LIB_SRC_DIRECTORY}portmidi/pm_test/midithru.c
#${LIB_SRC_DIRECTORY}portmidi/pm_test/mm.c
#${LIB_SRC_DIRECTORY}portmidi/pm_test/qtest.c
#${LIB_SRC_DIRECTORY}portmidi/pm_test/sysex.c
#${LIB_SRC_DIRECTORY}portmidi/pm_test/test.c
${LIB_SRC_DIRECTORY}portmidi/pm_win/pmwin.c
${LIB_SRC_DIRECTORY}portmidi/pm_win/pmwinmm.c
${LIB_SRC_DIRECTORY}portmidi/porttime/porttime.c
#${LIB_SRC_DIRECTORY}portmidi/porttime/ptlinux.c
#${LIB_SRC_DIRECTORY}portmidi/porttime/ptmacosx_cf.c
#${LIB_SRC_DIRECTORY}portmidi/porttime/ptmacosx_mach.c
${LIB_SRC_DIRECTORY}portmidi/porttime/ptwinmm.c

)
# This defines the #define on both Windows and Linux.
add_definitions( 
-DLIB
 )
add_library( ${TARGET} STATIC ${SOURCES})

target_include_directories( ${TARGET} PRIVATE 
${TARGET_SOURCE}/pm_common
${TARGET_SOURCE}/pm_win
${TARGET_SOURCE}/porttime
)

target_link_libraries( ${TARGET} )