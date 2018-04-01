#directory cmake-proxies/twolame
#UNUSED
set( TARGET twolame )
set( TARGET_SOURCE ${LIB_SRC_DIRECTORY}${TARGET} )
project( ${TARGET} )

set( SOURCES 
#${LIB_SRC_DIRECTORY}twolame/frontend/audioin_raw.c
#${LIB_SRC_DIRECTORY}twolame/frontend/audioin_sndfile.c
#${LIB_SRC_DIRECTORY}twolame/frontend/frontend.c
${LIB_SRC_DIRECTORY}twolame/libtwolame/ath.c
${LIB_SRC_DIRECTORY}twolame/libtwolame/availbits.c
${LIB_SRC_DIRECTORY}twolame/libtwolame/bitbuffer.c
${LIB_SRC_DIRECTORY}twolame/libtwolame/crc.c
${LIB_SRC_DIRECTORY}twolame/libtwolame/dab.c
${LIB_SRC_DIRECTORY}twolame/libtwolame/encode.c
${LIB_SRC_DIRECTORY}twolame/libtwolame/energy.c
${LIB_SRC_DIRECTORY}twolame/libtwolame/fft.c
${LIB_SRC_DIRECTORY}twolame/libtwolame/get_set.c
${LIB_SRC_DIRECTORY}twolame/libtwolame/mem.c
${LIB_SRC_DIRECTORY}twolame/libtwolame/psycho_0.c
${LIB_SRC_DIRECTORY}twolame/libtwolame/psycho_1.c
${LIB_SRC_DIRECTORY}twolame/libtwolame/psycho_2.c
${LIB_SRC_DIRECTORY}twolame/libtwolame/psycho_3.c
${LIB_SRC_DIRECTORY}twolame/libtwolame/psycho_4.c
${LIB_SRC_DIRECTORY}twolame/libtwolame/psycho_n1.c
${LIB_SRC_DIRECTORY}twolame/libtwolame/subband.c
${LIB_SRC_DIRECTORY}twolame/libtwolame/twolame.c
${LIB_SRC_DIRECTORY}twolame/libtwolame/util.c
#${LIB_SRC_DIRECTORY}twolame/simplefrontend/audio_wave.c
#${LIB_SRC_DIRECTORY}twolame/simplefrontend/simplefrontend.c
)
# This defines the #define on both Windows and Linux.
add_definitions( 
-D_LIB
-DLIBTWOLAME_STATIC
 )
add_library( ${TARGET} STATIC ${SOURCES})

target_include_directories( ${TARGET} PRIVATE 

)

target_link_libraries( ${TARGET} )