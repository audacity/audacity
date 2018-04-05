#directory cmake-proxies
set( LIB_SRC_DIRECTORY ${top_dir}/lib-src/ )

#These are done in their actual directories, no need for a proxy.

#Same idea, but not yet done/needed
#add_subdirectory( "mod-null" )
#add_subdirectory( "mod-nyq-bench" )
#add_subdirectory( "mod-track-panel" )

#These are all headers, nothing to build.
#add_subdirectory( "ffmpeg" ) 
#add_subdirectory( "lame" ) 


add_subdirectory( "expat" )
add_subdirectory( "FileDialog" )
#add_subdirectory( "help" )
add_subdirectory( "libflac" )
add_subdirectory( "libflac++" )
add_subdirectory( "libid3tag" )
add_subdirectory( "libmad" )
add_subdirectory( "libnyquist" )
add_subdirectory( "libogg" )
add_subdirectory( "libscorealign" )
add_subdirectory( "libsndfile" )
add_subdirectory( "libsoxr" )
add_subdirectory( "libvamp" )
add_subdirectory( "libvorbis" )
#add_subdirectory( "locale" )
add_subdirectory( "lv2" )
add_subdirectory( "mod-script-pipe" )
add_subdirectory( "portaudio-v19" )
add_subdirectory( "portmidi" )
add_subdirectory( "portmixer" )
#add_subdirectory( "portburn" ) # not built
add_subdirectory( "portsmf" )
add_subdirectory( "sbsms" )
add_subdirectory( "soundtouch" )
add_subdirectory( "twolame" )
