#directory lib-src
set( LIB_SRC_DIRECTORY ${top_dir}/lib-src/ )

#These two are done in lib-src
add_subdirectory( "mod-script-pipe" )
add_subdirectory( "FileDialog" )

#Same idea, but not yet done/needed
#add_subdirectory( "mod-null" )
#add_subdirectory( "mod-nyq-bench" )
#add_subdirectory( "mod-track-panel" )

#These are included in some other lib, and don't need a lib of their own.
#add_subdirectory( "lib-widget-extra" )

# Most of the others are done via cmake-proxies subdirectory.


#Some left over file names, not yet used in the cmake build.
#[[
${LIB_SRC_DIRECTORY}mod-null/ModNullCallback.cpp
${LIB_SRC_DIRECTORY}mod-nyq-bench/NyqBench.cpp

${LIB_SRC_DIRECTORY}mod-track-panel/ModTrackPanelCallback.cpp
${LIB_SRC_DIRECTORY}mod-track-panel/Registrar.cpp
${LIB_SRC_DIRECTORY}mod-track-panel/SkewedRuler.cpp
${LIB_SRC_DIRECTORY}mod-track-panel/TrackPanel2.cpp

${LIB_SRC_DIRECTORY}lib-widget-extra/NonGuiThread.cpp

]]#



