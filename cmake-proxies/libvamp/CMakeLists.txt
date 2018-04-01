#directory cmake-proxies/libvamp
#UNUSED
set( TARGET libvamp )
set( TARGET_SOURCE ${LIB_SRC_DIRECTORY}${TARGET} )
project( ${TARGET} )

set( SOURCES 
#${LIB_SRC_DIRECTORY}libvamp/examples/AmplitudeFollower.cpp
#${LIB_SRC_DIRECTORY}libvamp/examples/FixedTempoEstimator.cpp
#${LIB_SRC_DIRECTORY}libvamp/examples/PercussionOnsetDetector.cpp
#${LIB_SRC_DIRECTORY}libvamp/examples/plugins.cpp
#${LIB_SRC_DIRECTORY}libvamp/examples/PowerSpectrum.cpp
#${LIB_SRC_DIRECTORY}libvamp/examples/SpectralCentroid.cpp
#${LIB_SRC_DIRECTORY}libvamp/examples/ZeroCrossing.cpp
#${LIB_SRC_DIRECTORY}libvamp/host/vamp-simple-host.cpp
#${LIB_SRC_DIRECTORY}libvamp/rdf/generator/vamp-rdf-template-generator.cpp
#${LIB_SRC_DIRECTORY}libvamp/skeleton/MyPlugin.cpp
#${LIB_SRC_DIRECTORY}libvamp/skeleton/plugins.cpp
${LIB_SRC_DIRECTORY}libvamp/src/vamp-hostsdk/PluginBufferingAdapter.cpp
${LIB_SRC_DIRECTORY}libvamp/src/vamp-hostsdk/PluginChannelAdapter.cpp
${LIB_SRC_DIRECTORY}libvamp/src/vamp-hostsdk/PluginHostAdapter.cpp
${LIB_SRC_DIRECTORY}libvamp/src/vamp-hostsdk/PluginInputDomainAdapter.cpp
${LIB_SRC_DIRECTORY}libvamp/src/vamp-hostsdk/PluginLoader.cpp
${LIB_SRC_DIRECTORY}libvamp/src/vamp-hostsdk/PluginSummarisingAdapter.cpp
${LIB_SRC_DIRECTORY}libvamp/src/vamp-hostsdk/PluginWrapper.cpp
${LIB_SRC_DIRECTORY}libvamp/src/vamp-hostsdk/RealTime.cpp
#${LIB_SRC_DIRECTORY}libvamp/src/vamp-sdk/FFT.cpp
#${LIB_SRC_DIRECTORY}libvamp/src/vamp-sdk/FFTimpl.cpp
#${LIB_SRC_DIRECTORY}libvamp/src/vamp-sdk/PluginAdapter.cpp
#${LIB_SRC_DIRECTORY}libvamp/src/vamp-sdk/RealTime.cpp

#${LIB_SRC_DIRECTORY}libvamp/src/vamp-hostsdk/acsymbols.c
#${LIB_SRC_DIRECTORY}libvamp/src/vamp-sdk/acsymbols.c

)
# This defines the #define on both Windows and Linux.
add_definitions( 
-D_LIB
-D_USE_MATH_DEFINES
 )
add_library( ${TARGET} STATIC ${SOURCES})

target_include_directories( ${TARGET} PRIVATE 
${TARGET_SOURCE}
)

target_link_libraries( ${TARGET} )