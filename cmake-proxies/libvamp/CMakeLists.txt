
add_library( ${TARGET} STATIC )

def_vars()

list( APPEND SOURCES
   PRIVATE
      ${TARGET_ROOT}/src/vamp-hostsdk/PluginBufferingAdapter.cpp
      ${TARGET_ROOT}/src/vamp-hostsdk/PluginChannelAdapter.cpp
      ${TARGET_ROOT}/src/vamp-hostsdk/PluginHostAdapter.cpp
      ${TARGET_ROOT}/src/vamp-hostsdk/PluginInputDomainAdapter.cpp
      ${TARGET_ROOT}/src/vamp-hostsdk/PluginLoader.cpp
      ${TARGET_ROOT}/src/vamp-hostsdk/PluginSummarisingAdapter.cpp
      ${TARGET_ROOT}/src/vamp-hostsdk/PluginWrapper.cpp
      ${TARGET_ROOT}/src/vamp-hostsdk/RealTime.cpp
)

list( APPEND INCLUDES
   PUBLIC
      ${TARGET_ROOT}
)

list( APPEND DEFINES
   PRIVATE
      _USE_MATH_DEFINES
)

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )

