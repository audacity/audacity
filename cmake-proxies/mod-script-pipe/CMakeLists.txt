#directory cmake-proxies/mod-script-pipe
set( TARGET mod-script-pipe )
set( TARGET_SOURCE ${LIB_SRC_DIRECTORY}${TARGET} )
project( ${TARGET} )
find_package(wxWidgets REQUIRED COMPONENTS net core base)
include(${wxWidgets_USE_FILE})

set( SOURCES 
${TARGET_SOURCE}/PipeServer.cpp
${TARGET_SOURCE}/ScripterCallback.cpp
)
# This defines the #define on both Windows and Linux.
add_definitions( -DBUILDING_SCRIPT_PIPE )
add_library( ${TARGET} MODULE ${SOURCES})

target_include_directories( ${TARGET} PRIVATE 
  ${top_dir}/win
)

target_link_libraries( ${TARGET}  ${wxWidgets_LIBRARIES})