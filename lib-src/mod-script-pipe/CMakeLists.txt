#directory lib-src/mod-script-pipe
project (mod-script-pipe)
find_package(wxWidgets REQUIRED COMPONENTS net core base)
include(${wxWidgets_USE_FILE})
include_directories(${install_dir}../../win)

# This defines the #define on both Windows and Linux.
add_definitions( -DBUILDING_SCRIPT_PIPE )
add_library( mod-script-pipe MODULE PipeServer.cpp ScripterCallback.cpp)
target_link_libraries(mod-script-pipe ${wxWidgets_LIBRARIES})