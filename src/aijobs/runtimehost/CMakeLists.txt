add_executable(ai_runtime_host main.cpp)
target_link_libraries(ai_runtime_host PRIVATE Qt6::Core Qt6::Network)
set_target_properties(ai_runtime_host PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_INSTALL_PREFIX}/bin)
install(TARGETS ai_runtime_host RUNTIME DESTINATION bin)
