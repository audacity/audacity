# Test projects
# Use the macro to add test projects

MACRO(ADD_TEST appl_name)
   ADD_EXECUTABLE(${appl_name} "${appl_name}.c")
   TARGET_LINK_LIBRARIES(${appl_name} portaudio_static)
ENDMACRO(ADD_TEST)

ADD_TEST(patest_longsine)
