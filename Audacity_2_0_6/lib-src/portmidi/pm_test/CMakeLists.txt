# pm_test

# set the build directory to be in portmidi, not in portmidi/pm_test
# this is required for Xcode:
if(APPLE)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR})
endif(APPLE)

if(WIN32)
include(../pm_win/static.cmake)
endif(WIN32)

macro(make_a_test name)
  add_executable(${name} ${name}.c)
  target_link_libraries(${name} portmidi-static ${PM_NEEDED_LIBS})
  add_dependencies(${name} portmidi-static)
endmacro(make_a_test)

make_a_test(test)
make_a_test(midithread)
make_a_test(midithru)
make_a_test(sysex)
make_a_test(latency)
make_a_test(mm)
make_a_test(midiclock)
make_a_test(qtest)
