# SPDX-License-Identifier: MIT

add_executable(findlib findlib.c)

target_link_libraries(findlib ${CMAKE_DL_LIBS})

install(TARGETS findlib DESTINATION bin)
