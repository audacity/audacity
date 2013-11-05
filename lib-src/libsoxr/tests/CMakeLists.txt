# SoX Resampler Library       Copyright (c) 2007-13 robs@users.sourceforge.net
# Licence for this file: LGPL v2.1                  See LICENCE for details.

add_definitions (${PROJECT_C_FLAGS})
link_libraries (${PROJECT_NAME})

file (GLOB SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/*.c)
foreach (fe ${SOURCES})
  get_filename_component (f ${fe} NAME_WE)
  add_executable (${f} ${fe})
endforeach ()

enable_testing ()

set (sweep_to_freq 22050)
set (leader 1)
set (len 16)
math (EXPR base_rate "${sweep_to_freq} + ${sweep_to_freq}")

macro (add_vector r)
  set (output ${CMAKE_CURRENT_BINARY_DIR}/ref-${r}.s32)
  add_custom_command (OUTPUT ${output} DEPENDS vector-gen ${CMAKE_CURRENT_LIST_FILE}
    COMMAND vector-gen ${r} ${leader} ${len} ${sweep_to_freq} 1 ${output})
  set (vectors ${output} ${vectors})
endmacro ()

macro (add_cmp_test from to bits)
  set (name ${bits}-bit-perfect-${from}-${to})
  add_test (NAME ${name} COMMAND ${CMAKE_COMMAND} -Dbits=${bits} -DBIN=${BIN} -DEXAMPLES_BIN=${EXAMPLES_BIN} -Dleader=${leader} -Dto=${to}
    -Dfrom=${from} -Dlen=${len} -P ${CMAKE_CURRENT_SOURCE_DIR}/cmp-test.cmake)
  add_vector (${from})
  add_vector (${to})
endmacro ()

unset (test_bits)
if (WITH_SINGLE_PRECISION)
  set (test_bits 20)
endif ()
if (WITH_DOUBLE_PRECISION)
  set (test_bits ${test_bits} 24)
endif ()

foreach (b ${test_bits})
  foreach (r 96000 65537)
    add_cmp_test (${base_rate} ${r} ${b})
    add_cmp_test (${r} ${base_rate} ${b})
  endforeach ()
endforeach ()

add_custom_target (test-vectors ALL DEPENDS ${vectors})
