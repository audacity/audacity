# SoX Resampler Library       Copyright (c) 2007-13 robs@users.sourceforge.net
# Licence for this file: LGPL v2.1                  See LICENCE for details.

if (${bits} STREQUAL 24)
  set (quality 45)
else ()
  set (quality 44)
endif ()

set (output ${from}-${to}-${quality}.s32)

execute_process(COMMAND ${EXAMPLES_BIN}3-options-input-fn ${from} ${to} 1 2 2 ${quality} a
  INPUT_FILE ref-${from}.s32
  OUTPUT_FILE ${output}
  ERROR_VARIABLE test_error
  RESULT_VARIABLE test_result)

if (test_result)
  message (FATAL_ERROR "Resampling failure: ${test_error}")
endif ()

execute_process(COMMAND ${BIN}vector-cmp ref-${to}.s32 ${output} ${to} ${leader} ${len} ${bits} 98
  OUTPUT_VARIABLE test_output
  RESULT_VARIABLE test_result)

if (test_result)
  message (FATAL_ERROR ${test_output})
else ()
  message (STATUS ${test_output})
endif ()
