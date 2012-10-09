# SoX Resampler Library       Copyright (c) 2007-12 robs@users.sourceforge.net
# Licence for this file: LGPL v2.1                  See LICENCE for details.

if (${bits} STREQUAL 24)
  set (quality A0045)
else ()
  set (quality A0044)
endif ()

execute_process(COMMAND ${obin}3b-options-with-input-fn ${from} ${to} 1 2 2 ${quality}
  INPUT_FILE ref-${from}.s32
  OUTPUT_FILE ${from}-${to}.s32
  ERROR_VARIABLE test_error
  RESULT_VARIABLE test_result)

if (test_result)
  message (FATAL_ERROR "Resampling failure: ${test_error}")
#else ()
  #message (STATUS ${test_error})
endif ()

execute_process(COMMAND ${bin}vector-cmp ref-${to}.s32 ${from}-${to}.s32 ${to} ${leader} ${len} ${bits} 98
  OUTPUT_VARIABLE test_output
  RESULT_VARIABLE test_result)

if (test_result)
  message (FATAL_ERROR ${test_output})
else ()
  message (STATUS ${test_output})
endif ()
