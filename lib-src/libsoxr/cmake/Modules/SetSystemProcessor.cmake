# SoX Resampler Library       Copyright (c) 2007-16 robs@users.sourceforge.net
# Licence for this file: LGPL v2.1                  See LICENCE for details.

# Sets CMAKE_SYSTEM_PROCESSOR for cross-compiling.

macro (set_system_processor)
  if (CMAKE_CROSSCOMPILING)
    if ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "" OR "${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "unknown")
      unset(CMAKE_SYSTEM_PROCESSOR)
    endif ()
    if (NOT DEFINED CMAKE_SYSTEM_PROCESSOR)
      include (CheckCSourceCompiles)
      set (CPU_LINES
        "#if defined __x86_64__ || defined _M_X64  /*\;x86_64\;*/"
        "#if defined __i386__   || defined _M_IX86 /*\;x86_32\;*/"
        "#if defined __arm__    || defined _M_ARM  /*\;arm\;*/"
        )
      foreach (CPU_LINE ${CPU_LINES})
        string (CONCAT CPU_SOURCE "${CPU_LINE}" "
        int main() {return 0;}
        #endif
        ")
        unset (SYSTEM_PROCESSOR_DETECTED CACHE)
        check_c_source_compiles ("${CPU_SOURCE}" SYSTEM_PROCESSOR_DETECTED)
        if (SYSTEM_PROCESSOR_DETECTED)
          list (GET CPU_LINE 1 CMAKE_SYSTEM_PROCESSOR)
          message (STATUS "CMAKE_SYSTEM_PROCESSOR is ${CMAKE_SYSTEM_PROCESSOR}")
          break ()
        endif ()
      endforeach ()
    endif ()

    # N.B. Will not overwrite existing cache variable:
    set (CMAKE_SYSTEM_PROCESSOR "${CMAKE_SYSTEM_PROCESSOR}"
      CACHE STRING "Target system processor")
  endif ()
endmacro ()
