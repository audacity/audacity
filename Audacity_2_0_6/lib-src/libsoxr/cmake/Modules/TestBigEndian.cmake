# SoX Resampler Library       Copyright (c) 2007-13 robs@users.sourceforge.net
# Licence for this file: LGPL v2.1                  See LICENCE for details.

# - Macro to determine endian type
#  test_big_endian (VARIABLE)
#  VARIABLE - variable to store the result to

macro (test_big_endian VARIABLE)
  if ("HAVE_${VARIABLE}" MATCHES "^HAVE_${VARIABLE}$")
    include (CheckCSourceRuns)
    check_c_source_runs ("int main() {union {long i; char c[sizeof(long)];}
      const u = {1}; return !!u.c[0];}" HAVE_${VARIABLE})
    set (${VARIABLE} "${HAVE_${VARIABLE}}" CACHE INTERNAL "1 if system is big endian" FORCE)
  endif ()
endmacro ()
