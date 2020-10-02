#[[
Adds support for Nyquist plug-ins, which includes the Xlisp interpreter and
Lisp bindings of a sound object type and DSP functions, implemented in C, to
generate and transform sounds
]]

set( SOURCES
   LoadNyquist.cpp
   LoadNyquist.h
   Nyquist.cpp
   Nyquist.h
)
set( LIBRARIES
   Audacity
   libnyquist
)
audacity_module( mod-nyquist "${SOURCES}" "${LIBRARIES}"
   "" "" )
