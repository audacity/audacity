#[[
Interface to the Nyquist library, which can implement effects, generators,
analyzers, and tools invoking macro commands
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

# Third party libraries used only by this module
set(EXTRA_CLUSTER_NODES "${LIBRARIES}" PARENT_SCOPE)

audacity_module( mod-nyquist "${SOURCES}" "${LIBRARIES}"
   "" "" )
