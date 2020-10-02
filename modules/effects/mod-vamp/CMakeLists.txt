#[[
Support for analyzers that use the Vamp protocol and produce label tracks
]]

addlib( libvamp            vamp        VAMP        NO    YES   "vamp >= 2.5" "vamp-hostsdk >= 2.5" )

set( SOURCES
   LoadVamp.cpp
   LoadVamp.h
   VampEffect.cpp
   VampEffect.h
)
set( LIBRARIES
   PRIVATE
      Audacity
      libvamp
)

# Third party libraries used only by this module
set(EXTRA_CLUSTER_NODES "${LIBRARIES}" PARENT_SCOPE)

audacity_module( mod-vamp "${SOURCES}" "${LIBRARIES}"
   "" "" )
