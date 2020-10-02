#[[
Adds support for Vamp sound analyzer plug-ins, which can add label tracks that
mark features.

See https://www.vamp-plugins.org
]]

addlib( libvamp            vamp        VAMP        NO    YES   "vamp >= 2.5" "vamp-hostsdk >= 2.5" )

set( SOURCES
   LoadVamp.cpp
   LoadVamp.h
   VampEffect.cpp
   VampEffect.h
)
set( LIBRARIES
   Audacity
   libvamp
)
audacity_module( mod-vamp "${SOURCES}" "${LIBRARIES}"
   "" "" )
