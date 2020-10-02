#[[
Adds support for LADSPA plug-ins (Linux Audio Developer's Simple Plug-in API)
]]

# Handle Ladspa option
cmd_option(
   ${_OPT}use_ladspa
   "Use LADSPA plug-in support [on, off]"
   ON
)
set( USE_LADSPA ${${_OPT}use_ladspa} CACHE INTERNAL "" )

if(USE_LADSPA)
   set( SOURCES
      LadspaEffect.cpp
      LadspaEffect.h
      ladspa.h
   )
   audacity_module( mod-ladspa "${SOURCES}" "Audacity"
      "" "" )
endif()
