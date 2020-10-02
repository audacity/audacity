#[[
User interface for effects using the LADSPA protocol
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
      LadspaEditor.cpp
      LadspaEditor.h
      LadspaEffect.cpp
      LadspaEffect.h
      LadspaEffectOptionsDialog.cpp
      LadspaEffectOptionsDialog.h
   )
   set (LIBRARIES
      PRIVATE
         Audacity
	 lib-ladspa-interface
   )
   audacity_module( mod-ladspa "${SOURCES}" "${LIBRARIES}"
      "" "" )
  # Third party libraries linked directly only by this module
  # But others may have transitive dependency on them through this
  set(EXTRA_CLUSTER_NODES "lib-ladspa" PARENT_SCOPE)

endif()
