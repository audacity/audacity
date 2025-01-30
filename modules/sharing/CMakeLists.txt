#[[
A directory containing module targets
]]

# Include the modules that we'll build



set(MODULES)

if ( ${_OPT}has_audiocom_upload)
   list( APPEND MODULES
      mod-cloud-audiocom
      mod-musehub-ui
   )
endif()

audacity_module_subdirectory("${MODULES}")
