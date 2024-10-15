#[[
A directory containing module targets
]]

# Include the modules that we'll build

# The list of modules is ordered so that each module occurs after any others
# that it depends on
set(MODULES)

if ( ${_OPT}has_audiocom_upload)
   list( APPEND MODULES
      mod-cloud-audiocom
   )
endif()

audacity_module_subdirectory("${MODULES}")
