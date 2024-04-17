#[[
A directory containing modules related to the Nyquist language
]]

# Include the modules that we'll build

# The list of modules is ordered so that each module occurs after any others
# that it depends on
set( MODULES
)

if( NOT CMAKE_SYSTEM_NAME MATCHES "Windows" )
   list( APPEND MODULES
      mod-nyq-bench
   )
endif()

audacity_module_subdirectory("${MODULES}")
