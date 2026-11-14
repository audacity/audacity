#[[
A directory containing module targets
]]

# Include the modules that we'll build

# The list of modules is ordered so that each module occurs after any others
# that it depends on
set( MODULES
   mod-script-pipe
)

audacity_module_subdirectory("${MODULES}")
