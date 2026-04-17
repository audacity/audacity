#[[
Model Context Protocol server exposing Audacity's scripting / macro commands
over localhost HTTP. Mirrors mod-script-pipe but speaks MCP JSON-RPC instead
of the colon-delimited pipe format.
]]

set( SOURCES
   MCPModule.cpp
   MCPServer.cpp
)
set( DEFINES
   PRIVATE
      BUILDING_MOD_MCP

      # Match mod-script-pipe: prevents wxWidgets debug.h from defining
      # __WXDEBUG__ and emitting debug library pragmas in Release builds.
      wxDEBUG_LEVEL=0

      # cpp-httplib tunables. We never need TLS (localhost only) or payload
      # compression, so opt out of those features to keep the dependency
      # surface small.
      CPPHTTPLIB_THREAD_POOL_COUNT=4
)
set( LIBRARIES
   PRIVATE
      Audacity
      rapidjson::rapidjson
)
audacity_module( mod-mcp "${SOURCES}" "${LIBRARIES}"
   "${DEFINES}" "" )

target_include_directories( mod-mcp PRIVATE
   "${CMAKE_CURRENT_SOURCE_DIR}/httplib"
)
