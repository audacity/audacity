#[[
Small crossplatform IPC library, provides a simple way
to transfer data/messages between processes.
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

set( SOURCES
   IPCChannel.cpp
   IPCChannel.h
   IPCClient.cpp
   IPCClient.h
   IPCServer.cpp
   IPCServer.h
   internal/BufferedIPCChannel.cpp
   internal/BufferedIPCChannel.h
   internal/ipc-types.h
   internal/socket_guard.h
)
set( LIBRARIES
   au3-utility-interface
   PRIVATE
      $<$<PLATFORM_ID:Windows>:wsock32>
      $<$<PLATFORM_ID:Windows>:ws2_32>
)
audacity_library( au3-ipc "${SOURCES}" "${LIBRARIES}"
   "" ""
)
