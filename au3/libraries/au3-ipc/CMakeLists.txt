#[[
Small crossplatform IPC library, provides a simple way
to transfer data/messages between processes.
]]

set( TARGET au3-ipc )

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
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
