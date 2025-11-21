#[[
This library provides an interface to configure and start Crashpad handler 
in a platform independent way.
]]

set(SOURCES 
   CrashpadConfigurer.h
   CrashpadConfigurer.cpp
   internal/Util.h
)
set(LIBRARIES 
   PRIVATE
      crashpad::client
      crashpad::client_common
      crashpad::util
      crashpad::mini_chromium_base
)
set(DEFINES
   INTERFACE
      -DUSE_CRASHPAD
)

audacity_library( lib-crashpad-configurer "${SOURCES}" "${LIBRARIES}" "${DEFINES}" "" )
