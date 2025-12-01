#[[
This library provides an interface to configure and start Crashpad handler 
in a platform independent way.
]]

set( TARGET au3-crashpad-configurer )

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

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}" "${DEFINES}" "" )
