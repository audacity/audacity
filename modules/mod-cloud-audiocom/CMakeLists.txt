set( TARGET mod-cloud-audiocom )

set( SOURCES
   menus/Menus.cpp
   ui/LinkAccountDialog.cpp
   ui/LinkAccountDialog.h
   ui/LinkFailedDialog.cpp
   ui/LinkFailedDialog.h
   ui/LinkSucceededDialog.cpp
   ui/LinkSucceededDialog.h
   ui/ShareAudioDialog.cpp
   ui/ShareAudioDialog.h
   ui/ShareAudioToolbar.cpp
   ui/ShareAudioToolbar.h
   ui/UserImage.cpp
   ui/UserImage.h

   AudioComModule.cpp
   AuthorizationHandler.cpp
   AuthorizationHandler.h
)

if(${_OPT}has_url_schemes_support)
   list(APPEND SOURCES
      LinkURLHandler.cpp
   )
endif()

set( LIBRARIES
   PRIVATE
      lib-cloud-audiocom-interface
      lib-menus-interface
      lib-shuttlegui-interface
      lib-wx-wrappers-interface
      Audacity # Toolbars live here
)

audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
