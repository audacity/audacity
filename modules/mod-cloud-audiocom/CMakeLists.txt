set( TARGET mod-cloud-audiocom )

set( SOURCES
   ui/dialogs/LinkFailedDialog.cpp
   ui/dialogs/LinkFailedDialog.h
   ui/dialogs/LinkSucceededDialog.cpp
   ui/dialogs/LinkSucceededDialog.h
   ui/dialogs/LinkWithTokenDialog.cpp
   ui/dialogs/LinkWithTokenDialog.h
   ui/dialogs/ShareAudioDialog.cpp
   ui/dialogs/ShareAudioDialog.h

   ui/ShareAudioToolbar.cpp
   ui/ShareAudioToolbar.h
   ui/UserImage.cpp
   ui/UserImage.h
   ui/UserPanel.cpp
   ui/UserPanel.h

   AudioComModule.cpp
   AuthorizationHandler.cpp
   AuthorizationHandler.h
   CloudModuleSettings.cpp
   CloudModuleSettings.h
)

if(${_OPT}has_url_schemes_support)
   list(APPEND SOURCES
      LinkUrlHandler.cpp
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
