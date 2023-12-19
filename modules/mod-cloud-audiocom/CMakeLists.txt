set( TARGET mod-cloud-audiocom )

set( SOURCES
   menus/Menus.cpp
   ui/AudioComSyncUI.cpp
   ui/CloudProjectPropertiesDialog.cpp
   ui/CloudProjectPropertiesDialog.h
   ui/LinkWithTokenDialog.cpp
   ui/LinkWithTokenDialog.h
   ui/LinkFailedDialog.cpp
   ui/LinkFailedDialog.h
   ui/LinkSucceededDialog.cpp
   ui/LinkSucceededDialog.h
   ui/MixdownPropertiesDialog.cpp
   ui/MixdownPropertiesDialog.h
   ui/ProjectsListDialog.cpp
   ui/ProjectsListDialog.h
   ui/SelectSaveLocationDialog.cpp
   ui/SelectSaveLocationDialog.h
   ui/ShareAudioDialog.cpp
   ui/ShareAudioDialog.h
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
