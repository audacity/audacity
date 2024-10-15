#[[
Upload and download of project files in the cloud
]]

set( TARGET mod-cloud-audiocom )

set( SOURCES
   menus/AudioComMenus.cpp

   ui/images/CloudImages.cpp
   ui/images/CloudImages.hpp

   ui/dialogs/AudioComDialogBase.cpp
   ui/dialogs/AudioComDialogBase.h
   ui/dialogs/CloudLocationDialog.cpp
   ui/dialogs/CloudLocationDialog.h
   ui/dialogs/CloudProjectPropertiesDialog.cpp
   ui/dialogs/CloudProjectPropertiesDialog.h
   ui/dialogs/ConnectionIssuesDialog.cpp
   ui/dialogs/ConnectionIssuesDialog.h
   ui/dialogs/LinkAccountDialog.cpp
   ui/dialogs/LinkAccountDialog.h
   ui/dialogs/LinkFailedDialog.cpp
   ui/dialogs/LinkFailedDialog.h
   ui/dialogs/LinkSucceededDialog.cpp
   ui/dialogs/LinkSucceededDialog.h
   ui/dialogs/LinkWithTokenDialog.cpp
   ui/dialogs/LinkWithTokenDialog.h
   ui/dialogs/LoginDialog.cpp
   ui/dialogs/LoginDialog.h
   ui/dialogs/NotCloudProjectDialog.cpp
   ui/dialogs/NotCloudProjectDialog.h
   ui/dialogs/ProjectLimitDialog.cpp
   ui/dialogs/ProjectLimitDialog.h
   ui/dialogs/ProjectsListDialog.cpp
   ui/dialogs/ProjectsListDialog.h
   ui/dialogs/ProjectVersionConflictDialog.cpp
   ui/dialogs/ProjectVersionConflictDialog.h
   ui/dialogs/ShareAudioDialog.cpp
   ui/dialogs/ShareAudioDialog.h
   ui/dialogs/SyncFailedDialog.cpp
   ui/dialogs/SyncFailedDialog.h
   ui/dialogs/SyncInBackroundDialog.cpp
   ui/dialogs/SyncInBackroundDialog.h
   ui/dialogs/SyncSucceededDialog.cpp
   ui/dialogs/SyncSucceededDialog.h
   ui/dialogs/UnsyncedProjectDialog.cpp
   ui/dialogs/UnsyncedProjectDialog.h
   ui/dialogs/UploadCanceledDialog.cpp
   ui/dialogs/UploadCanceledDialog.h
   ui/dialogs/WaitForActionDialog.cpp
   ui/dialogs/WaitForActionDialog.h

   ui/AudioComPrefsPanel.cpp
   ui/CloudSyncStatusField.cpp
   ui/CloudSyncStatusField.h
   ui/ProjectCloudUIExtension.cpp
   ui/ProjectCloudUIExtension.h
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
   CloudProjectFileIOExtensions.cpp
   CloudProjectFileIOExtensions.h
   CloudProjectMixdownUtils.cpp
   CloudProjectMixdownUtils.h
   CloudProjectOpenUtils.cpp
   CloudProjectOpenUtils.h
)

if(${_OPT}has_url_schemes_support)
   list(APPEND SOURCES
      LinkUrlHandler.cpp
   )
endif()

set( LIBRARIES
   PRIVATE
      lib-cloud-audiocom-interface
)

set (EXTRA_CLUSTER_NODES "${LIBRARIES};lib-crypto" PARENT_SCOPE)

list(APPEND LIBRARIES
      lib-menus-interface
      lib-shuttlegui-interface
      lib-wx-wrappers-interface
      Audacity # Toolbars live here
)

audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
