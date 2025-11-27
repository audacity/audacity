#[[
Startup time injections of wxWidgets based implementations of services behind
toolkit-neutral facades
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

set( SOURCES
   AccessibleLinksFormatter.cpp
   AccessibleLinksFormatter.h
   AudacityLogoAlpha.xpm
   AudacityMessageBox.cpp
   AudacityMessageBox.h
   $<$<BOOL:${${_OPT}has_sentry_reporting}>:
      ErrorReportDialog.cpp
      ErrorReportDialog.h
   >
   ErrorDialog.cpp
   ErrorDialog.h
   HelpSystem.cpp
   HelpSystem.h
   HelpText.cpp
   HelpText.h
   HtmlWindow.cpp
   HtmlWindow.h
   Journal.cpp
   Journal.h
   JournalOutput.cpp
   JournalOutput.h
   JournalRegistry.cpp
   JournalRegistry.h
   LogWindow.cpp
   LogWindow.h
   MultiDialog.cpp
   MultiDialog.h
   ProgressDialog.cpp
   ProgressDialog.h
   SelectFile.cpp
   SelectFile.h
   wxWidgetsBasicUI.cpp
   wxWidgetsBasicUI.h
   wxWidgetsWindowPlacement.cpp
   wxWidgetsWindowPlacement.h
   SettingsWX.cpp
   SettingsWX.h
)
set( LIBRARIES
   au3-shuttlegui-interface
   au3-string-utils-interface
   au3-theme-interface
   au3-wx-wrappers-interface
   au3-preferences-interface
   # If Sentry reporting is enabled, this library depends on
   # au3-network-manager-interface
   # If Sentry reporting is disabled, an INTERFACE library
   # will be defined
   au3-sentry-reporting-interface
)

audacity_library( au3-wx-init "${SOURCES}" "${LIBRARIES}"
   "" ""
)
