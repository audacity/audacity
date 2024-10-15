#[[
Startup time injections of wxWidgets based implementations of services behind
toolkit-neutral facades
]]

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
   lib-shuttlegui-interface
   lib-string-utils-interface
   lib-theme-interface
   lib-wx-wrappers-interface
   lib-preferences-interface
   # If Sentry reporting is enabled, this library depends on
   # lib-network-manager-interface
   # If Sentry reporting is disabled, an INTERFACE library
   # will be defined
   lib-sentry-reporting-interface
)

audacity_library( lib-wx-init "${SOURCES}" "${LIBRARIES}"
   "" ""
)
