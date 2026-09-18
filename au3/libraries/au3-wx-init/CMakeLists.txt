#[[
Startup time injections of wxWidgets based implementations of services behind
toolkit-neutral facades
]]

set( TARGET au3-wx-init )

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
PUBLIC
   au3-basic-ui
   au3-exceptions
   au3-preferences
   au3-strings
PRIVATE
   au3-files
   # If Sentry reporting is disabled, au3-sentry-reporting is a header-only INTERFACE library
   au3-sentry-reporting
   au3-shuttlegui
   au3-string-utils
   au3-theme
   au3-utility
   au3-wx-wrappers
   wxBase
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
