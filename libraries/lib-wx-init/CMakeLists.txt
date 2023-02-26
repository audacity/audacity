#[[
Startup time injections of wxWidgets based implementations of services behind
toolkit-neutral facades
]]#

set( SOURCES
   AudacityLogoAlpha.xpm
   AudacityMessageBox.cpp
   AudacityMessageBox.h
   BasicMenu.cpp
   BasicMenu.h
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
   JournalEvents.cpp
   JournalEvents.h
   JournalOutput.cpp
   JournalOutput.h
   JournalRegistry.cpp
   JournalRegistry.h
   JournalWindowPaths.cpp
   JournalWindowPaths.h
   KeyboardCapture.cpp
   KeyboardCapture.h
   LogWindow.cpp
   LogWindow.h
   MultiDialog.cpp
   MultiDialog.h
   NumericTextCtrl.cpp
   NumericTextCtrl.h
   ProgressDialog.cpp
   ProgressDialog.h
   SelectFile.cpp
   SelectFile.h
   VetoDialogHook.h
   auStaticText.cpp
   auStaticText.h
   numformatter.cpp
   numformatter.h
   valnum.cpp
   valnum.h
   wxWidgetsBasicUI.cpp
   wxWidgetsBasicUI.h
   wxWidgetsWindowPlacement.cpp
   wxWidgetsWindowPlacement.h
)
set( LIBRARIES
   lib-numeric-formats-interface
   lib-shuttlegui-interface
   lib-string-utils-interface
   lib-theme-interface
   lib-wx-wrappers-interface
)
audacity_library( lib-wx-init "${SOURCES}" "${LIBRARIES}"
   "" ""
)

if( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
   set_source_files_properties(
      KeyboardCapture.cpp
      PROPERTIES
         COMPILE_FLAGS "-x objective-c++"
         SKIP_PRECOMPILE_HEADERS YES
   )
endif()
