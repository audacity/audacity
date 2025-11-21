#[[
A library for certain dialog utilities for text selection, file selection,
accessibility, and wrapping wxDialog base classes to use TranslatableString
instead of wxString in constructions.

Very much not toolkit-neutral!
]]

set( SOURCES
   AccessibilityUtils.cpp
   AccessibilityUtils.h
   AudacityTextEntryDialog.cpp
   AudacityTextEntryDialog.h
   AudacityDontAskAgainMessageDialog.cpp
   AudacityDontAskAgainMessageDialog.h
   SpinControl.cpp
   SpinControl.h
   FileDialog/FileDialog.cpp
   FileDialog/FileDialog.h
   $<$<PLATFORM_ID:Windows>:
      FileDialog/win/FileDialogPrivate.cpp
      FileDialog/win/FileDialogPrivate.h
   >
   $<$<PLATFORM_ID:Darwin>:
      FileDialog/mac/FileDialogPrivate.mm
      FileDialog/mac/FileDialogPrivate.h
   >
   $<$<PLATFORM_ID:Linux,FreeBSD,OpenBSD,CYGWIN>:
      FileDialog/gtk/FileDialogPrivate.cpp
      FileDialog/gtk/FileDialogPrivate.h
   >
   WindowAccessible.cpp
   WindowAccessible.h
   wxPanelWrapper.cpp
   wxPanelWrapper.h
)

set( LIBRARIES
   lib-files-interface
   wxwidgets::wxwidgets
)

audacity_library( lib-wx-wrappers "${SOURCES}" "${LIBRARIES}"
   ""
   "$<$<PLATFORM_ID:Linux,FreeBSD,OpenBSD,NetBSD,CYGWIN>:PkgConfig::GTK>"
)
