#[[
Adds all user interface related to label tracks, including menu items and a
macro command

Adds graphical representation of labels and mouse interaction
]]

set( SOURCES
   DropoutDetector.cpp
   LabelDefaultClickHandle.cpp
   LabelDefaultClickHandle.h
   LabelDialog.cpp
   LabelDialog.h
   LabelGlyphHandle.cpp
   LabelGlyphHandle.h
   LabelImportExport.cpp
   LabelImportExport.h
   LabelImportExportMenus.cpp
   LabelMenus.cpp
   LabelNavigationMenus.cpp
   LabelTextHandle.cpp
   LabelTextHandle.h
   LabelTrackCopyPaste.cpp
   LabelTrackControls.cpp
   LabelTrackControls.h
   LabelTrackShifter.cpp
   LabelTrackVRulerControls.cpp
   LabelTrackVRulerControls.h
   LabelTrackView.cpp
   LabelTrackView.h
   SetLabelCommand.cpp
   SetLabelCommand.h
)
audacity_module( mod-label-track-ui "${SOURCES}" "Audacity"
   "" "" )
