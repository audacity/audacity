#[[
A non-modal dialog displays information about undo and redo history states and
allows one-click movement among them.  A button to purge old states and
conserve some memory and disk space may be enabled in the build.
]]

list( APPEND SOURCES
   HistoryWindow.cpp
   HistoryWindow.h
)
audacity_module( mod-undo-history "${SOURCES}" "Audacity"
   "" "" )
