#[[
A dialog to set Audacity on auto-pilot to record at some later time for some
duration, then save or export and optinally exit
]]

set( SOURCES
   TimerRecordDialog.cpp
   TimerRecordDialog.h
)
set( LIBRARIES
   Audacity
)
audacity_module( mod-timer-record "${SOURCES}" "${LIBRARIES}"
   "" "" )
