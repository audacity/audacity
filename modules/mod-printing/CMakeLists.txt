#[[
Inserts Page Setup and Print commands in the File menu, to print images of
tracks
]]

set( SOURCES
   Printing.cpp
)
audacity_module( mod-printing "${SOURCES}" "Audacity"
   "" "" )
