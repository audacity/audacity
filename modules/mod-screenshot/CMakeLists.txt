#[[
A dialog in the Tools menu, and a macro command, for taking screenshots of
chosen portions of Audacity's user interface.  It has been used to update
images for the manual.
]]

set( SOURCES
   Screenshot.cpp
   ScreenshotCommand.cpp
   ScreenshotCommand.h
)
audacity_module( mod-screenshot "${SOURCES}" "Audacity"
   "" "" )
