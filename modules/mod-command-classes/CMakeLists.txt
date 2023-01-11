#[[
Inserts many individual macro commands that become available in the macro
editing dialog.  This single module might easily be divided into many.
]]

set( SOURCES
   CompareAudioCommand.cpp
   CompareAudioCommand.h
   Demo.cpp
   Demo.h
   DragCommand.cpp
   DragCommand.h
   GetInfoCommand.cpp
   GetInfoCommand.h
   GetTrackInfoCommand.cpp
   GetTrackInfoCommand.h
   HelpCommand.cpp
   HelpCommand.h
   ImportExportCommands.cpp
   ImportExportCommands.h
   MessageCommand.cpp
   MessageCommand.h
   ModCommandsMain.cpp
   OpenSaveCommands.cpp
   OpenSaveCommands.h
   PreferenceCommands.cpp
   PreferenceCommands.h
   SelectCommand.cpp
   SelectCommand.h
   SetClipCommand.cpp
   SetClipCommand.h
   SetEnvelopeCommand.cpp
   SetEnvelopeCommand.h
   SetLabelCommand.cpp
   SetLabelCommand.h
   SetProjectCommand.cpp
   SetProjectCommand.h
   SetTrackInfoCommand.cpp
   SetTrackInfoCommand.h
)
set( LIBRARIES
   Audacity
)
audacity_module( mod-command-classes "${SOURCES}" "${LIBRARIES}"
   "" "" )
