# docimages_oddments.py
# Sends commands to get images for the manual.
# Image oddments that don't fit the other categories.

# Make sure Audacity is running first and that mod-script-pipe is enabled
# before running this script.
# Historically this file has had peculiar problems with line endings.

#load and run the common core.
exec( open("docimages_core.py" ).read() )

import time

def oddments_imagesA():
    for name in ["Select","Envelope","Draw","Zoom","TimeShift","Multi"] :
        do( name + "Tool" )
        capture( name + "Tool.png" , 'Tools' );
    #A track is needed for the buttons to be active.
    loadMonoTracks(1)
    do( 'SetPreference: Name="/GUI/Theme" Value="high-contrast"')
    for id in range( 11000, 11006 ):
        do( "Drag: Id="+str( id) + " FromX=10 FromY=10" )
        capture( "Button" + str(id) +"Hover.png", "Transport" )
        do( "Drag: Id="+str( id) + " FromX=1000 FromY=10" )
    for id in range( 11200, 11206 ):
        do( "Drag: Id="+str( id) + " FromX=10 FromY=10" )
        capture( "Button" + str(id) +"Hover.png", "Tools" )
        do( "Drag: Id="+str( id) + " FromX=1000 FromY=10" )
    for id in range( 11300, 11312 ):
        do( "Drag: Id="+str( id) + " FromX=10 FromY=10" )
        capture( "Button" + str(id) +"Hover.png", "Edit" )
        do( "Drag: Id="+str( id) + " FromX=1000 FromY=10" )
    do( 'SetPreference: Name="/GUI/Theme" Value="light"')
    #Restore default tool.
    do('SelectTool')


def oddments_imagesB():
    loadMonoTracks(1)
    #Bring window to top now
    capture( "Dummy.png", "Ruler" )
    #We hope nothing else gets focus before the next capture, so
    #that we actually get to see something!
    do( "Drag: Window=Timeline FromX=200 FromY=10 ToX=600 ToY=10" )
    time.sleep(3.0)
    #Disable bringing to top, so as not to destroy quick play.
    capture( "QuikPlay001.png", "FirstTrackPlus ToTop=0" )

oddments_imagesA()
oddments_imagesB()


