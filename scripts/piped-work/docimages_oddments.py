# docimages_oddments.py
# Sends commands to get images for the manual.
# Image oddments that don't fit the other categories.

# Make sure Audacity is running first and that mod-script-pipe is enabled
# before running this script.

#load and run the common core.
exec( open("docimages_core.py" ).read() )

import time


# 11 2KHz tones, of decreasing amplitude.
# With label track to annotate it.
def makeStepper():
    makeWayForTracks()
    do( 'NewMonoTrack' )
    do( 'Select: Start=0 End=22')
    do( 'Silence' ) #Just so we can watch.
    do( 'FitInWindow')
    for i in range( 0, 11 ):
        do( 'Select: Start='+str(i*2)+' End='+str(i*2+2) )
        do( 'Chirp: StartFreq=2000 EndFreq=2000 StartAmp=' + str( (400**(0.1 * (10-i)))/400 )+' EndAmp=' + str( (400**(0.1 * (10-i) ))/400 ))
    do( 'Select: Start=0 End=22')
    do( 'Join' )
    do( 'FitInWindow')
    do( 'AddLabelTrack' )
    for i in range( 0, 11 ):
        do( 'Select: Start='+str(i*2)+' End='+str(i*2+2) )
        do( 'AddLabel' )
        do( 'SetLabel: Label=' + str(i)+' Selected=0 Text='+str( -(i*10) ))
    do( 'Select: Start=0 End=0')
  

def spectro_image1and2() :
    loadStereoTracks(1)
    # A stereo track
    capture( 'Spectral001.png', 'First_Track' )
    # As spectrogram.
    do( 'SetTrack: Track=0 Display=Spectrogram')
    do( 'Select: Start=55 End=70 First=0 Last=1')
    capture( 'Spectral002.png', 'First_Track' )
    # Half spectrogram, half wave.
    do( 'SetTrack: Channel=1 Display=Waveform')
    capture( 'MixedMode.png', 'First_Track' )

def spectro_image3and4():
    makeStepper();
    # Stepper tone, viewed in dB.
    do( 'SetTrack: Scale=dB')
    capture( 'Spectral003.png', 'First_Two_Tracks' )
    # As spectrogram.
    do( 'SetTrack: Display=Spectrogram')
    capture( 'Spectral004.png', 'First_Two_Tracks' )

def oddments_imagesA():
    for name in ["Select","Envelope","Draw","Zoom","TimeShift","Multi"] :
        do( name + "Tool" )
        capture( name + "Tool.png" , 'Tools' );
    #A track is needed for the buttons to be active.
    loadMonoTracks(1)
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


def oddments_imagesB():
    loadMonoTracks(1)
    #Bring window to top now
    capture( "Dummy.png", "Ruler" )
    #We hope nothing else gets focus before the next capture, so
    #that we actually get to see something!
    do( "Drag: Window=Timeline FromX=200 FromY=10 ToX=600 ToY=10" )
    time.sleep(3.0)
    #Disable bringing to top, so as not to destroy quick play.
    capture( "QuikPlay001.png", "First_Track_Plus ToTop=0" )

oddments_imagesA()
oddments_imagesB()

