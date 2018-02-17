# docimages_spectro.py
# Sends commands to get images for the manual.
# Images for https://alphamanual.audacityteam.org/man/Spectrogram_View

# Make sure Audacity is running first and that mod-script-pipe is enabled
# before running this script.

#load and run the common core.
exec( open("docimages_core.py" ).read() )



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
  

def spectro_imagesA() :
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

def spectro_imagesB():
    makeStepper();
    # Stepper tone, viewed in dB.
    do( 'SetTrack: Scale=dB')
    capture( 'Spectral003.png', 'First_Two_Tracks' )
    # As spectrogram.
    do( 'SetTrack: Display=Spectrogram')
    capture( 'Spectral004.png', 'First_Two_Tracks' )
    
#quickTest()

spectro_imagesA()
spectro_imagesB()

