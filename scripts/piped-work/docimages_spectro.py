# docimages_spectro.py
# Sends commands to get images for the manual.
# Images for https://alphamanual.audacityteam.org/man/Spectrogram_View

# Make sure Audacity is running first and that mod-script-pipe is enabled
# before running this script.

#load and run the common core.
exec( open("docimages_core.py" ).read() )

import math
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
    do( 'NewLabelTrack' )
    for i in range( 0, 11 ):
        do( 'Select: Track=1 Start='+str(i*2)+' End='+str(i*2+2) )
        do( 'AddLabel' )
        do( 'SetLabel: Label=' + str(i)+' Selected=0 Text='+str( -(i*10) ))
    do( 'Select: TrackCount=0 Start=0 End=0')
  

def spectro_imagesA() :
    loadStereoTracks(1)
    # A stereo track
    capture( 'Spectral001.png', 'FirstTrack' )
    # As spectrogram.
    do( 'Select' );
    do( 'SetTrack: Display=Spectrogram')
    do( 'Select: Start=55 End=70')
    capture( 'Spectral002.png', 'AllTracks' )
    # Half spectrogram, half wave.
    do( 'Select: Track=0.5 TrackCount=0.5' );
    do( 'SetTrack: Display=Waveform')
    capture( 'MixedMode.png', 'AllTracks' )

def spectro_imagesB():
    makeStepper();
    # Stepper tone, viewed in dB.
    do( 'Select: Start=0 End=0' );
    do( 'SetTrack: Scale=dB')
    capture( 'Spectral003.png', 'AllTracks' )
    # As spectrogram.
    do( 'SetTrack: Display=Spectrogram')
    capture( 'Spectral004.png', 'AllTracks' )

def spectro_imagesC():
    # A chirp and the word 'Audacity'
    loadExample( 'AudacitySpectral.wav' )
    do( 'Select: Start=0 End=0' );
    capture( 'Spectral005.png', 'AllTracks' )
    do( 'SetTrack: Scale=dB')
    capture( 'Spectral006.png', 'AllTracks' )
    do( 'SetTrack: Display=Spectrogram')
    capture( 'Spectral007.png', 'AllTracks' )
    do( 'Select: Start=1.5 End=2.1 Low=3000 High=6000') 
    capture( 'Spectral008.png', 'AllTracks' )
    do( 'Select: Start=1.1 End=2.5' )
    do( 'ZoomSel' )
    do( 'Select: Start=1.5 End=2.1 Low=3000 High=6000')
    do( 'SetTrack: Height=400' )
    multiWindow( "SpectralVocal" )

def setWindow( name, value ):
    do( 'Select: Start=0 End=0' );
    do( 'SetTrack: SpecPrefs=1 Name="Window Size '+value+'"' )
    do( 'SetPreference: Name="/Spectrum/FFTSize" Reload=1 Value='+value )
    do( 'SetTrack: Display=Spectrogram' )
    do( 'Select: TrackCount=0 Start=0 End=0' );
    capture( name + postfix + value + '.png', 'AllTracks' )


def multiWindow( name ) :
    setWindow( name, "256" )
    setWindow( name, "512" )
    setWindow( name, "2048" )
    setWindow( name, "4096" )
    setWindow( name, "8192" )
    setWindow( name, "1024" ) # done last so we restore the default.
    

def spectro_imagesD():
    makeWayForTracks()
    do( 'NewMonoTrack' )
    do( 'Select: Start=0.7 End=1.3')
    do( 'Silence' ) #Just so we can watch.
    do( 'Select: Start=0.8 End=1.2')
    do( 'ZoomSel')
    do( 'Select: Start=0.7 End=1.3')
    do( 'Tone: Frequency=3000 Amplitude=0.8' )
    do( 'Select: Start=0.99 End=0.99005' )
    do( 'Tone: Frequency=12000 Amplitude=0.9' )
    do( 'Select: Start=1.01 End=1.01005' )
    do( 'Tone: Frequency=12000 Amplitude=0.9' )
    do( 'Select: Start=0 End=0' )
    multiWindow( 'SpectralAt' )

def spectro_imagesE():
    makeWayForTracks()
    do( 'NewMonoTrack' )
    do( 'Select: Start=0 End=1.2')
    do( 'ZoomSel')
    do( 'Pluck' )
    do( 'Select: Start=0.1 End=1.0')
    do( 'ZoomSel')
    multiWindow( 'SpectralNote' )

def makeScale( start, end, count ) :   
    a = math.exp( math.log( end/start) /(count-1 ))
    makeWayForTracks()
    do( 'NewMonoTrack' )
    do( 'Select: Start=0 End=' + str( count/10 ))
    do( 'Silence' ) #To see it happen...
    do( 'SetTrack: Track=0 SpecPrefs=1 Display=Spectrogram' )
    do( 'ZoomSel' )
    #do( 'SetTrack: Track=0 Display=Spectrogram' )
    for i in range( 0 , count , 2 ):
        note = start * ( a ** i )
        #print( note )
        do( 'Select: Start=' + str( i / 10) + ' End=' + str( (i+1)/10 ))
        do( 'Tone: Frequency=' +str( note ) )
        do( 'Select: Start=' + str( i / 10) + ' End=' + str( ( i / 10) +0.05))
        do( 'FadeIn' )
        do( 'Select: Start=' + str( (i+1) / 10 -0.05) + ' End=' + str( (i+1) / 10 ))
        do( 'FadeOut' )
    do( 'FitInWindow' )
    #do( 'Select: Start=0 End=' + str( count/10 ))
    #do( 'Join' )
    do( 'Select: TrackCount=0 Start=0 End=0')
        

def spectro_imagesF():
    makeScale( 200, 4000, 100 )
    do( 'Select: Start=0 End=0' )
    do( 'SetTrack: Display=Spectrogram' )
    capture( 'ScaleLin.png', 'AllTracks' )
    do( 'SetPreference: Name=/Spectrum/ScaleType Value=1 Reload=1')
    capture( 'ScaleLog.png', 'AllTracks' )
    do( 'SetPreference: Name=/Spectrum/ScaleType Value=0 Reload=1')
    
        
#quickTest()

do( 'SetPreference: Name="/GUI/Theme" Value=light Reload=1' )


postfix = ''
spectro_imagesA()
spectro_imagesB()
spectro_imagesC()
spectro_imagesD()
spectro_imagesE()
spectro_imagesF()

do( 'SetPreference: Name="/GUI/Theme" Value=dark Reload=1' )

postfix = 'Dark'
spectro_imagesA()
spectro_imagesB()
spectro_imagesC()
spectro_imagesD()
spectro_imagesE()
spectro_imagesF()

postfix = ''
do( 'SetPreference: Name="/GUI/Theme" Value=light Reload=1' )
