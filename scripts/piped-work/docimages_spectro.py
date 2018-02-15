# docimages_spectro.py
# Sends commands to get images for the manual.
# Images for https://alphamanual.audacityteam.org/man/Spectrogram_View

# Make sure Audacity is running first and that mod-script-pipe is enabled
# before running this script.

import os
import sys


if( sys.platform  == 'win32' ):
    print( "pipe-test.py, running on windows" )
    toname = '\\\\.\\pipe\\ToSrvPipe'
    fromname = '\\\\.\\pipe\\FromSrvPipe'
    EOL = '\r\n\0'
else:
    print( "pipe-test.py, running on linux or mac" )
    toname = '/tmp/audacity_script_pipe.to.' + str(os.getuid())
    fromname = '/tmp/audacity_script_pipe.from.' + str(os.getuid())
    EOL = '\n'

print( "Write to  \"" + toname +"\"" )
if not os.path.exists( toname ) :
   print( " ..does not exist.  Ensure Audacity is running with mod-script-pipe." )
   sys.exit();
    
print( "Read from \"" + fromname +"\"")
if not os.path.exists( fromname ) :
   print( " ..does not exist.  Ensure Audacity is running with mod-script-pipe." )
   sys.exit();

print( "-- Both pipes exist.  Good." )

tofile = open( toname, 'wt+' )
print( "-- File to write to has been opened" )
fromfile = open( fromname, 'rt')
print( "-- File to read from has now been opened too\r\n" )


def sendCommand( command ) :
    print( "Send: >>> \n"+command )
    tofile.write( command + EOL )	
    tofile.flush()

def getResponse() :
    result = ''
    line = ''
    while line != '\n' :
        result += line
        line = fromfile.readline()
	#print(" I read line:["+line+"]")
    return result

def doCommand( command ) :
    sendCommand( command )
    response = getResponse()
    print( "Rcvd: <<< \n" + response )
    return response

def do( command ) :
    doCommand( command )

def quickTest() :
    do( 'Help: Command=Help' )

def setup() :
    global path
    global sample
    global sample2
    path = 'C:\\Users\\James Crook\\'
    sample ='C:\\Users\\James Crook\\Music\\The Poodle Podcast.wav'
    sample2 ='C:\\Users\\James Crook\\Music\\PoodlePodStereo.wav'
    do( 'SetProject: X=10 Y=10 Width=850 Height=800' )

def makeWayForTracks(  ) :
    # Twice to remove stereo tracks.
    do( 'SelectTracks: First=0 Last=20' )
    do( 'RemoveTracks' )
    do( 'SelectTracks: First=0 Last=20' )
    do( 'RemoveTracks' )

def capture( name, what ) :
    global path
    do( 'Screenshot: Path="'+path+name+'" CaptureWhat=' + what )

def loadMonoTrack():
    global sample
    makeWayForTracks( )
    do( 'Import2: Filename="'+sample+'"' )
    do( 'Select: First=0 Last=0 Start=0 End=150')
    do( 'Trim')
    do( 'ZoomSel' )

def loadStereoTrack():
    global sample2
    makeWayForTracks( )
    do( 'Import2: Filename="'+sample2+'"' )
    do( 'Select: First=0 Last=0 Start=0 End=150')
    do( 'Trim')
    do( 'ZoomSel' )
    
def loadMonoTracks( num ) :
    makeWayForTracks( )
    loadMonoTrack()
    do( 'SetTrack: Track=0 Name="Foxy Lady"')
    for i in range( 0, num-1 ):
        do( 'Duplicate' )
    do( 'FitInWindow' )
    do( 'Select: Start=55 End=70')

def loadStereoTracks( num ) :
    makeWayForTracks( )
    loadStereoTrack()
    do( 'SetTrack: Track=0 Name="Foxy Lady"')
    for i in range( 0, num-1 ):
        do( 'Duplicate' )
    do( 'FitInWindow' )
    do( 'Select: Start=55 End=70 First=0 Last=' + str(num*2-1) )

def makeMonoTracks( num ) :
    makeWayForTracks( )
    for i in range( 0, num ):
        do( 'NewMonoTrack' )
    do( 'SetTrack: Track=0 Name="Foxy Lady"')
    do( 'Select: Start=0 End=150 First=0 Last=' + str(num-1) )
    do( 'Chirp: StartAmp=0.5' )
    do( 'Wahwah' )
    do( 'FitInWindow' )
    do( 'Select: Start=55 End=70')
 
def makeStereoTracks( num ) :
    makeWayForTracks( )
    for i in range( 0, num ):
       do( 'NewStereoTrack' )
    do( 'SetTrack: Track=0 Name="Voodoo Children IN STEREO"')
    do( 'Select: Start=0 End=150 First=0 Last=' + str(num*2-1) )
    do( 'Chirp: StartAmp=0.5' )
    do( 'Wahwah' )
    do( 'FitInWindow' )
    do( 'Select: Start=55 End=70')

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
  

def image1and2() :
    loadStereoTracks(1)
    # A stereo track
    capture( 'Spectral001.png', 'First_Track' )
    # As spectrogram.
    do( 'SetTrack: Track=0 Display=Spectrogram')
    do( 'SetTrack: Track=1 Display=Spectrogram')
    do( 'Select: Start=55 End=70 First=0 Last=1')
    capture( 'Spectral002.png', 'First_Track' )

def image3and4():
    makeStepper();
    # Stepper tone, viewed in dB.
    do( 'SetTrack: Scale=dB')
    capture( 'Spectral003.png', 'First_Two_Tracks' )
    # As spectrogram.
    do( 'SetTrack: Display=Spectrogram')
    capture( 'Spectral004.png', 'First_Two_Tracks' )
    
#quickTest()
setup()
image1and2()
image3and4()

