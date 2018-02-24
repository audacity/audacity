# docimages_tracks.py
# Sends commands to get images for the manual.
# Images for https://alphamanual.audacityteam.org/man/Audio_Tracks

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
    path = 'C:\\Users\\James Crook\\'
    sample ='C:\\Users\\James Crook\\Music\\The Poodle Podcast.wav'
    do( 'SetProject: X=10 Y=10 Width=850 Height=800' )

def makeWayForTracks(  ) :
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
    
def loadMonoTracks( num ) :
    makeWayForTracks( )
    loadMonoTrack()
    do( 'SetTrack: Track=0 Name="Foxy Lady"')
    for i in range( 0, num-1 ):
        do( 'Duplicate' )
    do( 'FitInWindow' )
    do( 'Select: Start=55 End=70')

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

# A mono track complete with ruler
def image1() :
    loadMonoTracks(1)
    capture( 'AutoTracks001.png', 'First_Track_Plus' )

# A stereo track, with its name on the track    
def image2() :
    makeStereoTracks(1)
    capture( 'AutoTracks002.png', 'First_Track' )

# Four colours of track
def image3() :
    loadMonoTracks( 4 )
    do( 'SetTrack: Track=0 Name="Instrument 1" Height=122 Color=Color0')
    do( 'SetTrack: Track=1 Name="Instrument 2" Height=122 Color=Color1')
    do( 'SetTrack: Track=2 Name="Instrument 3" Height=122 Color=Color2')
    do( 'SetTrack: Track=3 Name="Instrument 4" Height=122 Color=Color3')
    capture( 'AutoTracks003.png', 'First_Four_Tracks' )

# Two Tracks, ready to make stereo
def image4():
    loadMonoTracks(2)
    do( 'SetTrack: Track=0 Name="Left Track" Height=80')
    do( 'SetTrack: Track=1 Name="Right Track" Height=80')
    capture( 'AutoTracks004.png', 'First_Two_Tracks' )

# Mono tracks made stereo    
def image5():
    loadMonoTracks(2)
    do( 'SetTrack: Track=0 Pan=-1 Height=80')
    do( 'SetTrack: Track=1 Pan=1 Height=80')
    do( 'MixAndRender' )
    do( 'SetTrack: Track=0 Name="Combined" Height=80')
    do( 'SetTrack: Track=1 Height=80')
    do( 'Select: First=0 Last=1' )
    capture( 'AutoTracks005.png', 'First_Track' )

# A stereo track, with different sized channels
def image6() :
    makeStereoTracks(1)
    do( 'SetTrack: Track=0 Height=80')
    do( 'SetTrack: Track=1 Height=180')
    capture( 'AutoTracks006.png', 'First_Track' )

# Two mono tracks of different sizes
def image7() :
    loadMonoTracks(2)
    do( 'SetTrack: Track=0 Height=180')
    do( 'SetTrack: Track=1 Height=80')
    capture( 'AutoTracks007.png', 'First_Two_Tracks' )

# Mono with arrow at start.
def image8() :
    loadMonoTracks(1)
    do( 'SetClip: Clip=0 Start=-4.0')
    capture( 'AutoTracks008.png', 'First_Track' )

# Zoomed in to show points stem-plot
def image9() :
    #make rather than load.  We want an artificial track.
    makeMonoTracks(1)
    do( 'Select: Start=0 End=0.003' )
    do( 'ZoomSel' );
    do( 'Amplify: Ratio=3.0' )
    do( 'SetPreference: Name=/GUI/SampleView Value=1 Reload=1')
    capture( 'AutoTracks009.png', 'First_Track' )

# Zoomed in to show points stem-plot and then no stem plot
def image9and10() :
    image9()
    do( 'SetPreference: Name=/GUI/SampleView Value=0 Reload=1')
    capture( 'AutoTracks010.png', 'First_Track' )
    
#quickTest()
setup()
image1()
image2()
image3()
image4()
image5()
image6()      
image7()
image8()
image9and10()

