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
    print( "Send: >>> "+command )
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
    print( "Rcvd: <<< " + response )
    return response

def do( command ) :
    doCommand( command )

def quickTest() :
    do( 'Help: Command=Help' )
    do( 'Help: Command="SetTrackInfo"' )
    do( 'SetPreference: Name=GUI/Theme Value=light Reload=false' )


def setup() :
    global path
    path = '\"C:/Users/James Crook/\"'
    do( 'SetProject: X=10 Y=10 Width=850 Height=800' )

def makeWayForTracks(  ) :
    do( 'SelectTracks: FirstTrack=0 LastTrack=20' )
    do( 'RemoveTracks' )


def makeMonoTracks( num ) :
    makeWayForTracks( )
    for i in range( 0, num ):
        do( 'NewMonoTrack' )
    do( 'SetTrack: TrackIndex=0 Name="Foxy Lady"')
    do( 'SelectTime: StartTime=0 EndTime=30' )
    do( 'SelectTracks: FirstTrack=0 LastTrack=' + str(num-1) )
    do( 'Chirp: StartAmp=0.5' )
    do( 'Wahwah' )
    do( 'FitInWindow' )
    do( 'SelectTime: StartTime=11 EndTime=14')
 
def makeStereoTracks( num ) :
    makeWayForTracks( )
    for i in range( 0, num ):
       do( 'NewStereoTrack' )
    do( 'SetTrack: TrackIndex=0 Name="Voodoo Children IN STEREO"')
    do( 'SelectTime: StartTime=0 EndTime=30' )
    do( 'SelectTracks: FirstTrack=0 LastTrack=' + str(num*2-1) )
    do( 'Chirp: StartAmp=0.5' )
    do( 'Wahwah' )
    do( 'FitInWindow' )
    do( 'SelectTime: StartTime=11 EndTime=14')

# A mono track complete with ruler
def image1() :
    global path
    makeMonoTracks(1)
    do( 'Screenshot: Path='+path+' CaptureWhat=First_Track_Plus' )

# A stereo track, with its name on the track    
def image2() :
    global path
    makeStereoTracks(1)
    do( 'Screenshot: Path='+path+' CaptureWhat=First_Track' )

# Four colours of track
def image3() :
    global path
    makeMonoTracks( 4 )
    do( 'SetTrack: TrackIndex=0 Name="Instrument 1" Height=122 Color=Color0')
    do( 'SetTrack: TrackIndex=1 Name="Instrument 2" Height=122 Color=Color1')
    do( 'SetTrack: TrackIndex=2 Name="Instrument 3" Height=122 Color=Color2')
    do( 'SetTrack: TrackIndex=3 Name="Instrument 4" Height=122 Color=Color3')
    do( 'Screenshot: Path='+path+' CaptureWhat=Tracks' )

# Two Tracks, ready to make stereo
def image4():
    global path
    makeMonoTracks(2)
    do( 'SetTrack: TrackIndex=0 Name="Left Track" Height=80')
    do( 'SetTrack: TrackIndex=1 Name="Right Track" Height=80')
    do( 'Screenshot: Path='+path+' CaptureWhat=Tracks' )

# Mono tracks made stereo    
def image5():
    global path
    makeMonoTracks(2)
    do( 'SetTrack: TrackIndex=0 Pan=-1 Height=80')
    do( 'SetTrack: TrackIndex=1 Pan=1 Height=80')
    do( 'MixAndRender' )
    do( 'SetTrack: TrackIndex=0 Name="Combined" Height=80')
    do( 'SetTrack: TrackIndex=1 Height=80')
    do( 'SelectTracks: FirstTrack=0 LastTrack=1' )
    do( 'Screenshot: Path='+path+' CaptureWhat=First_Track' )

# A stereo track, with different sized channels
def image6() :
    global path
    makeStereoTracks(1)
    do( 'SetTrack: TrackIndex=0 Height=80')
    do( 'SetTrack: TrackIndex=1 Height=180')
    do( 'Screenshot: Path='+path+' CaptureWhat=First_Track' )

# Two mono tracks of different sizes
def image7() :
    global path
    makeMonoTracks(2)
    do( 'SetTrack: TrackIndex=0 Height=80')
    do( 'SetTrack: TrackIndex=1 Height=180')
    do( 'Screenshot: Path='+path+' CaptureWhat=Tracks' )

# Mono with arrow at start.
def image8() :
    global path
    makeMonoTracks(1)
    do( 'SetClip: ClipIndex=0 Start=-4.0')
    do( 'Screenshot: Path='+path+' CaptureWhat=First_Track' )

# Zoomed in to show points stem-plot
def image9() :
    global path
    do( 'SetPreference: Name=/GUI/SampleView Value=1 Reload=True')
    makeMonoTracks(1)
    do( 'SelectTime: StartTime=0 EndTime=0.003' )
    do( 'ZoomSel' );
    do( 'Amplify: Ratio=3.0' )
    do( 'Screenshot: Path='+path+' CaptureWhat=First_Track' )

# Zoomed in to show points no stem plot
def image10() :
    global path
    do( 'SetPreference: Name=/GUI/SampleView Value=0 Reload=True')
    makeMonoTracks(1)
    do( 'SelectTime: StartTime=0 EndTime=0.003' )
    do( 'ZoomSel' );
    do( 'Amplify: Ratio=3.0' )
    do( 'Screenshot: Path='+path+' CaptureWhat=First_Track' )
    


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
image9()
image10()

