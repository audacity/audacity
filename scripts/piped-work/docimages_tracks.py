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
    do( 'SetProject: X=10 Y=10 Width=1000 Height=800' )

def makeMonoTrack() :    
    do( 'SelectTime: StartTime=0 EndTime=30' )
    do( 'Chirp: StartAmp=0.5' )
    do( 'SelectTracks' )
    do( 'Wahwah' )
    do( 'FitInWindow' )
    do( 'SetTrack: TrackIndex=0 Name="Foxy Lady"')
 
def makeStereoTrack() :
    do( 'SetProject: X=10 Y=10 Width=1000 Height=800' )
    do( 'SelectTime: StartTime=0 EndTime=30' )
    do( 'NewStereoTrack' )
    do( 'Chirp: StartAmp=0.5' )
#    do( 'SelectTracks: FirstTrack=0 LastTrack=1' )
    do( 'Wahwah' )
    do( 'FitInWindow' )
    do( 'SetTrack: TrackIndex=0 Name="Voodoo Children IN STEREO"')

def closeTrack( ):
    do( 'SetTrack: TrackIndex=0 Focuseed=True' )
    do( 'TrackClose' )

def image1() :
    global path
    makeMonoTrack()
    do( 'SelectTime: StartTime=11 EndTime=14')
    do( 'Screenshot: Path='+path+' CaptureWhat=First_Track_Plus' )
    
def image2() :
    global path
    makeStereoTrack()
    do( 'SelectTime: StartTime=11 EndTime=14')
    do( 'Screenshot: Path='+path+' CaptureWhat=First_Track_Plus' )


#quickTest()
setup()
image1()
closeTrack()
image2()
closeTrack()
        



