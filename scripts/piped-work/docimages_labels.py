# docimages_labels.py
# Sends commands to get images for the manual.
# Images for https://alphamanual.audacityteam.org/man/Removing_Labels_-_Examples

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

def addLabels():
    do( 'SelectTime: StartTime=0 EndTime=1' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'SetLabel: LabelIndex=0 Text="Homweward Bound" Start=1 End=1 ')
    do( 'SetLabel: LabelIndex=1 Text="Silver Dagger" Start=7 End=7 ')
    do( 'SetLabel: LabelIndex=2 Selected=1 Text="NOISE" Start=9 End=12 ')
    do( 'SetLabel: LabelIndex=3 Text="Blood in These Veins" Start=16 End=16 ')
    do( 'SelectTime: StartTime=9 EndTime=12' )

# A mono track complete with ruler
def image1() :
    global path
    makeStereoTracks(1)
    addLabels()
    do( 'Screenshot: Path='+path+' CaptureWhat=First_Two_Tracks' )

    
#quickTest()
setup()
image1()

