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


def capture( what ) :
    global path
    do( 'Screenshot: Path='+path+' CaptureWhat=' + what )

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

def addLabels2():
    do( 'SelectTime: StartTime=0 EndTime=1' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'NewLabelTrack' )
    #do( 'SelectTracks: FirstTrack=2 LastTrack=2' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'SetLabel: LabelIndex=0 Selected=0 Text="intro" Start=0.1 End=0.1 ')
    do( 'SetLabel: LabelIndex=1 Text="thoughtpiece" Start=3 End=3 ')
    do( 'SetLabel: LabelIndex=2 Text="discussion" Start=12 End=12 ')
    do( 'SetLabel: LabelIndex=3 Text="summary" Start=20 End=20 ')
    do( 'SetLabel: LabelIndex=4 Text="credits" Start=25 End=25 ')
    do( 'SetLabel: LabelIndex=5 Selected=0 Text="Bach" Start=0.1 End=0.1 ')
    do( 'SetLabel: LabelIndex=6 Text="Vivaldi" Start=6 End=6 ')
    do( 'SetLabel: LabelIndex=7 Text="Mozart" Start=12 End=12 ')
    do( 'SetLabel: LabelIndex=8 Text="Satie" Start=18 End=18 ')
    do( 'SetLabel: LabelIndex=9 Text="Chopin" Start=24 End=24 ')
    do( 'SelectTracks: FirstTrack=3 LastTrack=3' )


# A stero track with four labels.
def image1() :
    makeStereoTracks(1)
    addLabels()
    capture( 'First_Two_Tracks' )

# Removing a label with delete (fraud - we used split delete)
def image2() :
    makeStereoTracks(1)
    addLabels()
    do( "SelectTime: StartTime=8.9 EndTime=12.1" )
    do( "SelectTracks: FirstTrack=2 LastTrack=2" )
    do( "SplitDelete" )
    do( "SelectTime: StartTime=0 EndTime=0" )
    do( "SelectTracks: FirstTrack=0 LastTrack=2" )
    capture( 'First_Two_Tracks' )

# Removing a label with split-delete step 1
def image3() :
    makeStereoTracks(1)
    addLabels()
    do( "SelectTime: StartTime=8.9 EndTime=12.1" )
    do( "SelectTracks: FirstTrack=2 LastTrack=2" )
    capture( 'First_Two_Tracks' )

# Removing a label with split-delete step 1
def image4() :
    makeStereoTracks(1)
    addLabels()
    do( "SelectTime: StartTime=8.9 EndTime=12.1" )
    do( "SelectTracks: FirstTrack=2 LastTrack=2" )
    do( "SplitDelete" )
    capture( 'First_Two_Tracks' )

def image5() :
    makeStereoTracks(1)
    addLabels2()
    do( "SelectTime: StartTime=0 EndTime=0" )
    capture( 'First_Three_Tracks' )

def image6() :
    makeStereoTracks(1)
    addLabels2()
    do( "SelectTime: StartTime=5.7 EndTime=11.7" )
    capture( 'First_Three_Tracks' )

def image7() :
    makeStereoTracks(1)
    addLabels2()
    do( "SelectTime: StartTime=5.7 EndTime=11.7" )
    do( "Delete" )
    do( "SelectTime: StartTime=0 EndTime=0" )
    capture( 'First_Three_Tracks' )
    
#quickTest()
setup()
image1()
image2()
image3()
image4()
image5()
image6()
image7()
