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
    global sample
    path = '\"C:/Users/James Crook/\"'
    sample ='\"C:\\Users\\Public\\Music\\Sample Music\\Concerto.mp3\"'
    do( 'SetProject: X=10 Y=10 Width=850 Height=800' )

def makeWayForTracks(  ) :
    do( 'Select: First=0 Last=20' )
    do( 'RemoveTracks' )


def capture( name, what ) :
    global path
    do( 'Screenshot: Path='+path+name+' CaptureWhat=' + what )

def makeMonoTracks( num ) :
    makeWayForTracks( )
    for i in range( 0, num ):
        do( 'NewMonoTrack' )
    do( 'SetTrack: Track=0 Name="Foxy Lady"')
    do( 'Select: Start=0 End=30 First=0 Last=' + str(num-1) )
    do( 'Chirp: StartAmp=0.5' )
    do( 'Wahwah' )
    do( 'FitInWindow' )
    do( 'Select: Start=11 End=14')

def loadStereoTrack():
    global sample
    makeWayForTracks( )
    do( 'Import2: Filename='+sample )
    do( 'Select: Start=0 End=150')
    do( 'ZoomSel' )
    do( 'SetTrack: Track=0 Name="The Poodle Podcast"')
 
def makeStereoTracks( num ) :
    makeWayForTracks( )
    if( num == 1 ):
        loadStereoTrack()
        return
    for i in range( 0, num ):
       do( 'NewStereoTrack' )
    do( 'SetTrack: Track=0 Name="Voodoo Children IN STEREO"')
    do( 'Select: Start=0 End=30 First=0 Last=' + str(num*2-1) )
    do( 'Chirp: StartAmp=0.5' )
    do( 'Wahwah' )
    do( 'FitInWindow' )
    do( 'Select: Start=55 End=70')

def addLabels():
    do( 'Select: Start=0 End=1' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'SetLabel: Label=0 Text="Homweward Bound" Start=1 End=1 ')
    do( 'SetLabel: Label=1 Text="Silver Dagger" Start=35 End=35 ')
    do( 'SetLabel: Label=2 Selected=1 Text="NOISE" Start=45 End=60 ')
    do( 'SetLabel: Label=3 Text="Blood in These Veins" Start=80 End=80 ')
    do( 'Select: Start=45 End=60' )

def addLabels2():
    do( 'Select: Start=0 End=1' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'NewLabelTrack' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'SetLabel: Label=0 Selected=0 Text="intro" Start=0.5 End=0.5 ')
    do( 'SetLabel: Label=1 Text="thought piece" Start=15 End=15 ')
    do( 'SetLabel: Label=2 Text="discussion" Start=60 End=60 ')
    do( 'SetLabel: Label=3 Text="summary" Start=100 End=100 ')
    do( 'SetLabel: Label=4 Text="credits" Start=125 End=125 ')
    do( 'SetLabel: Label=5 Selected=0 Text="Bach" Start=0.5 End=0.5 ')
    do( 'SetLabel: Label=6 Text="Vivaldi" Start=30 End=30 ')
    do( 'SetLabel: Label=7 Text="Mozart" Start=60 End=60 ')
    do( 'SetLabel: Label=8 Text="Satie" Start=90 End=90 ')
    do( 'SetLabel: Label=9 Text="Chopin" Start=120 End=120 ')
    do( 'Select: First=3 Last=3' )


# A stero track with four labels.
def image1() :
    makeStereoTracks(1)
    addLabels()
    do( "Select: Start=0 End=0" )
    capture( 'AutoLabels001.png', 'First_Two_Tracks' )

# Removing a label with delete (fraud - we used split delete)
def image2() :
    makeStereoTracks(1)
    addLabels()
    do( "Select: Start=44.5 End=60.5 First=2 Last=2" )
    do( "SplitDelete" )
    do( "Select: Start=0 End=0 First=0 Last=2" )
    capture( 'AutoLabels002.png','First_Two_Tracks' )

# Removing a label with split-delete step 1
def image3() :
    makeStereoTracks(1)
    addLabels()
    do( "Select: Start=44.5 End=60.5 First=2 Last=2" )
    capture( 'AutoLabels003.png','First_Two_Tracks' )

# Removing a label with split-delete step 1
def image4() :
    makeStereoTracks(1)
    addLabels()
    do( "Select: Start=44.5 End=60.5 First=2 Last=2" )
    do( "SplitDelete" )
    capture( 'AutoLabels004.png','First_Two_Tracks' )

def image5() :
    makeStereoTracks(1)
    addLabels2()
    do( "Select: Start=0 End=0" )
    capture( 'AutoLabels005.png','First_Three_Tracks' )

def image6() :
    makeStereoTracks(1)
    addLabels2()
    do( "Select: Start=28.5 End=58.5" )
    capture( 'AutoLabels006.png','First_Three_Tracks' )

def image7() :
    makeStereoTracks(1)
    addLabels2()
    do( "Select: Start=28.5 End=58.5" )
    do( "Delete" )
    do( "Select: Start=0 End=0" )
    capture( 'AutoLabels007.png','First_Three_Tracks' )

def image8to10() :
    makeStereoTracks(1)
    addLabels2()
    do( "Select: First=2 Last=2 Start=100 End=125" )
    do( "AddLabel" )
    do( 'SetLabel: Label=9 Text="Clap" selected=0 Start=110 End=118 ')
    do( 'Select: First=0 Last=3 Start=0 End=0')
    capture( 'AutoLabels008.png','First_Three_Tracks' )
    do( 'SetLabel: Label=9 Text="Clap" selected=1 Start=110 End=118 ')
    do( 'Select: First=0 Last=3 Start=110 End=118')
    capture( 'AutoLabels009.png','First_Three_Tracks' )
    do( 'Delete' )
    capture( 'AutoLabels010.png','First_Three_Tracks' )
    
#quickTest()
setup()
#image1()
#image2()
#image3()
#image4()
#image5()
#image6()
#image7()
image8to10()
