# docimages_core.py
# Sends commands to get images for the manual.
# This is the shared part reused in a bunch of scripts.

# Make sure Audacity is running first and that mod-script-pipe is enabled
# before running this script.

import os
import sys

def startPipes() :
    global tofile
    global fromfile
    global EOL
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

    tofile = open( toname, 'wt' )
    print( "-- File to write to has been opened" )
    fromfile = open( fromname, 'rt')
    print( "-- File to read from has now been opened too\r\n" )


def sendCommand( command ) :
    global tofile
    global EOL
    print( "Send: >>> \n"+command )
    tofile.write( command + EOL )	
    tofile.flush()

def getResponse() :
    """Return the command response."""
    global fromfile
    result = ''
    line = ''
    while True:
        result += line
        line = fromfile.readline()
        if line == '\n' and len(result) > 0:
            break
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
    global sample_path
    global result_path
    global sample
    global sample2
    global postfix
    postfix = ''
    path = os.path.abspath(__file__);
    
    path = os.path.dirname( path )
    path = os.path.dirname( path )
    path = os.path.dirname( path )
    path = os.path.join( path, 'tests' )
    result_path = os.path.join( path, 'results' )
    sample_path = os.path.join( path, 'samples' )
    sample      = os.path.join( sample_path, 'FifeAndDrums.wav' )
    sample2     = os.path.join( sample_path, 'FifeAndDrumsStereo.wav' )
   
    startPipes()
    do( 'SetProject: X=10 Y=10 Width=910 Height=800' )

def imageSet(name):
    print("****************** " + name + " ***************************")

def makeWayForTracks(  ) :
    do( 'Select: TrackCount=20' )
    do( 'RemoveTracks' )

def capture( name, what ) :
    global result_path
    global postfix
    name = name.split( '.png' )[0] + postfix + '.png' 
    do( 'Screenshot: Path="'+os.path.join( result_path, name) +'" CaptureWhat=' + what )

def loadExample( name ):
    global sample_path
    makeWayForTracks( )
    do( 'Import2: Filename="'+os.path.join( sample_path, name)+'"' )
    do( 'Select: Start=0 End=0')
    do( 'FitInWindow' )

def loadMonoTrack():
    global sample
    makeWayForTracks( )
    do( 'Import2: Filename="'+sample+'"' )
    do( 'Select: Start=0 End=150')
    do( 'Trim')
    do( 'ZoomSel' )

def loadStereoTrack():
    global sample2
    makeWayForTracks( )
    do( 'Import2: Filename="'+sample2+'"' )
    do( 'Select: Start=0 End=150')
    do( 'Trim')
    do( 'ZoomSel' )
    
def loadMonoTracks( num ) :
    makeWayForTracks( )
    loadMonoTrack()
    do( 'Select: Track=0')
    do( 'SetTrack: Name="Foxy Lady"')
    for i in range( 0, num-1 ):
        do( 'Select')
        do( 'Duplicate' )
    do( 'FitInWindow' )
    do( 'Select: Start=55 End=70 TrackCount=' + str(num))

def loadStereoTracks( num ) :
    makeWayForTracks( )
    loadStereoTrack()
    do( 'SetTrack: Track=0 Name="Foxy Lady"')
    for i in range( 0, num-1 ):
        do( 'Select')
        do( 'Duplicate' )
    do( 'FitInWindow' )
    do( 'Select: Start=55 End=70 TrackCount=' + str(num) )

def makeMonoTracks( num ) :
    makeWayForTracks( )
    for i in range( 0, num ):
        do( 'NewMonoTrack' )
    do( 'SetTrack: Track=0 Name="Foxy Lady"')
    do( 'Select: Start=0 End=150 TrackCount=' + str(num) )
    do( 'Chirp: StartAmp=0.5' )
    do( 'Wahwah' )
    do( 'FitInWindow' )
    do( 'Select: Start=55 End=70')
 
def makeStereoTracks( num ) :
    makeWayForTracks( )
    for i in range( 0, num ):
       do( 'NewStereoTrack' )
    do( 'Select' )
    do( 'SetTrack: Name="Voodoo Children IN STEREO"')
    do( 'Select: Start=0 End=150 TrackCount=' + str(num) )
    do( 'Chirp: StartAmp=0.5' )
    do( 'Wahwah' )
    do( 'FitInWindow' )
    do( 'Select: Start=55 End=70')

try:
    coreLoaded
except NameError:    
    setup()
    coreLoaded=True
    print( "Set up done")
else :
    print( "Already set up")


