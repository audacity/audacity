# recording-test.py
# loads testfile-in.wav, plays it, recording at the same time, and then 
# exports the result as testfile-out.wav. 
# Suitable for rinse and repeat with different input files.
# Assumes testfile-in.wav is one minute of audio.

# Make sure Audacity is running first and that mod-script-pipe is enabled
# before running this script.

import os
import sys
import time

if( sys.platform  == 'win32' ):
    print( "recording-test.py, running on windows" )
    toname = '\\\\.\\pipe\\ToSrvPipe'
    fromname = '\\\\.\\pipe\\FromSrvPipe'
    EOL = '\r\n\0'
else:
    print( "recording-test.py, running on linux or mac" )
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
    do( 'Help: CommandName=Help' )

# You will need to modify the paths in this function and the next.
def playRecord( name ) :
    do("Import: Filename='C:\\Users\\James Crook\\Documents\\Audacity\\" + name + ".wav'")
    do("Select: Track=0")
    do("SelectTrackStartToEnd")
    do("MenuCommand: CommandName=Record2ndChoice")

def exportIt( name ):
    do("Select: Track=1")
    do("SelectTrackStartToEnd")
    do("Export: Filename='C:\\Users\\James Crook\\Documents\\Audacity\\" + name + ".wav' Mode=Selection Channels=1.0")
    do("Select: Track=0")
    do("SelectTrackStartToEnd")
    do("MenuCommand: CommandName=RemoveTracks")

def doOneFile( name ):
    playRecord(name + "-in")
    time.sleep( 65 )
    exportIt(name + "-out")
    

quickTest()
doOneFile("testfile")

