# get-gui-structure.py
# Obtains all menus and all box locations from the currently running Audacity.
# This is useful for wit.audacityteam.org, where we need these lists when we
# draw the menus and create the image maps.

# Make sure Audacity is running first and that mod-script-pipe is enabled
# before running this script.

import os
import sys


if( sys.platform  == 'win32' ):
    print( "get-gui-structure.py, running on windows" )
    toname = '\\\\.\\pipe\\ToSrvPipe'
    fromname = '\\\\.\\pipe\\FromSrvPipe'
    EOL = '\r\n\0'
else:
    print( "get-gui-structure.py, running on linux or mac" )
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
    return result

def doCommand( command ) :
    sendCommand( command )
    response = getResponse()
    print( "Rcvd: <<< " + response )
    return response

def do( command ) :
    doCommand( command )


def getStructure() :
    #do( 'Help: CommandName=Help' )
    #do( 'Help: CommandName=SetPreference' )
    #do( 'SetPreference: PrefName=GUI/Theme PrefValue=light' )
    do( 'GetInfo: Type=Boxes' )
    do( 'GetInfo: Type=Menus' )

getStructure()
