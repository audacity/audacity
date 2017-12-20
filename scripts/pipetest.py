# pipetest.py
# Tests the audacity pipe.  Sends 3 commands.
# Keep pipetest.py short!!
# You can make more complicated longer tests to test other functionality
# or to generate screenshots etc in other scripts.


toname = '\\\\.\\pipe\\ToSrvPipe'
fromname = '\\\\.\\pipe\\FromSrvPipe'

tofile = open( toname, 'wt' )
fromfile = open( fromname, 'rt')

def sendCommand( command ) :
    print( "Send: "+command )
    tofile.write( command + '\r\n\0' )
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
    print( "Rcvd: " + response )
    return response

def quickTest() :
    doCommand( 'Help: CommandName=Help' )
    doCommand( 'Help: CommandName=SetPreference' )
    doCommand( 'SetPreference: PrefName=GUI/Theme PrefValue=light' )


quickTest()
