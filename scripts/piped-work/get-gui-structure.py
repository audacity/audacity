# get-gui-structure.py
# Obtains all menus and all box locations from the currently running Audacity.


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

def getStructure() :
    doCommand( 'GetAllMenuCommands:' )


getStructure()
