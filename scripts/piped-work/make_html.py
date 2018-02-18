# Takes an image file directory and makes the web page that lists them.
# They are listed in creation date/time order.

import glob
import os

def getFiles() :
    files = glob.glob("C:\\OpenSourceGit\\AudacityTeamTools\\wit-html\\auto_images\\*.png")
    files.sort(key=os.path.getmtime)
    return [ os.path.basename( name ) for name in files ]
    #print("\n".join(files))

def oneItem( name ) :
    return "<img src='./auto_images/"+name+"'><br><em>"+name+"</em><br>"

files = getFiles()
print( "\n".join( [ oneItem(name) for name in files ] ) )
