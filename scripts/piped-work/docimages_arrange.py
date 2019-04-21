# docimages_arrange.py
# Sends commands to get images for the manual.
# These ones arrange tracks and do alignment.

# Make sure Audacity is running first and that mod-script-pipe is enabled
# before running this script.

#load and run the common core.
exec( open("docimages_core.py" ).read() )

import time


def loadFourColours() :
    loadMonoTracks( 4 )
    do( 'SelectTracks: Track=0' )
    do( 'SetTrack: Name="Claire" Height=60 Color=Color0')
    do( 'SetClip: At=1 Start=25')
    do( 'SelectTracks: Track=1' )
    do( 'SetTrack: Track=1 Name="Ann" Height=60 Color=Color1')
    do( 'SetClip: At=1 Start=15')
    do( 'SelectTracks: Track=2' )
    do( 'SetTrack: Track=2 Name="Bob" Height=60 Color=Color2')
    do( 'SetClip: At=1 Start=20')
    do( 'SelectTracks: Track=3' )
    do( 'SetTrack: Track=3 Name="David" Height=60 Color=Color3')
    do( 'SetClip: At=1 Start=10')
    do( 'Select: TrackCount=100 Mode=Remove' )

def loadFourColoursSelected() :
    loadFourColours()
    do( 'ZoomOut' )
    do( 'Select: Start=90 End=135 Track=0 TrackCount=100' )

def blockMoves( name ):
    # These are the align commands that move tracks 'en block'.
    loadFourColoursSelected()
    capture( name + '001.png', 'AllTracksPlus' )
    do( 'Align_StartToZero' )
    capture( name + '002.png', 'AllTracksPlus' )
    loadFourColoursSelected()
    do( 'Align_StartToSelStart' )
    capture( name + '003.png', 'AllTracksPlus' )
    loadFourColoursSelected()
    do( 'Align_StartToSelEnd' )
    capture( name + '004.png', 'AllTracksPlus' )
    loadFourColoursSelected()
    do( 'Align_EndtoSelStart' )
    capture( name + '005.png', 'AllTracksPlus' )
    loadFourColoursSelected()
    do( 'Align_EndtoSelEnd' )
    capture( name + '006.png', 'AllTracksPlus' )

def track_moves( type ) :
    loadFourColours()
    # Sorting tracks into order
    do( 'SetTrack: Track=1 Focused=1')
    capture( 'TrackOrder002.png', 'AllTracks' )

def arrange_imagesA() :
    loadFourColours()
    # Moving tracks up and down.
    capture( 'TrackOrder001.png', 'AllTracks' )
    do( 'SetTrack: Track=1 Focused=1')
    # ToTop=0 to show the focus...
    capture( 'TrackOrder002.png', 'AllTracks ToTop=0' )
    do( 'TrackMoveUp' )
    capture( 'TrackUp.png', 'AllTracks ToTop=0' )
    do( 'TrackMoveDown' ) # undo
    do( 'TrackMoveDown' )
    capture( 'TrackDown.png', 'AllTracks ToTop=0' )
    do( 'TrackMoveTop' ) 
    capture( 'TrackTop.png', 'AllTracks ToTop=0' )
    do( 'TrackMoveBottom' ) 
    capture( 'TrackBottom.png', 'AllTracks ToTop=0' )
    # Sorting tracks into order
    do( 'SortByName')
    capture( 'TrackOrder003.png', 'AllTracks' )
    do( 'SortByTime')
    capture( 'TrackOrder004.png', 'AllTracks' )
    # Aligning tracks
    do( 'Select: First=0 Last=100 From=0 To=0')
    do( 'Align_Together' )
    capture( 'TrackAlign001.png', 'AllTracks' )
    do( 'Align_EndtoEnd' )
    do( 'FitInWindow' )
    capture( 'TrackAlign002.png', 'AllTracks' )


def arrange_imagesB() :
    blockMoves( 'BlockMoves' )
    do( 'MoveSelectionWithTracks')
    blockMoves( 'BlockAndCursorMoves' )
    do( 'MoveSelectionWithTracks')



#quickTest()

arrange_imagesA()
arrange_imagesB()



