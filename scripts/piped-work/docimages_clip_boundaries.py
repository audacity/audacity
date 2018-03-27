# docimages_clip_boundariess.py
# Sends commands to get images for the manual.
# Images for clip boundary manipulation, per that chapter in the manual
# Make sure Audacity is running first and that mod-script-pipe is enabled
# before running this script.

#load and run the common core.
exec( open("docimages_core.py" ).read() )

def gappyTrack2() :
    loadMonoTracks(1)
    # A mono track
    do( 'Select: Start=0 End=10')
    do( 'SplitCut' )
    do( 'Select: Start=30 End=45')
    do( 'SplitCut' )
    do( 'Select: Start=90 End=100')
    do( 'SplitCut' )
    do( 'Select: Start=120 End=135')
    do( 'SplitCut' )
    do( 'Select: Start=0 End=0')

def clipb_imagesA():
    gappyTrack2()
    # clip bound left
    capture( 'ClipBounds001.png', 'AllTracks' )
    do( 'Select: Start=60 End=60')
    capture( 'ClipBounds002.png', 'AllTracks' )
    do( 'SelPrevClipBoundaryToCursor' )
    capture( 'ClipBounds003.png', 'AllTracks' )
    do( 'SelPrevClipBoundaryToCursor' )
    capture( 'ClipBounds004.png', 'AllTracks' )
    do( 'SelPrevClipBoundaryToCursor' )
    capture( 'ClipBounds005.png', 'AllTracks' )
    # clip bound right
    gappyTrack2()
    capture( 'ClipBounds006.png', 'AllTracks' )
    do( 'Select: Start=60 End=60')
    capture( 'ClipBounds007.png', 'AllTracks' )
    do( 'SelCursorToNextClipBoundary' )
    capture( 'ClipBounds008.png', 'AllTracks' )
    do( 'SelCursorToNextClipBoundary' )
    capture( 'ClipBounds009.png', 'AllTracks' )
    do( 'SelCursorToNextClipBoundary' )
    capture( 'ClipBounds010.png', 'AllTracks' )
    # clip left
    gappyTrack2()
    capture( 'ClipBounds011.png', 'AllTracks' )
    do( 'Select: Start=60 End=60')
    capture( 'ClipBounds012.png', 'AllTracks' )
    do( 'SelPrevClip' )
    capture( 'ClipBounds013.png', 'AllTracks' )
    do( 'SelPrevClip' )
    capture( 'ClipBounds014.png', 'AllTracks' )
    # clip right
    gappyTrack2()
    capture( 'ClipBounds015.png', 'AllTracks' )
    do( 'Select: Start=60 End=60')
    capture( 'ClipBounds016.png', 'AllTracks' )
    do( 'SelNextClip' )
    capture( 'ClipBounds017.png', 'AllTracks' )
    do( 'SelNextClip' )
    capture( 'ClipBounds018.png', 'AllTracks' )





clipb_imagesA()
