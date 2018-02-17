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
    capture( 'ClipBounds001.png', 'All_Tracks' )
    do( 'Select: Start=60 End=60')
    capture( 'ClipBounds002.png', 'All_Tracks' )
    do( 'SelPrevClipBoundaryToCursor' )
    capture( 'ClipBounds003.png', 'All_Tracks' )
    do( 'SelPrevClipBoundaryToCursor' )
    capture( 'ClipBounds004.png', 'All_Tracks' )
    do( 'SelPrevClipBoundaryToCursor' )
    capture( 'ClipBounds005.png', 'All_Tracks' )
    # clip bound right
    gappyTrack2()
    capture( 'ClipBounds006.png', 'All_Tracks' )
    do( 'Select: Start=60 End=60')
    capture( 'ClipBounds007.png', 'All_Tracks' )
    do( 'SelCursorToNextClipBoundary' )
    capture( 'ClipBounds008.png', 'All_Tracks' )
    do( 'SelCursorToNextClipBoundary' )
    capture( 'ClipBounds009.png', 'All_Tracks' )
    do( 'SelCursorToNextClipBoundary' )
    capture( 'ClipBounds010.png', 'All_Tracks' )
    # clip left
    gappyTrack2()
    capture( 'ClipBounds011.png', 'All_Tracks' )
    do( 'Select: Start=60 End=60')
    capture( 'ClipBounds012.png', 'All_Tracks' )
    do( 'SelPrevClip' )
    capture( 'ClipBounds013.png', 'All_Tracks' )
    do( 'SelPrevClip' )
    capture( 'ClipBounds014.png', 'All_Tracks' )
    # clip right
    gappyTrack2()
    capture( 'ClipBounds015.png', 'All_Tracks' )
    do( 'Select: Start=60 End=60')
    capture( 'ClipBounds016.png', 'All_Tracks' )
    do( 'SelNextClip' )
    capture( 'ClipBounds017.png', 'All_Tracks' )
    do( 'SelNextClip' )
    capture( 'ClipBounds018.png', 'All_Tracks' )





clipb_imagesA()
