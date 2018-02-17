# docimages_arrange.py
# Sends commands to get images for the manual.
# These ones arrange tracks and do alignment.

# Make sure Audacity is running first and that mod-script-pipe is enabled
# before running this script.

#load and run the common core.
exec( open("docimages_core.py" ).read() )

import time



def arrange_imagesA() :
    loadMonoTracks( 4 )
    do( 'SetTrack: Track=0 Name="Claire" Height=60 Color=Color0')
    do( 'SetTrack: Track=1 Name="Ann" Height=60 Color=Color1')
    do( 'SetTrack: Track=2 Name="Bob" Height=60 Color=Color2')
    do( 'SetTrack: Track=3 Name="David" Height=60 Color=Color3')
    do( 'SetClip: Track=0 At=1 Start=25')
    do( 'SetClip: Track=1 At=1 Start=15')
    do( 'SetClip: Track=2 At=1 Start=20')
    do( 'SetClip: Track=3 At=1 Start=10')
    do( 'Select: First=0 Last=100 Mode=Remove' )
    capture( 'TrackOrder001.png', 'All_Tracks' )
    do( 'SetTrack: Track=2 Focused=1')
    capture( 'TrackOrder002.png', 'All_Tracks' )
    do( 'SortByName')
    capture( 'TrackOrder003.png', 'All_Tracks' )
    do( 'SortByTime')
    capture( 'TrackOrder004.png', 'All_Tracks' )

def arrange_imagesB() :
    loadMonoTracks( 3 )
    do( 'SetClip: Track=0 At=1 Start=5')
    time.sleep( 1 )
    do( 'SetClip: Track=1 At=1 Start=3')
    time.sleep( 1 )
    do( 'SetClip: Track=2 At=1 Start=4')
    time.sleep( 1 )
    do( 'SetClip: Track=3 At=1 Start=2')
    capture( 'TrackOrder001.png', 'All_Tracks' )

def arrange_imagesC() :

    do( 'SetTrack: Track=2 Focused=1')
    capture( 'TrackOrder002.png', 'All_Tracks' )
    do( 'SortByName')
    capture( 'TrackOrder003.png', 'All_Tracks' )
    do( 'SortByTime')
    capture( 'TrackOrder004.png', 'All_Tracks' )

#quickTest()

arrange_imagesA()
#arrange_imagesB()
#arrange_imagesC()


