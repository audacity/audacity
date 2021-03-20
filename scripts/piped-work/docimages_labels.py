# docimages_labels.py
# Sends commands to get images for the manual.
# Images for https://alphamanual.audacityteam.org/man/Removing_Labels_-_Examples

# Make sure Audacity is running first and that mod-script-pipe is enabled
# before running this script.

#load and run the common core.
exec( open("docimages_core.py" ).read() )



def addLabels():
    #do( 'Select: Start=0 End=1' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'SetLabel: Label=0 Text="Homweward Bound" Start=1 End=1 ')
    do( 'SetLabel: Label=1 Text="Silver Dagger" Start=35 End=35 ')
    do( 'SetLabel: Label=2 Selected=1 Text="NOISE" Start=45 End=60 ')
    do( 'SetLabel: Label=3 Text="Blood in These Veins" Start=80 End=80 ')
    do( 'Select: Start=45 End=60' )

def addLabels2():
    #do( 'Select: Start=0 End=1' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'NewLabelTrack' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'AddLabel' )
    do( 'SetLabel: Label=0 Selected=0 Text="intro" Start=0.5 End=0.5 ')
    do( 'SetLabel: Label=1 Text="thought piece" Start=15 End=15 ')
    do( 'SetLabel: Label=2 Text="discussion" Start=60 End=60 ')
    do( 'SetLabel: Label=3 Text="summary" Start=100 End=100 ')
    do( 'SetLabel: Label=4 Text="credits" Start=125 End=125 ')
    do( 'SetLabel: Label=5 Selected=0 Text="Bach" Start=0.5 End=0.5 ')
    do( 'SetLabel: Label=6 Text="Vivaldi" Start=30 End=30 ')
    do( 'SetLabel: Label=7 Text="Mozart" Start=60 End=60 ')
    do( 'SetLabel: Label=8 Text="Satie" Start=90 End=90 ')
    do( 'SetLabel: Label=9 Text="Chopin" Start=120 End=120 ')
    do( 'Select: Track=3' )


def label_image1and2() :
    makeStereoTracks(1)
    addLabels()
    # A stereo track with four labels.
    do( "Select: Start=0 End=0" )
    capture( 'AutoLabels001.png', 'FirstTwoTracks' )
    # Removing a label with delete (fraud - we used split delete)
    do( "Select: Start=44.5 End=60.5 Track=1" )
    do( "SplitDelete" )
    do( "Select: Start=0 End=0 TrackCount=2" )
    capture( 'AutoLabels002.png','FirstTwoTracks' )

def label_image3and4() :
    makeStereoTracks(1)
    addLabels()
    # Removing a label with split-delete step 1
    do( "Select: Start=44.5 End=60.5 Track=1" )
    capture( 'AutoLabels003.png','FirstTwoTracks' )
    # Removing a label with split-delete step 2
    do( "Select: Start=44.5 End=60.5 Track=1" )
    do( "SplitDelete" )
    do( "Select: Start=0 End=0 TrackCount=0" )
    capture( 'AutoLabels004.png','FirstTwoTracks' )

def label_image5and6and7() :
    makeStereoTracks(1)
    addLabels2()
    # Nothing selected
    do( "Select: Start=0 End=0 TrackCount=0" )
    capture( 'AutoLabels005.png','FirstThreeTracks' )
    # A range selected in label track.
    do( "Select: Start=28.5 End=58.5 Track=1" )
    capture( 'AutoLabels006.png','FirstThreeTracks' )
    # Deleting in label track only.
    do( "Delete" )
    do( "Select: Start=0 End=0 TrackCount=0" )
    capture( 'AutoLabels007.png','FirstThreeTracks' )

def label_image8and9and10() :
    makeStereoTracks(1)
    addLabels2()
    # Select nothing in all three tracks.
    do( "Select: TrackCount=2 Start=100 End=125" )
    do( "AddLabel" )
    do( 'SetLabel: Label=9 Text="Clap" Selected=0 Start=110 End=118 ')
    do( 'Select: TrackCount=2 Start=0 End=0')
    capture( 'AutoLabels008.png','FirstThreeTracks' )
    # Select label and all three tracks
    do( 'SetLabel: Label=9 Text="Clap" Selected=1 Start=110 End=118 ')
    do( 'Select: TrackCount=3 Start=110 End=118')
    capture( 'AutoLabels009.png','FirstThreeTracks' )
    # Delete label and from all three tracks.
    do( 'Delete' )
    capture( 'AutoLabels010.png','FirstThreeTracks' )

imageSet("Labels")    
label_image1and2()
label_image3and4()
label_image5and6and7()
label_image8and9and10()




