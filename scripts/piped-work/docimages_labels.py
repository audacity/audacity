# docimages_labels.py
# Sends commands to get images for the manual.
# Images for https://alphamanual.audacityteam.org/man/Removing_Labels_-_Examples

# Make sure Audacity is running first and that mod-script-pipe is enabled
# before running this script.

#load and run the common core.
exec( open("docimages_core.py" ).read() )



def addLabels():
    do( 'Select: Start=0 End=1' )
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
    do( 'Select: Start=0 End=1' )
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
    do( 'Select: First=3 Last=3' )


def image1and2() :
    makeStereoTracks(1)
    addLabels()
    # A stero track with four labels.
    do( "Select: Start=0 End=0" )
    capture( 'AutoLabels001.png', 'First_Two_Tracks' )
    # Removing a label with delete (fraud - we used split delete)
    do( "Select: Start=44.5 End=60.5 First=2 Last=2" )
    do( "SplitDelete" )
    do( "Select: Start=0 End=0 First=0 Last=2" )
    capture( 'AutoLabels002.png','First_Two_Tracks' )

def image3and4() :
    makeStereoTracks(1)
    addLabels()
    # Removing a label with split-delete step 1
    do( "Select: Start=44.5 End=60.5 First=2 Last=2" )
    capture( 'AutoLabels003.png','First_Two_Tracks' )
    # Removing a label with split-delete step 1
    do( "Select: Start=44.5 End=60.5 First=2 Last=2" )
    do( "SplitDelete" )
    capture( 'AutoLabels004.png','First_Two_Tracks' )

def image5and6and7() :
    makeStereoTracks(1)
    addLabels2()
    # Nothing selected
    do( "Select: Start=0 End=0" )
    capture( 'AutoLabels005.png','First_Three_Tracks' )
    # A range selected in label track.
    do( "Select: Start=28.5 End=58.5" )
    capture( 'AutoLabels006.png','First_Three_Tracks' )
    # Deleting in label track only.
    do( "Delete" )
    do( "Select: Start=0 End=0" )
    capture( 'AutoLabels007.png','First_Three_Tracks' )

def image8and9and10() :
    makeStereoTracks(1)
    addLabels2()
    # Select nothing in all three tracks.
    do( "Select: First=2 Last=2 Start=100 End=125" )
    do( "AddLabel" )
    do( 'SetLabel: Label=9 Text="Clap" selected=0 Start=110 End=118 ')
    do( 'Select: First=0 Last=3 Start=0 End=0')
    capture( 'AutoLabels008.png','First_Three_Tracks' )
    # Select label and all three tracks
    do( 'SetLabel: Label=9 Text="Clap" selected=1 Start=110 End=118 ')
    do( 'Select: First=0 Last=3 Start=110 End=118')
    capture( 'AutoLabels009.png','First_Three_Tracks' )
    # Delete label and from all three tracks.
    do( 'Delete' )
    capture( 'AutoLabels010.png','First_Three_Tracks' )
    
image1and2()
image3and4()
image5and6and7()
image8and9and10()




