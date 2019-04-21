# docimages_tracks.py
# Sends commands to get images for the manual.
# Images for https://alphamanual.audacityteam.org/man/Audio_Tracks

# Make sure Audacity is running first and that mod-script-pipe is enabled
# before running this script.


#load and run the common core.
exec( open("docimages_core.py" ).read() )


def track_image1and8() :
    loadMonoTracks(1)
    # A mono track complete with ruler
    capture( 'AutoTracks001.png', 'FirstTrackPlus' )
    # Mono with arrow at start.
    do( 'SetClip: Clip=0 Start=-4.0')
    capture( 'AutoTracks008.png', 'FirstTrack' )

def track_image2and6() :
    makeStereoTracks(1)
    # A stereo track, with its name on the track    
    capture( 'AutoTracks002.png', 'FirstTrack' )
    # A stereo track, with different sized channels
    do( 'Select: Track=0 TrackCount=0.5' );
    do( 'SetTrack: Height=80')
    do( 'Select: Track=0.5 TrackCount=0.5' );
    do( 'SetTrack: Height=180')
    do( 'Select' );
    capture( 'AutoTracks006.png', 'FirstTrack' )

# Four colours of track
def track_image3() :
    loadMonoTracks( 4 )
    do( 'Select: Track=0' );
    do( 'SetTrack: Name="Instrument 1" Height=122 Color=Color0')
    do( 'Select: Track=1' );
    do( 'SetTrack: Name="Instrument 2" Height=122 Color=Color1')
    do( 'Select: Track=2' );
    do( 'SetTrack: Name="Instrument 3" Height=122 Color=Color2')
    do( 'Select: Track=3' );
    do( 'SetTrack: Name="Instrument 4" Height=122 Color=Color3')
    do( 'Select: TrackCount=4' );
    capture( 'AutoTracks003.png', 'FirstFourTracks' )

def track_image7and4and5():
    loadMonoTracks(2)
    # Two mono tracks of different sizes
    do( 'Select: Track=0' );
    do( 'SetTrack: Height=180')
    do( 'Select: Track=1' );
    do( 'SetTrack: Height=80')
    do( 'Select: TrackCount=2' );
    capture( 'AutoTracks007.png', 'FirstTwoTracks' )
    # Two Tracks, ready to make stereo
    do( 'Select: Track=0' );
    do( 'SetTrack: Name="Left Track" Height=80')
    do( 'Select: Track=1' );
    do( 'SetTrack: Name="Right Track" Height=80')
    do( 'Select: TrackCount=2' );
    capture( 'AutoTracks004.png', 'FirstTwoTracks' )
    # Combined Stereo Track
    do( 'Select: Track=0' );
    do( 'SetTrack: Pan=-1 Height=80')
    do( 'Select: Track=1' );
    do( 'SetTrack: Pan=1 Height=80')
    do( 'MixAndRender' )
    do( 'Select: Track=0' );
    do( 'SetTrack: Name="Combined" Height=80')
    capture( 'AutoTracks005.png', 'FirstTrack' )


def track_image9and10() :
    #make rather than load.  We want an artificial track.
    makeMonoTracks(1)
    # Zoomed in to show points stem-plot
    do( 'Select: Start=0 End=0.003' )
    do( 'ZoomSel' );
    do( 'Amplify: Ratio=3.0' )
    do( 'SetPreference: Name=/GUI/SampleView Value=1 Reload=1')
    do( 'Select: TrackCount=3' );
    capture( 'AutoTracks009.png', 'FirstTrack' )
    # Zoomed in to show points stem-plot and then no stem plot
    do( 'SetPreference: Name=/GUI/SampleView Value=0 Reload=1')
    do( 'Select: TrackCount=3' );
    capture( 'AutoTracks010.png', 'FirstTrack' )

imageSet("Tracks")    
track_image1and8()
track_image2and6()
track_image3()
track_image7and4and5()
track_image9and10()

