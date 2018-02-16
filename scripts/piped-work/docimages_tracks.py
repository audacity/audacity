# docimages_tracks.py
# Sends commands to get images for the manual.
# Images for https://alphamanual.audacityteam.org/man/Audio_Tracks

# Make sure Audacity is running first and that mod-script-pipe is enabled
# before running this script.


#load and run the common core.
exec( open("docimages_core.py" ).read() )


def image1and8() :
    loadMonoTracks(1)
    # A mono track complete with ruler
    capture( 'AutoTracks001.png', 'First_Track_Plus' )
    # Mono with arrow at start.
    do( 'SetClip: Clip=0 Start=-4.0')
    capture( 'AutoTracks008.png', 'First_Track' )

def image2and6() :
    makeStereoTracks(1)
    # A stereo track, with its name on the track    
    capture( 'AutoTracks002.png', 'First_Track' )
    # A stereo track, with different sized channels
    do( 'SetTrack: Track=0 Height=80')
    do( 'SetTrack: Track=1 Height=180')
    capture( 'AutoTracks006.png', 'First_Track' )

# Four colours of track
def image3() :
    loadMonoTracks( 4 )
    do( 'SetTrack: Track=0 Name="Instrument 1" Height=122 Color=Color0')
    do( 'SetTrack: Track=1 Name="Instrument 2" Height=122 Color=Color1')
    do( 'SetTrack: Track=2 Name="Instrument 3" Height=122 Color=Color2')
    do( 'SetTrack: Track=3 Name="Instrument 4" Height=122 Color=Color3')
    capture( 'AutoTracks003.png', 'First_Four_Tracks' )

def image7and4and5():
    loadMonoTracks(2)
    # Two mono tracks of different sizes
    do( 'SetTrack: Track=0 Height=180')
    do( 'SetTrack: Track=1 Height=80')
    capture( 'AutoTracks007.png', 'First_Two_Tracks' )
    # Two Tracks, ready to make stereo
    do( 'SetTrack: Track=0 Name="Left Track" Height=80')
    do( 'SetTrack: Track=1 Name="Right Track" Height=80')
    capture( 'AutoTracks004.png', 'First_Two_Tracks' )
    # Combined Stereo Track
    do( 'SetTrack: Track=0 Pan=-1 Height=80')
    do( 'SetTrack: Track=1 Pan=1 Height=80')
    do( 'MixAndRender' )
    do( 'SetTrack: Track=0 Name="Combined" Height=80')
    do( 'SetTrack: Track=1 Height=80')
    do( 'Select: First=0 Last=1' )
    capture( 'AutoTracks005.png', 'First_Track' )


def image9and10() :
    #make rather than load.  We want an artificial track.
    makeMonoTracks(1)
    # Zoomed in to show points stem-plot
    do( 'Select: Start=0 End=0.003' )
    do( 'ZoomSel' );
    do( 'Amplify: Ratio=3.0' )
    do( 'SetPreference: Name=/GUI/SampleView Value=1 Reload=1')
    capture( 'AutoTracks009.png', 'First_Track' )
    # Zoomed in to show points stem-plot and then no stem plot
    do( 'SetPreference: Name=/GUI/SampleView Value=0 Reload=1')
    capture( 'AutoTracks010.png', 'First_Track' )
    
image1and8()
image2and6()
image3()
image7and4and5()
image9and10()

