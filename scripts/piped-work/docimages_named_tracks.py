# docimages_named_tracks.py
# Sends commands to get images for the manual.
# Images for https://alphamanual.audacityteam.org/man/Audio_Tracks

# Make sure Audacity is running first and that mod-script-pipe is enabled
# before running this script.


#load and run the common core.
exec( open("docimages_core.py" ).read() )

import time


def ntrack_stereo() :
    loadStereoTrack()
    # A stereo track, with its name on the track    
    capture( 'AutoTracks002.png', 'First_Track' )
    # A stereo track, with different sized channels
    do( 'SetTrack: Channel=0 Height=80')
    do( 'SetTrack: Channel=1 Height=80')
    do( 'SetTrack: Name="Voodoo Child (Slight Return)' )
    capture( 'Audio Track with track name superimposed.png', 'All_Tracks' )

# Four colours of track
def ntrack_colour() :
    loadMonoTracks( 4 )
    do( 'SetTrack: Track=0 Name="Instrument 1 colorway" Height=90 Color=Color0')
    do( 'SetTrack: Track=1 Name="Instrument 2 colorway" Height=90 Color=Color1')
    do( 'SetTrack: Track=2 Name="Instrument 3 colorway" Height=90 Color=Color2')
    do( 'SetTrack: Track=3 Name="Instrument 4 colorway" Height=90 Color=Color3')
    do( 'Select: First=0 Last=3 Start=50 End=70')
    capture( 'Waveform colorways.png', 'All_Tracks' )

# Showing the pin...
def ntrack_pinned() :
    loadMonoTracks( 1 )
    do( 'SetTrack: Name="Waveform moving this way <==' )
    do( 'SetPreference: Name="/AudioIO/PinnedHead" Value=1 Reload=1')
    do( 'Play' )
    time.sleep( 1 )
    capture( 'Pinned head playback annotated trackname.png', 'All_Tracks_Plus' )
    do( 'Stop')
    do( 'SetPreference: Name="/AudioIO/PinnedHead" Value=0 Reload=1')

def ntrack_punch():
    loadMonoTracks(2)
    do( 'AddLabelTrack' )

    #create the end result
    do('Select: First=1 Last=1 Start=50 End=70')
    do('SplitNew')
    
    do( 'SetTrack: Track=0 Name="Backup of Original" Height=90')
    do( 'SetTrack: Track=1 Name="Original Recording" Height=90')
    do( 'Select: First=0 Last=0' )
    #show a messed up original...
    do( 'Distortion' )
    do( 'WahWah' )
    
    do( 'Select: First=2 Last=2 Start=40 End=80' )
    do( 'Duplicate' )
    do( 'Select: First=2 Last=2 Start=40 End=80' )
    do( 'Silence' )
    # Just to get silence before and after.
    do( 'Select: First=2 Last=3 Start=40 End=80' )
    do( 'MixAndRender' )
    do( 'SetTrack: Track=2 Name="Punched in" Height=90')
    do( 'Select: First=2 Last=2 Start=50 End=70' )
    do( 'AddLabel' )
    do( 'SetTrack: Track=3 Focused=1')
    do( 'TrackMoveUp' )
    do( 'SetTrack: Track=0 Mute=1')
    do( 'SetLabel: Text="Bad Part" Selected=0')
    do( 'Select: First=1 Last=1 Start=40 End=80' )

    #work backwards from the end result...
    capture( 'Punch-in tracks002.png', 'All_Tracks' )
    do( 'Select: First=3 Last=3 Start=40 End=80' )
    do( 'RemoveTracks')
    do( 'SetTrack: Track=1 Solo=1')
    do( 'Select: First=1 Last=1 Start=40 End=80' )
    capture( 'Punch-in tracks001.png', 'All_Tracks' )
    do( 'Select: First=1 Last=1 Start=50 End=70' )
    capture( 'Punch-in tracks000.png', 'All_Tracks' )

imageSet("NamedTracks")
do( 'SetPreference: Name="/GUI/ShowTrackNameInWaveform" Value=1')
ntrack_stereo()
ntrack_colour()
ntrack_pinned()
ntrack_punch()
