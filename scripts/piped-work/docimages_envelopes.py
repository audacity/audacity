# docimages_envelopes.py
# Sends commands to get images for the manual.
# Images for envelope manipulation.

# Make sure Audacity is running first and that mod-script-pipe is enabled
# before running this script.

#load and run the common core.
exec( open("docimages_core.py" ).read() )



def env_images() :
    loadMonoTracks(1)
    do( 'Select: Start=0 End=0')
    # A mono track
    capture( 'Envelope001.png', 'AllTracks' )
    do( 'EnvelopeTool' )
    # As spectrogram.
    capture( 'Envelope002.png', 'AllTracks' )
    do( 'SetEnvelope: Time=55 Value=0.9');
    capture( 'Envelope003.png', 'AllTracks' )
    do( 'SetEnvelope: Time=120 Value=0.4');
    capture( 'Envelope004.png', 'AllTracks' )
    do( 'SetEnvelope: Time=125 Value=0.9');
    capture( 'Envelope005.png', 'AllTracks' )
    do( 'SetEnvelope: Time=45 Value=0.85');
    capture( 'Envelope006.png', 'AllTracks' )
    do( 'SetEnvelope: Time=25 Value=1.85');
    capture( 'Envelope007.png', 'AllTracks' )
    do( 'SetEnvelope: Time=0 Value=0.85');
    capture( 'Envelope008.png', 'AllTracks' )
    do( 'SetTrack: VZoom=Times2' )
    capture( 'Envelope009.png', 'AllTracks' )
    do( 'SetTrack: VZoom=HalfWave' )
    capture( 'Envelope010.png', 'AllTracks' )
    do( 'SelectTool' )
    capture( 'Envelope011.png', 'AllTracks' )
    do( 'SetTrack: VZoom=Reset' )
    capture( 'Envelope012.png', 'AllTracks' )

    

env_images()

