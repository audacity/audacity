# docimages_after.py
# Sends commands to get images for the manual.
# Images for before and after for most commands

# Make sure Audacity is running first and that mod-script-pipe is enabled
# before running this script.

#load and run the common core.
exec( open("docimages_core.py" ).read() )



Commands = ["Cut", "Split", "SplitCut",
            "Silence", "Trim",
            "SplitNew", "Duplicate",
            "ZoomIn", "ZoomOut", "ZoomNormal", "ZoomSel", "FitInWindow", "FitV" 
            ]

Effects  = [
            "Reverse","FadeIn", "FadeOut", "Invert",
            "Amplify","BassAndTreble","Compressor","Distortion","Echo",
            "Equalization","Normalize","Phaser",
            "Repeat","Reverb","Wahwah",
            "High-passFilter","Low-passFilter",
            "NotchFilter","AdjustableFade","Delay","Limiter"               
            ]

Slow = ["ChangePitch","ChangeSpeed", "ChangeTempo","Paulstretch" ]


Generators = ["Chirp","DtmfTones","Noise","Tone","Pluck"]



# "Delete", "Copy", - look same as cut
# "SplitDelete" - same as splitcut

ToDo = ["Paste", "Join", "DisJoin",
        
"ClipFix",
"AutoDuck",
"ClickRemoval",
"FindClipping",
"TruncateSilence",
"SilenceFinder",
"BeatFinder",
"NyquistPrompt",
"RhythmTrack",
"SoundFinder",
"SpectralEditMultiTool",
"SpectralEditParametricEq",
"SpectralEditShelves",
"VocalReductionAndIsolation",
"CrossfadeClips",
"CrossfadeTracks",
"RegularIntervalLabels",

"Vocoder", #stereo        
        
]

def starterTrack():
    loadMonoTracks(1)
    do( 'Select: Start=10 End=130')    
    do( 'ZoomSel' )
    do( 'Select: Start=55 End=70')

def withDcBias( amount ):
    loadMonoTracks(1)
    do( 'NewMonoTrack' )
    do( 'Select: Track=1 Start=0 End=0.5' )
    do( 'Tone: Frequency=1.0 Amplitude='+str(amount) +
        ' Waveform=Square Interpolation=Linear' )
    do( 'Repeat: Count=300' )
    do( 'Join' );
    do( 'Select: TrackCount=2 Start=0 End=150' )
    do( 'MixAndRender' );
    do( 'Select: TrackCount=0 Start=0 End=0' )
    do( 'SetTrack: Name="Track with DC Bias"')
    
def spaceyTrack() :   
    loadMonoTracks(1)
    do( 'Select: Start=20 End=40')
    do( 'Silence' )
    do( 'Select: Start=60 End=100')
    do( 'Silence' )
    do( 'Select: Start=10 End=140')


def imageAfters( commands, doWhat):
    starterTrack()
    capture( 'BeforeEffect.png', 'AllTracksPlus' )
    for name in commands :
        starterTrack()
        do( 'SetTrack: Name="'+name+'"')
        do( name )
        capture( 'After' + name + '1.png' , doWhat )
        do( 'Select: TrackCount=0 Start=0 End=0')
        capture( 'After' + name + '2.png' , doWhat )


def generators():
    for name in Generators :
        makeWayForTracks()
        do( 'NewMonoTrack' )
        do( 'SetTrack: Name="'+name+'"')
        do( 'Select: Start=0 End=10' )
        do( name )
        do( 'ZoomSel' )
        do( 'Select: TrackCount=0 Start=0 End=0' )
        capture( 'After' + name + '.png' , 'AllTracksPlus' )


def spaceDemo():
    spaceyTrack()
    capture( 'BeforeTruncateSilence.png' , 'AllTracks' )
    do( 'Select: Start=0 End=0')
    capture( 'SpaceyTrack.png' , 'AllTracks' )
    do( 'TruncateSilence' )
    capture( 'AfterTruncateSilence1.png' , 'AllTracks' )
    do( 'Select: Start=0 End=0')
    capture( 'AfterTruncateSilence2.png' , 'AllTracks' )
    spaceyTrack()
    do( 'Disjoin' )
    capture( 'AfterDisjoin1.png' , 'AllTracks' )
    do( 'Select: Start=0 End=0')
    capture( 'AfterDisjoin2.png' , 'AllTracks' )
    

def biasDemo():
    withDcBias( 0.1 )
    capture( 'DcBias.png' , 'AllTracks' )

imageSet("After")    
imageAfters( Commands, 'AllTracksPlus' ) # With ruler
imageAfters( Effects, 'AllTracks' ) # Without ruler
imageAfters( Slow, 'AllTracks' ) # Without ruler
generators()
spaceDemo()
biasDemo()    


