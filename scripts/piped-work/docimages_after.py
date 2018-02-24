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
            "HighPassFilter","LowPassFilter",
            "NotchFilter","AdjustableFade","Delay","Limiter"               
            ]

Slow = ["ChangePitch","ChangeSpeed", "ChangeTempo","Paulstretch",
         "TimeScale"
            ]


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
    do( 'Select: First=0 Last=0 Start=10 End=130')    
    do( 'ZoomSel' )
    do( 'Select: Start=55 End=70')

def withDcBias( amount ):
    loadMonoTracks(1)
    do( 'NewMonoTrack' )
    do( 'Select: First=1 Last=1 Start=0 End=0.5' )
    do( 'Tone: Frequency=1.0 Amplitude='+str(amount) +
        ' Waveform=Square Interpolation=Linear' )
    do( 'Repeat: Count=300' )
    do( 'Join' );
    do( 'Select: First=0 Last=1 Start=0 End=150' )
    do( 'MixAndRender' );
    do( 'Select: First=0 Last=0 Start=0 End=0' )
    do( 'SetTrack: Name="Track with DC Bias"')
    
def spaceyTrack() :   
    loadMonoTracks(1)
    do( 'Select: First=0 Last=0 Start=20 End=40')
    do( 'Silence' )
    do( 'Select: First=0 Last=0 Start=60 End=100')
    do( 'Silence' )
    do( 'Select: First=0 Last=0 Start=10 End=140')


def imageAfters( commands, doWhat):
    starterTrack()
    capture( 'BeforeEffect.png', 'All_Tracks_Plus' )
    for name in commands :
        starterTrack()
        do( 'SetTrack: Name="'+name+'"')
        do( name )
        capture( 'After' + name + '1.png' , doWhat )
        do( 'Select: Start=0 End=0')
        capture( 'After' + name + '2.png' , doWhat )


def generators():
    for name in Generators :
        makeWayForTracks()
        do( 'NewMonoTrack' )
        do( 'SetTrack: Name="'+name+'"')
        do( 'Select: Start=0 End=10' )
        do( name )
        do( 'ZoomSel' )
        do( 'Select: Start=0 End=0' )
        capture( 'After' + name + '.png' , 'All_Tracks_Plus' )


def spaceDemo():
    spaceyTrack()
    capture( 'BeforeTruncateSilence.png' , 'All_Tracks' )
    do( 'Select: First=0 Last=0 Start=0 End=0')
    capture( 'SpaceyTrack.png' , 'All_Tracks' )
    do( 'TruncateSilence' )
    capture( 'AfterTruncateSilence1.png' , 'All_Tracks' )
    do( 'Select: First=0 Last=0 Start=0 End=0')
    capture( 'AfterTruncateSilence2.png' , 'All_Tracks' )
    spaceyTrack()
    do( 'Disjoin' )
    capture( 'AfterDisjoin1.png' , 'All_Tracks' )
    do( 'Select: First=0 Last=0 Start=0 End=0')
    capture( 'AfterDisjoin2.png' , 'All_Tracks' )
    

def biasDemo():
    withDcBias( 0.1 )
    capture( 'DcBias.png' , 'All_Tracks' )

imageSet("After")    
imageAfters( Commands, 'All_Tracks_Plus' ) # With ruler
imageAfters( Effects, 'All_Tracks' ) # Without ruler
imageAfters( Slow, 'All_Tracks' ) # Without ruler
generators()
spaceDemo()
biasDemo()    


