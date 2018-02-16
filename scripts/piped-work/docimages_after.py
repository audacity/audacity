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
            "Amplify","BassAndTreble","ChangePitch","ChangeSpeed",
            "ChangeTempo","Compressor","Distortion","Echo",
            "Equalization","Normalize","Paulstretch","Phaser",
            "Repeat","Reverb","TimeScale","Wahwah",
            "HighPassFilter","LowPassFilter","Vocoder",
            "NotchFilter","AdjustableFade","Delay","Limiter"               
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
        
]

def starterTrack():
    loadMonoTracks(1)
    do( 'Select: First=0 Last=0 Start=10 End=130')    
    do( 'ZoomSel' )
    do( 'Select: Start=55 End=70')

def imageAfters():
    starterTrack()
    capture( 'BeforeEffect.png', 'All_Tracks_plus' )
    for name in Commands :
        starterTrack()
        do( 'SetTrack: Name="'+name+'"')
        do( name )
        capture( 'After' + name + '.png' , 'All_Tracks_Plus' )


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
    
imageAfters()
generators()

