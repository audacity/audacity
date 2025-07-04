/*
Helper functions that consolidate shortcuts. These helper functions are written in Pascal case (eg. NewProject, CutAndLeaveGap)
 */

var Navigation = require("Navigation.js")

module.exports = {
    NewProject: function()
    {
        //This emulates the New Project command via Ctrl+N
        api.keyboard.key("Ctrl+N")
    },
    Open: function()
    {
        //This emulates the Open Project command via Ctrl+O
        api.keyboard.key("Ctrl+O")
    },
    Close: function()
    {
        //This emulates the Close Project command via Ctrl+W
        api.keyboard.key("Ctrl+W")
    },
    Exit: function()
    {
        //This emulates the Close Project command via Ctrl+Q
        api.keyboard.key("Ctrl+W")
    },
    Save: function()
    {
        //This emulates the Save project command via Ctrl+S
        api.keyboard.key("Ctrl+S")
    },
    Import: function()
    {
        //This emulates Import file command via Ctrl+Shift+I (THIS NEEDS TO BE TESTED WHEN IMPLEMENTED)
        api.keyboard.key("Ctrl+Shift+I")
    },
    Undo: function()
    {
        //This emulates the Undo command via Ctrl+Z
        api.keyboard.key("Ctrl+Z")
    },
    Redo: function()
    {
        //This emulates the Redo command via Ctrl+Y
        api.keyboard.key("Ctrl+Y")
    },
    Cut: function()
    {
        //This emulates the Cut command via Ctrl+X
        api.keyboard.key("Ctrl+X")
    },
    Delete: function()
    {
        //This emulates the Delete command via Ctrl+K
        api.keyboard.key("Ctrl+K")
    },
    Copy: function()
    {
        //This emulates the Copy command via Ctrl+C
        api.keyboard.key("Ctrl+C")
    },
    Paste: function()
    {
        //This emulates the Paste command via Ctrl+V
        api.keyboard.key("Ctrl+V")
    },
    Duplicate: function()
    {
        //This emulates the Duplicate command via Ctrl+D
        api.keyboard.key("Ctrl+D")
    },
    Preferences: function()
    {
        //This Emulates the Open Preferences command via Ctrl+P on Windows and Linux
        api.keyboard.key("Ctrl+P")
    },
    MacPreferences: function()
    {
        //This emulates the Open Preferences command via Ctrl+, on macOS
        api.keyboard.key("Ctrl+,")
    },
    ShiftDelete: function()
    {
        //This emulates the Shift+Delete shorcut
        api.keyboard.key("Ctrl+Del")
    },
    CutAndLeaveGap: function()
    {
        //This emulates the Cut and Leave Gap shorcut via Ctrl+Alt+X
        api.keyboard.key("Ctrl+Alt+X")
    },
    MacCutAndLeaveGap: function()
    {
        //This emulates the Delete and Leave Gap command via Ctrl+Alt+K
        api.keyboard.key("Ctrl+Alt+K")
    },
    SilenceAudio: function()
    {
        //This emulates the Silence Audio shorcut via Ctrl+L
        api.keyboard.key("Ctrl+L")
    },
    TrimAudio: function()
    {
        //This emulates the Trim Audio shortcut via Ctrl+T
        api.keyboard.key("Ctrl+T")
    },
    Spilt: function()
    {
        //This emulates the Split audio shortcut via Ctrl+I
        api.keyboard.key("Ctrl+I")
    },
    SplitNew: function()
    {
        //This emulates the Split New shortcut via Ctrl+Alt+I
        api.keyboard.key("Ctrl+Alt+I")
    },
    Join: function() 
    {
        //This emulates the Join shortcut via Ctrl+J
        api.keyboard.key("Ctrl+J")
    },
    DetachAtSilences: function()
    {
        //This emulates the Detach at Silences shortcut via Ctrl+Alt+J
        api.keyboard.key("Ctrl+Alt+J")
    },
    AddLabel: function()
    {
        //This emulates the Add label shortcut via Ctrl+B
        api.keyboard.key("Ctrl+B")
    },
    AddLabelAtPlayback: function()
    {
        //This emulates the Add Label at playback shortcut via Ctrl+M on WIndows and Linux
        api.keyboard.key("Ctrl+M")
    },
    MacAddLabelAtPlayback: function()
    {
        //This emulates the Add Label at playback shortcut via Ctrl+. on macOS
        api.keyboard.key("Ctrl+.")
    },
    SelectAll: function()
    {
        //This emulates the select all command via Ctrl+A 
        api.keyboard.key("Ctrl+A")
    },
    DeselectAll: function()
    {
        //This emulates the Deselect All command via Ctrl+Shift+A
        api.keyboard.key("")
    },
    SelectAllTracks: function()
    {
        //This emulates the In All Tracks selection shortcut via Ctrl+Shift+K
        api.keyboard.key("Ctrl+Shift+K")
    },
    SelectAllSyncTracks: function()
    {
        //Extends the current selection up and/or down into all sync-locked tracks in the currently selected track group. via Ctrl+Shift+Y
        api.keyboard.key("Ctrl+Shift+Y")
    },
    LeftAtPlaybackPosition: function()
    {
        //See usage in Audacity Manual
        api.keyboard.key("[")
    },
    RightAtPlaybackPosition: function()
    {
        //See usage in Audacity Manual
        api.keyboard.key("]")
    },
    TrackStartToCursor: function()
    {
        //Selects a region in the selected track(s) from the start of the track to the cursor position.
        api.keyboard.key("Shift+J")
    },
    CursorToTrackEnd: function()
    {
        //Selects a region in the selected track(s) from the cursor position to the end of the track.
        api.keyboard.key("Shift+K")
    },
    ZoomIn: function()
    {
        //Zooms out until the entire project just fits in the window.
        api.keyboard.key("Ctrl+1")
    },
    ZoomNormal: function()
    {
        //Adjusts the height of all the tracks until they fit in the project window.
        api.keyboard.key("Ctrl+2")
    },
    ZoomOut: function()
    {
        //	Zooms out displaying less detail over a greater length of time.
        api.keyboard.key("Ctrl+3")
    },
    ZoomToSelection: function()
    {
        //Zooms in or out so that the selected audio fills the width of the window.
        api.keyboard.key("Ctrl+E")
    },
    ZoomToggle: function()
    {
        api.keyboard.key("Shift+Z")
    },
    FitToWidth: function()
    {
        //Zooms out until the entire project just fits in the window.
        api.keyboard.key("Ctrl+F")
    },
    FitToHeight: function()
    {
        //Adjusts the height of all the tracks until they fit in the project window.
        api.keyboard.key("Ctrl+Shift+F")
    },
    CollapseAllTracks: function()
    {
        //Collapses all tracks to take up the minimum amount of space.
        api.keyboard.key("Ctrl+Shift+C")
    },
    ExpandCollapsedTracks: function()
    {
        //Expands all collapsed tracks to their original size before the last collapse.
        api.keyboard.key("Ctrl+Shift+X")
    },
    SelectionStart: function()
    {
        //When there is a selection, moves the cursor to the start of the selection and removes the selection.
        api.keyboard.key("Ctrl+[")
    },
    SelectionEnd: function()
    {
        //When there is a selection, moves the cursor to the end of the selection and removes the selection.
        api.keyboard.key("Ctrl+]")
    },
    PlayStop: function()
    {
        //Starts and stops playback or stops a recording (stopping does not change the restart position)
        api.keyboard.key("Space")
    },
    PlayStopSetCursor: function()
    {
        //Starts playback like "Play/Stop", but stopping playback sets the restart position to the stop point. 
        api.keyboard.key("X")
    },
    PlayOnce: function()
    {
        //Plays the looping region only once when looping is enabled.
        api.keyboard.key("Shift+Space")
    },
    Pause: function()
    {
        //Temporarily pauses playing or recording without losing your place.
        api.keyboard.key("P")
    },
    Record: function()
    {
        //Starts recording at the end of the currently selected track(s).
        api.keyboard.key("R")
    },
    RecordNewTrack: function()
    {
        //Recording begins on a new track at either the current cursor location or at the beginning of the current selection.
        api.keyboard.key("Shift+R")
    },
    PunchAndRoll: function()
    {
        //Re-record over audio, with a pre-roll of audio that comes before.
        api.keyboard.key("Shift+D")
    },
    SelectionStart: function()
    {
        //Moves cursor to the left edge of the current selection and removes the selection. 
        Navigation.goToControl("NavigationLeftPanel", "ListView", "Audio 1") //This needs to be changed when Track view can be selected
        Navigation.left()
    },
    SelectionEnd: function()
    {
        //Moves cursor to the left edge of the current selection and removes the selection. 
        Navigation.goToControl("NavigationLeftPanel", "ListView", "Audio 1") //This needs to be changed when Track view can be selected
        Navigation.right()
    },
    TrackStart: function()
    {
        //Moves the cursor to the start of the selected track.
        api.keyboard.key("J")
    },
    TrackEnd: function()
    {
        //Moves the cursor to the end of the selected track.
        api.keyboard.key("K")
    },
    ProjectStart: function()
    {
        //Moves the cursor to the beginning of the project.
        api.keyboard.key("Home")
    },
    ProjectEnd: function()
    {
        //Moves the cursor to the end of the project.
        api.keyboard.key("End")
    },
    Loop: function()
    {
        //Toggles playback looping on/off.
        api.keyboard.key("L")
    },
    ClearLoop: function()
    {
        //Clears the looping region.
        api.keyboard.key("Shift+Alt+L")
    },
    SetLoopToSelection: function()
    {
        //Sets the current selection range as the new looping region.
        api.keyboard.key("Shift+L")
    },
    CreateNewTrack: function()
    {
        //Creates a new empty mono audio track. CURRENTLY BROKEN, pressKey is not a function
       api.keyboard.key("Ctrl+Shift+N")
    },
    MixAndRenderToNewTrack: function()
    {
        //Same as Tracks > Mix and Render except that the original tracks are preserved rather than being replaced by the resulting "Mix" track.
        api.keyboard.key("Ctrl+Shift+M")
    },
    MuteAllTracks: function()
    {
        //	Mutes all the audio tracks in the project as if you had used the mute buttons from the Track Control Panel on each track.
        api.keyboard.key("Ctrl+U")
    },
    UnmuteAllTracks: function()
    {
        //Unmutes all the audio tracks in the project as if you had released the mute buttons from the Track Control Panel on each track.
        api.keyboard.key("Ctrl+Shift+U")
    },
    MuteTracks: function()
    {
        //Mutes the selected tracks.
        api.keyboard.key("Ctrl+Alt+U")
    },
    UnmuteTracks: function()
    {
        //Unmutes the selected tracks.
        api.keyboard.key("Ctrl+Alt+Shift+U")
    },
    RepeatLastEffect: function()
    {
        //Repeats the last used effect at its last used settings and without displaying any dialog.
        api.keyboard.key("Ctrl+R")
    }
}
