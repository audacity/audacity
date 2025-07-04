/*
* Audacity: A Digital Audio Editor
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
        api.keyboard.key("Ctrl+O")
    },
    Close: function()
    {
        api.keyboard.key("Ctrl+W")
    },
    Exit: function()
    {
        api.keyboard.key("Ctrl+W")
    },
    Save: function()
    {
        api.keyboard.key("Ctrl+S")
    },
    Import: function()
    {
        //(THIS NEEDS TO BE TESTED WHEN IMPLEMENTED)
        api.keyboard.key("Ctrl+Shift+I")
    },
    Undo: function()
    {
        api.keyboard.key("Ctrl+Z")
    },
    Redo: function()
    {
        api.keyboard.key("Ctrl+Y")
    },
    Cut: function()
    {
        api.keyboard.key("Ctrl+X")
    },
    Delete: function()
    {
        api.keyboard.key("Ctrl+K")
    },
    Copy: function()
    {
        api.keyboard.key("Ctrl+C")
    },
    Paste: function()
    {
        api.keyboard.key("Ctrl+V")
    },
    Duplicate: function()
    {
        api.keyboard.key("Ctrl+D")
    },
    Preferences: function()
    {
        api.keyboard.key("Ctrl+P")
    },
    MacPreferences: function()
    {
        api.keyboard.key("Ctrl+,")
    },
    ShiftDelete: function()
    {
        api.keyboard.key("Ctrl+Del")
    },
    CutAndLeaveGap: function()
    {
        api.keyboard.key("Ctrl+Alt+X")
    },
    MacCutAndLeaveGap: function()
    {
        api.keyboard.key("Ctrl+Alt+K")
    },
    SilenceAudio: function()
    {
        api.keyboard.key("Ctrl+L")
    },
    TrimAudio: function()
    {
        api.keyboard.key("Ctrl+T")
    },
    Spilt: function()
    {
        api.keyboard.key("Ctrl+I")
    },
    SplitNew: function()
    {
        api.keyboard.key("Ctrl+Alt+I")
    },
    Join: function() 
    {
        api.keyboard.key("Ctrl+J")
    },
    DetachAtSilences: function()
    {
        api.keyboard.key("Ctrl+Alt+J")
    },
    AddLabel: function()
    {
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
        api.keyboard.key("Ctrl+A")
    },
    DeselectAll: function()
    {
        api.keyboard.key("Ctrl+Shift+A")
    },
    SelectAllTracks: function()
    {
        api.keyboard.key("Ctrl+Shift+K")
    },
    SelectAllSyncTracks: function()
    {
        api.keyboard.key("Ctrl+Shift+Y")
    },
    LeftAtPlaybackPosition: function()
    {
        api.keyboard.key("[")
    },
    RightAtPlaybackPosition: function()
    {
        api.keyboard.key("]")
    },
    TrackStartToCursor: function()
    {
        api.keyboard.key("Shift+J")
    },
    CursorToTrackEnd: function()
    {
        api.keyboard.key("Shift+K")
    },
    ZoomIn: function()
    {
        api.keyboard.key("Ctrl+1")
    },
    ZoomNormal: function()
    {
        api.keyboard.key("Ctrl+2")
    },
    ZoomOut: function()
    {
        api.keyboard.key("Ctrl+3")
    },
    ZoomToSelection: function()
    {
        api.keyboard.key("Ctrl+E")
    },
    ZoomToggle: function()
    {
        api.keyboard.key("Shift+Z")
    },
    FitToWidth: function()
    {
        api.keyboard.key("Ctrl+F")
    },
    FitToHeight: function()
    {
        api.keyboard.key("Ctrl+Shift+F")
    },
    CollapseAllTracks: function()
    {
        api.keyboard.key("Ctrl+Shift+C")
    },
    ExpandCollapsedTracks: function()
    {
        api.keyboard.key("Ctrl+Shift+X")
    },
    SelectionStart: function()
    {
        api.keyboard.key("Ctrl+[")
    },
    SelectionEnd: function()
    {
        api.keyboard.key("Ctrl+]")
    },
    PlayStop: function()
    {
        api.keyboard.key("Space")
    },
    PlayStopSetCursor: function()
    {
        api.keyboard.key("X")
    },
    PlayOnce: function()
    {
        api.keyboard.key("Shift+Space")
    },
    Pause: function()
    {
        api.keyboard.key("P")
    },
    Record: function()
    {
        api.keyboard.key("R")
    },
    RecordNewTrack: function()
    {
        api.keyboard.key("Shift+R")
    },
    PunchAndRoll: function()
    {
        api.keyboard.key("Shift+D")
    },
    SelectionStart: function()
    {
        Navigation.goToControl("NavigationLeftPanel", "ListView", "Audio 1") //This needs to be changed when Track view can be selected
        Navigation.left()
    },
    SelectionEnd: function()
    {
        Navigation.goToControl("NavigationLeftPanel", "ListView", "Audio 1") //This needs to be changed when Track view can be selected
        Navigation.right()
    },
    TrackStart: function()
    {
        api.keyboard.key("J")
    },
    TrackEnd: function()
    {
        api.keyboard.key("K")
    },
    ProjectStart: function()
    {
        api.keyboard.key("Home")
    },
    ProjectEnd: function()
    {
        api.keyboard.key("End")
    },
    Loop: function()
    {
        api.keyboard.key("L")
    },
    ClearLoop: function()
    {
        api.keyboard.key("Shift+Alt+L")
    },
    SetLoopToSelection: function()
    {
        api.keyboard.key("Shift+L")
    },
    CreateNewTrack: function()
    {
       api.keyboard.key("Ctrl+Shift+N")
    },
    MixAndRenderToNewTrack: function()
    {
        api.keyboard.key("Ctrl+Shift+M")
    },
    MuteAllTracks: function()
    {
        api.keyboard.key("Ctrl+U")
    },
    UnmuteAllTracks: function()
    {
        api.keyboard.key("Ctrl+Shift+U")
    },
    MuteTracks: function()
    {
        api.keyboard.key("Ctrl+Alt+U")
    },
    UnmuteTracks: function()
    {
        api.keyboard.key("Ctrl+Alt+Shift+U")
    },
    RepeatLastEffect: function()
    {
        api.keyboard.key("Ctrl+R")
    }
}
