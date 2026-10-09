/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick

//! PlaybackToolBar.qml instantiates VideoPanelModel to read the thumbnail
//! size, so the type has to exist even with the video module compiled out -
//! otherwise the whole playback toolbar fails to load. The height it reports
//! is never used, because the toolbar item it sizes is never present.
QtObject {
    property int toolbarHeight: 44
}
