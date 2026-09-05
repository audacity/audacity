/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick

//! ProjectPage.qml imports Audacity.Video unconditionally, so the module has
//! to resolve even when the video module is compiled out - otherwise the
//! whole project page fails to load and the application is unusable.
Item {
    property var navigationSection
    property int navigationOrderStart
}
