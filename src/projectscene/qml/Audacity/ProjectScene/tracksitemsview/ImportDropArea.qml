import QtQuick
import QtQuick.Controls

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene
import Audacity.Project
import Audacity.Playback

DropArea {
    id: root

    property var tracksItemsView: null
    property var tracksViewState: null
    property var timeline: null

    signal setGuidelineRequested(var pos, bool visibility)

    QtObject {
        id: prv

        property var lastProbedUrls: null
    }

    DropController {
        id: dropController
    }

    Timer {
        id: clearPreviewClipsTimer
        interval: 100
        onTriggered: {
            tracksItemsView.clearPreviewImportClip([])
            prv.lastProbedUrls = null
            dropController.endImportDrag()
            root.setGuidelineRequested(-1, false)
        }
    }

    onEntered: drop => {
        clearPreviewClipsTimer.stop()

        let urls = drop.urls
        dropController.startImportDrag()
        if (!prv.lastProbedUrls) {
            // NOTE: working with urls list from DropArea
            // is expensive so avoid it otherwise the preview clip
            // move will be laggy
            dropController.probeAudioFilesLength(urls)
            prv.lastProbedUrls = urls
        }

        var trackId = tracksViewState.trackAtPosition(drop.x, drop.y)
        dropController.prepareConditionalTracks(trackId, urls.length)
        dropController.removeDragAddedTracks(trackId, urls.length)

        let tracksIds = dropController.draggedTracksIds(trackId, urls.length)
        tracksItemsView.clearPreviewImportClip(tracksIds /* tracks not to clear */)
        const durations  = dropController.lastProbedDurations();
        const titles     = dropController.lastProbedFileNames();

        tracksItemsView.previewImportClipRequested(tracksIds, drop.x, durations, titles);

        root.setGuidelineRequested(drop.x, true)
    }

    onExited: {
        clearPreviewClipsTimer.start()
    }

    onPositionChanged: {
        // NOTE! Qt does not reliably send onPositionChanged for external drags
        // it is expected that Qt may trigger entered/exited signals alternately
        // instead of positionChanged
    }

    onDropped: drop => {
        // Forces conversion to a compatible array
        let urls = drop.urls.concat([]);

        let trackId = tracksViewState.trackAtPosition(drop.x, drop.y)
        let tracksIds = dropController.draggedTracksIds(trackId, urls.length)
        dropController.handleDroppedFiles(tracksIds, timeline.context.positionToTime(drop.x), urls)

        dropController.endImportDrag()
        drop.acceptProposedAction()

        root.setGuidelineRequested(-1, false)
        prv.lastProbedUrls = null
    }
}
