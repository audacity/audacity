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

    signal externalDropAreaEntered(var drop)
    signal externalDropAreaExitted()
    signal externalDropAreaDropped(var drop)

    onExternalDropAreaEntered: drop => {
        handleOnEntered(drop, true)
    }

    onExternalDropAreaExitted: {
        clearPreviewClipsTimer.start()
    }

    onExternalDropAreaDropped: drop => {
        handleOnDropped(drop, true)
    }

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
        handleOnEntered(drop, false)
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
        handleOnDropped(drop, false)
    }

    function handleOnEntered(drop, externalDrop) {
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

        let dropX = 0
        let dropY = drop.y
        if (!externalDrop) {
            dropX = drop.x
            dropY -= root.timeline.height
        }

        var trackId = tracksViewState.trackAtPosition(dropX, dropY)
        dropController.prepareConditionalTracks(trackId, urls.length)
        dropController.removeDragAddedTracks(trackId, urls.length)

        let tracksIds = dropController.draggedTracksIds(trackId, urls.length)
        tracksItemsView.clearPreviewImportClip(tracksIds /* tracks not to clear */)
        const durations  = dropController.lastProbedDurations();
        const titles     = dropController.lastProbedFileNames();

        tracksItemsView.previewImportClipRequested(tracksIds, dropX, durations, titles);

        root.setGuidelineRequested(dropX, true)
    }

    function handleOnDropped(drop, externalDrop) {
        // Forces conversion to a compatible array
        let urls = drop.urls.concat([]);

        let dropX = 0
        let dropY = drop.y
        if (!externalDrop) {
            dropX = drop.x
            dropY -= root.timeline.height
        }

        let trackId = tracksViewState.trackAtPosition(dropX, dropY)
        let tracksIds = dropController.draggedTracksIds(trackId, urls.length)
        dropController.handleDroppedFiles(tracksIds, timeline.context.positionToTime(dropX), urls)

        dropController.endImportDrag()
        drop.acceptProposedAction()

        root.setGuidelineRequested(-1, false)
        prv.lastProbedUrls = null
    }
}
