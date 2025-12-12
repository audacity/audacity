import QtQuick
import QtQuick.Controls

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene
import Audacity.Project
import Audacity.Playback

DropArea {
    id: root

    anchors.fill: content

    property var lastProbedUrls: null

    property var tracksItemsView: null
    property var tracksViewState: null
    property var content: null
    property var timeline: null

    signal setGuideline(var pos, bool visibility)

    DropController {
        id: dropController
    }

    Timer {
        id: clearPreviewClipsTimer
        interval: 100
        onTriggered: {
            tracksItemsView.clearPreviewImportClip([])
            root.lastProbedUrls = null
            dropController.endImportDrag()
            root.setGuideline(-1, false)
        }
    }

    onEntered: drop => {
        clearPreviewClipsTimer.stop()

        let urls = drop.urls
        dropController.startImportDrag()
        if (!lastProbedUrls) {
            // NOTE: working with urls list from DropArea
            // is expensive so avoid it otherwise the preview clip
            // move will be laggy
            dropController.probeAudioFilesLength(urls)
            root.lastProbedUrls = urls
        }

        let position = mapToItem(content, Qt.point(drop.x, drop.y))

        var trackId = tracksViewState.trackAtPosition(position.x, position.y)
        dropController.prepareConditionalTracks(trackId, urls.length)
        dropController.removeDragAddedTracks(trackId, urls.length)

        let tracksIds = dropController.draggedTracksIds(trackId, urls.length)
        tracksItemsView.clearPreviewImportClip(tracksIds /* tracks not to clear */)
        const durations  = dropController.lastProbedDurations();
        const titles     = dropController.lastProbedFileNames();

        tracksItemsView.previewImportClipRequested(tracksIds, position.x, durations, titles);

        root.setGuideline(position.x, true)
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

        let position = mapToItem(content, Qt.point(drop.x, drop.y))
        let trackId = tracksViewState.trackAtPosition(position.x, position.y)
        let tracksIds = dropController.draggedTracksIds(trackId, urls.length)
        dropController.handleDroppedFiles(tracksIds, timeline.context.positionToTime(position.x), urls)

        dropController.endImportDrag()
        drop.acceptProposedAction()

        root.setGuideline(-1, false)
        root.lastProbedUrls = null
    }
}
