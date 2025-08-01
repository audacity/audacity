import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents
import Muse.GraphicalEffects

import Audacity.ProjectScene
import Audacity.Playback

Rectangle {

    id: root

    property alias context: waveView.context
    property alias clipKey: waveView.clipKey
    property alias clipTime: waveView.clipTime
    property alias title: titleLabel.text
    property int pitch: 0
    property int speedPercentage: 0
    property alias showChannelSplitter: channelSplitter.visible
    property alias channelHeightRatio: channelSplitter.channelHeightRatio
    property var canvas: null
    property color clipColor: "#677CE4"
    property color normalHeaderColor: root.currentClipStyle == ClipStyle.COLORFUL ? root.clipColor : root.classicHeaderColor
    property color selectedHeaderColor: root.currentClipStyle == ClipStyle.COLORFUL ? ui.blendColors("#ffffff", root.clipColor, 0.3) : classicHeaderColor
    property color normalHeaderHoveredColor: root.currentClipStyle == ClipStyle.COLORFUL ? ui.blendColors("#ffffff", root.clipColor, 0.8) : classicHeaderHoveredColor
    property color selectedHeaderHoveredColor: root.currentClipStyle == ClipStyle.COLORFUL ? ui.blendColors("#ffffff", root.clipColor, 0.2) : classicHeaderHoveredColor
    readonly property color classicHeaderColor: "#D0D6F2"
    readonly property color classicHeaderHoveredColor: "#B0B6D8"
    property int currentClipStyle: ClipStyle.COLORFUL
    property int groupId: -1
    property bool clipSelected: false
    property bool isDataSelected: false
    property bool isMultiSelectionActive: false
    property bool multiClipsSelected: root.isMultiSelectionActive && root.clipSelected
    property bool moveActive: false
    property bool isAudible: true
    property real selectionStart: 0
    property real selectionWidth: 0
    property bool selectionInProgress: false
    property bool enableCursorInteraction: !selectionInProgress && !isBrush

    property real distanceToLeftNeighbor: -1
    property real distanceToRightNeighbor: -1

    property real leftVisibleMargin: 0
    property real rightVisibleMargin: 0

    property bool collapsed: false

    property bool multiSampleEdit: false

    property bool asymmetricStereoHeightsPossible: false

    signal clipStartEditRequested()
    signal clipEndEditRequested()

    signal clipLeftTrimRequested(bool completed, int action)
    signal clipRightTrimRequested(bool completed, int action)
    signal clipLeftStretchRequested(bool completed, int action);
    signal clipRightStretchRequested(bool completed, int action);

    signal requestSelected()
    signal requestSelectionReset()
    signal splitterPositionChangeRequested(int position)

    signal pitchChangeRequested()
    signal pitchResetRequested()

    signal speedChangeRequested()
    signal speedResetRequested()

    signal titleEditStarted()
    signal titleEditAccepted(var newTitle)
    signal titleEditCanceled()

    signal startAutoScroll()
    signal stopAutoScroll()

    // mouse position event is not propagated on overlapping mouse areas
    // so we are handling it manually
    signal clipItemMousePositionChanged(real x, real y)
    signal clipHeaderHoveredChanged(bool value)

    property alias navigation: navCtrl

    radius: 4
    color: clipSelected ? "white" : clipColor
    border.color: "#000000"
    opacity: root.moveActive && clipSelected ? 0.5 : isAudible ? 1.0 : 0.3

    property int borderWidth: 1
    property bool hover: root.containsMouse || headerDragArea.containsMouse
    property bool headerHovered: headerDragArea.containsMouse
    property var lastSample: undefined
    property bool altPressed: false
    property bool isBrush: waveView.isStemPlot && root.altPressed
    property bool isIsolationMode: false
    property bool containsMouse: false
    property alias isNearSample: waveView.isNearSample
    property alias currentChannel: waveView.currentChannel
    property bool leftTrimContainsMouse: false
    property bool rightTrimContainsMouse: false
    property alias leftTrimPressedButtons: leftTrimStretchEdgeHover.pressedButtons
    property alias rightTrimPressedButtons: rightTrimStretchEdgeHover.pressedButtons

    PlaybackStateModel {
        id: playbackState
    }

    // for navigating between clips
    NavigationControl {
        id: navCtrl
        name: root.name
        enabled: root.enabled && root.visible

        accessible.role: MUAccessible.Button
        accessible.name: root.name

        onActiveChanged: function(active) {
            if (active) {
                root.forceActiveFocus()
            }
        }

        onTriggered: {
            clipNavigationPanel.requestActive()
        }
    }

    NavigationFocusBorder {
        navigationCtrl: navCtrl
    }

    // panel for navigating within the clip's items
    property NavigationPanel clipNavigationPanel: NavigationPanel {
        name: "ClipNavigationPanel"
        enabled: navCtrl.active
        direction: NavigationPanel.Horizontal
        section: navigation.panel.section
        onActiveChanged: function(active) {
            if (active) {
                root.forceActiveFocus()
            }
        }

        onNavigationEvent: function(event) {
            if (event.type === NavigationEvent.Escape && !clipHandles.leftTrimActive
                    && !clipHandles.rightTrimActive && !clipHandles.leftStretchActive
                    && !clipHandles.rightStretchActive) {
                navCtrl.requestActive()
            }
        }
    }

    onHeaderHoveredChanged: {
        root.clipHeaderHoveredChanged(headerHovered)
    }

    onAsymmetricStereoHeightsPossibleChanged: {
        if (!asymmetricStereoHeightsPossible) {
            root.ratioChanged(showChannelSplitter ? 0.5 : 1)
        }
    }

    function editTitle() {
        editLoader.edit(titleLabel.text)
    }

    function acceptEditTitle(newTitle) {
        Qt.callLater(root.titleEditAccepted, newTitle)
    }

    function mousePositionChanged(x, y) {
        clipItemMousePositionChanged(x, y)
        waveView.onWaveViewPositionChanged(x, y - header.height)
    }

    function mousePressAndHold(x, y) {
        root.altPressed
            ? waveView.smoothLastClickPos(x, y - header.height)
            : waveView.setLastClickPos(x, y - header.height, x, y - header.height)
        waveView.update()
    }

    function mouseReleased() {
        waveView.isNearSample = false
        waveView.onWaveViewPositionChanged(lastSample.x, lastSample.y)
    }

    function setLastSample(x, y) {
        lastSample = {x: x, y: y - header.height}
    }

    function setContainsMouse(containsMouse) {
        if (!root.enableCursorInteraction) {
            return;
        }

        root.containsMouse = containsMouse
        if (!root.containsMouse && !root.multiSampleEdit) {
            waveView.isNearSample = false
        }
    }

    function updateWave() {
        waveView.update()
    }

    ClipContextMenuModel {
        id: singleClipContextMenuModel
        clipKey: root.clipKey

        onClipTitleEditRequested: {
            root.editTitle()
        }
    }

    ContextMenuLoader {
        id: singleClipContextMenuLoader

        onHandleMenuItem: function(itemId) {
            singleClipContextMenuModel.handleMenuItem(itemId)
        }
    }

    MultiClipContextMenuModel {
        id: multiClipContextMenuModel
    }

    ContextMenuLoader {
        id: multiClipContextMenuLoader

        onHandleMenuItem: function(itemId) {
            multiClipContextMenuModel.handleMenuItem(itemId)
        }
    }

    Component.onCompleted: {
        singleClipContextMenuModel.load()
        multiClipContextMenuModel.load()
    }

    Component.onDestruction: {
        //! NOTE The outer component uses this information to handle the current cursor.
        // It is important to cleanup this state before removing the component
        // to prevent the cursor from being stuck in the wrong state.
        waveView.isNearSample = false
        waveView.isStemPlot = false
        root.leftTrimContainsMouse = false
        root.rightTrimContainsMouse = false
    }

    MouseArea {
        id: hoverArea
        anchors.fill: parent

        hoverEnabled: true
        cursorShape: {
            // Show forbidden cursor during playback for sample editing
            if ((root.isNearSample || root.isIsolationMode) && playbackState.isPlaying) {
                return Qt.ForbiddenCursor
            }
            return Qt.IBeamCursor
        }
        acceptedButtons: Qt.RightButton

        visible: root.enableCursorInteraction

        onClicked: function(e) {
            if (root.multiClipsSelected) {
                multiClipContextMenuLoader.show(Qt.point(e.x, e.y), multiClipContextMenuModel.items)
            } else {
                singleClipContextMenuLoader.show(Qt.point(e.x, e.y), singleClipContextMenuModel.items)
                root.requestSelected()
            }
        }

        onPositionChanged: function (e) {
            clipItemMousePositionChanged(e.x, e.y)

            // propagate mouse position to the wave view adjusting the y position
            waveView.onWaveViewPositionChanged(e.x, e.y - header.height)
        }

        onContainsMouseChanged: {
            root.setContainsMouse(containsMouse)
        }
    }

    MouseArea {
        id: leftTrimStretchEdgeHover

        x: distanceToLeftNeighbor >= -0.5 && distanceToLeftNeighbor <= 10 ? root.x - Math.min(distanceToLeftNeighbor / 2, 5) : root.x - 5
        width: distanceToLeftNeighbor >= -0.5 && distanceToLeftNeighbor <= 10 ? 6 + Math.min(distanceToLeftNeighbor / 2, 5) : 11
        z: headerDragArea.z + 1
        height: !root.collapsed ? root.height / 3 : root.height / 2

        anchors.top: root.top

        hoverEnabled: true
        visible: !root.clipSelected && root.enableCursorInteraction

        cursorShape: Qt.BlankCursor

        // make sure cursor is visible on top of nearby clips
        onContainsMouseChanged: {
            root.leftTrimContainsMouse = containsMouse
            if (containsMouse || pressedButtons) {
                root.parent.z = 1
            } else {
                root.parent.z = 0
            }
        }

        onPressed: function(e) {
            root.clipStartEditRequested()
        }

        onReleased: function(e) {
            if (e.modifiers & (Qt.AltModifier | Qt.MetaModifier)) {
                root.clipLeftStretchRequested(true, ClipBoundaryAction.Shrink)
            } else {
                root.clipLeftTrimRequested(true, ClipBoundaryAction.Shrink)
            }

            root.stopAutoScroll()
            // this needs to be always at the very end
            root.clipEndEditRequested()
        }

        onPositionChanged: function(e) {
            let mousePos = mapToItem(root, e.x, e.y)
            clipItemMousePositionChanged(mousePos.x, mousePos.y)

            if (e.modifiers & (Qt.AltModifier | Qt.MetaModifier)) {
                if (pressed) {
                    root.clipLeftStretchRequested(false, ClipBoundaryAction.Shrink)
                }
            } else {
                if (pressed) {
                    root.clipLeftTrimRequested(false, ClipBoundaryAction.Shrink)
                }
            }
        }
    }

    MouseArea {
        id: rightTrimStretchEdgeHover

        x: root.width - 5
        z: headerDragArea.z + 1
        width: distanceToRightNeighbor >= -0.5 && distanceToRightNeighbor <= 10 ? 6 + Math.min(distanceToRightNeighbor / 2, 5): 11
        height: !root.collapsed ? root.height / 3 : root.height / 2

        anchors.top: root.top

        hoverEnabled: true
        visible: !root.clipSelected && root.enableCursorInteraction

        cursorShape: Qt.BlankCursor

        // make sure cursor is visible on top of nearby clips
        onContainsMouseChanged: {
            root.rightTrimContainsMouse = containsMouse
            if (containsMouse || pressedButtons) {
                root.parent.z = 1
            } else {
                root.parent.z = 0
            }
        }

        onPressed: function(e) {
            root.clipStartEditRequested()
        }

        onReleased: function(e) {
            if (e.modifiers & (Qt.AltModifier | Qt.MetaModifier)) {
                root.clipRightStretchRequested(true, ClipBoundaryAction.Shrink)
            } else {
                root.clipRightTrimRequested(true, ClipBoundaryAction.Shrink)
            }

            root.stopAutoScroll()
            // this needs to be always at the very end
            root.clipEndEditRequested()
        }

        onPositionChanged: function(e) {
            let mousePos = mapToItem(root, e.x, e.y)
            clipItemMousePositionChanged(mousePos.x, mousePos.y)

            if (e.modifiers & (Qt.AltModifier | Qt.MetaModifier)) {
                if (pressed) {
                    root.clipRightStretchRequested(false, ClipBoundaryAction.Shrink)
                }
            } else {
                if (pressed) {
                    root.clipRightTrimRequested(false, ClipBoundaryAction.Shrink)
                }
            }
        }
    }

    Rectangle {
        id: inner

        anchors.fill: parent

        layer.enabled: true
        layer.effect: EffectOpacityMask {
            maskSource: RoundedRectangle {
                width: inner.width
                height: inner.height
                radius: root.radius
            }
        }

        Rectangle {
            id: header
            anchors.top: parent.top
            anchors.left: parent.left
            anchors.right: parent.right

            height: 20
            z: 2

            visible: !root.collapsed || root.hover

            Rectangle {
                id: headerSelectionRectangle

                x: root.selectionStart
                z: 0 // Ensure this is below the header content
                width: Math.min(root.selectionWidth, header.width)

                anchors.top: header.top
                anchors.bottom: header.bottom

                color: waveView.transformColor(clipColor)
                visible: root.isDataSelected && currentClipStyle == ClipStyle.COLORFUL
            }

            MouseArea {
                id: headerDragArea
                anchors.fill: parent

                visible: root.enableCursorInteraction

                acceptedButtons: Qt.LeftButton
                hoverEnabled: true
                cursorShape: Qt.OpenHandCursor

                property var lastClickTime: 0
                property int doubleClickInterval: 400

                //! IMPORTANT NOTE: clip moving is handled in TracksClipsView (because
                // Clip UI element will be destroyed along with its MouseArea if the clip is moved
                // to the other track and we're gonna loose the ability to handle this MouseArea's events)
                // hence we need to let simple events pass (e.accepted = false). Unfortunately this breaks
                // detecting composed events like doubleClick so we need to take care of it manually.
                onPressed: function(e) {
                    var currentTime = Date.now();
                    if (currentTime - lastClickTime < doubleClickInterval) {
                        //! NOTE Handle doubleClick logic
                        root.editTitle()
                    } else {
                        //! NOTE Handle singleClick logic
                        if (!root.multiClipsSelected) {
                            root.requestSelected()
                        }

                        lastClickTime = currentTime;
                    }

                    e.accepted = false
                }

                onPositionChanged: function(e) {
                    root.clipItemMousePositionChanged(e.x, e.y)

                    e.accepted = false
                }

                onReleased: function(e) {
                    e.accepted = false
                }
            }

            StyledTextLabel {
                id: titleLabel
                anchors.top: parent.top
                anchors.bottom: parent.bottom
                anchors.left: parent.left
                anchors.right: buttonsRow.left
                anchors.leftMargin: root.leftVisibleMargin + 4
                anchors.rightMargin: 8
                horizontalAlignment: Qt.AlignLeft

                NavigationControl {
                    id: titleEditNavCtrl
                    name: "TitleEditNavCtrl"
                    enabled: root.enabled && root.visible
                    panel: root.clipNavigationPanel
                    column: 3

                    accessible.enabled: titleEditNavCtrl.enabled

                    onTriggered: {
                        root.editTitle()
                    }
                }

                NavigationFocusBorder {
                    navigationCtrl: titleEditNavCtrl

                    anchors.topMargin: 1
                    anchors.bottomMargin: 0
                    radius: 5
                }
            }

            Loader {
                id: editLoader

                anchors.fill: titleLabel

                property bool isEditState: false
                sourceComponent: editLoader.isEditState ? titleEditComp : null

                function edit(text) {
                    root.titleEditStarted()
                    editLoader.isEditState = true
                    editLoader.item.currentText = text
                    editLoader.item.newTitle = text
                    editLoader.item.visible = true
                    editLoader.item.ensureActiveFocus()
                }
            }

            Component {
                id: titleEditComp

                TextInputField {
                    id: titleEdit

                    property string newTitle: ""

                    anchors.fill: parent
                    background.color: header.color
                    background.border.width: 0
                    background.radius: 0
                    inputField.color: titleLabel.color
                    textSidePadding: 0
                    visible: false

                    onTextChanged: function(text) {
                        titleEdit.newTitle = text
                    }

                    onAccepted: {
                        root.acceptEditTitle(titleEdit.newTitle)
                        editLoader.isEditState = false
                    }

                    onEscaped: {
                        editLoader.isEditState = false
                    }

                    onFocusChanged: {
                        if (!titleEdit.focus) {
                            titleEdit.visible = false
                            titleEdit.accepted()
                        }
                    }
                }
            }

            Row {
                id: buttonsRow

                anchors.right: parent.right
                anchors.rightMargin: 4
                anchors.verticalCenter: parent.verticalCenter

                spacing: 2

                ClipItemPropertyButton {
                    id: pitchBtn

                    mouseArea.visible: root.enableCursorInteraction

                    text: {
                        let semis = Math.trunc(root.pitch / 100)
                        let cents = Math.abs(root.pitch % 100).toString().padStart(2, "0")
                        return semis + (cents > "00" ? "." + cents : "")
                    }
                    icon: root.pitch > 0 ? IconCode.ARROW_UP : IconCode.ARROW_DOWN

                    visible: root.pitch !== 0

                    onClicked: function(mouse){
                        if (mouse.modifiers & Qt.ControlModifier) {
                            root.pitchResetRequested()
                        } else {
                            root.pitchChangeRequested()
                        }
                    }
                }

                ClipItemPropertyButton {
                    id: speedBtn

                    mouseArea.visible: root.enableCursorInteraction

                    icon: IconCode.CLOCK
                    text: root.speedPercentage + "%"

                    visible: root.speedPercentage !== 100.0

                    onClicked: function(mouse){
                        if (mouse.modifiers & Qt.ControlModifier) {
                            root.speedResetRequested()
                        } else {
                            root.speedChangeRequested()
                        }
                    }
                }

                MenuButton {
                    id: menuBtn
                    width: 16
                    height: 16
                    anchors.verticalCenter: parent.verticalCenter

                    mouseArea.visible: root.enableCursorInteraction

                    menuModel: (root.multiClipsSelected || root.groupId != -1) ? multiClipContextMenuModel : singleClipContextMenuModel

                    navigation.name: "ClipMenuBtn"
                    navigation.panel: root.clipNavigationPanel
                    navigation.column: 4

                    onHandleMenuItem: function(itemId) {
                        Qt.callLater(menuModel.handleMenuItem, itemId)
                    }

                    onClicked: {
                        if (!root.multiClipsSelected) {
                            if (!root.clipSelected) {
                                root.requestSelectionReset()
                            }
                            root.requestSelected()
                        }
                    }
                }
            }
        }

        WaveView {
            id: waveView
            anchors.top: (!root.collapsed && header.visible) ? header.bottom : parent.top
            anchors.left: parent.left
            anchors.right: parent.right
            anchors.bottom: parent.bottom

            channelHeightRatio: showChannelSplitter ? root.channelHeightRatio : 1

            clipColor: root.clipColor
            clipSelected: root.clipSelected
            isIsolationMode: root.isIsolationMode
            multiSampleEdit: root.multiSampleEdit
            isBrush: root.isBrush

            function onWaveViewPositionChanged(x, y) {
                if (waveView.isIsolationMode) {
                    waveView.setIsolatedPoint(x,y)
                    waveView.update()
                } else if (root.multiSampleEdit && !root.altPressed) {
                    var lastX = root.lastSample.x
                    var lastY = root.lastSample.y
                    waveView.setLastClickPos(lastX, lastY, x, y)
                    waveView.update()
                } else {
                    waveView.setLastMousePos(x, y)
                }
            }

            ChannelSplitter {
                id: channelSplitter

                anchors.fill: parent

                editable: root.enableCursorInteraction && root.asymmetricStereoHeightsPossible
                asymmetricStereoHeightsPossible: root.asymmetricStereoHeightsPossible

                color: "#000000"
                opacity: 0.10

                onPositionChangeRequested: function (position) {
                    root.splitterPositionChangeRequested(position)
                }
            }

            FlatButton {
                id: accessibilitySelectBtn

                anchors.horizontalCenter: parent.horizontalCenter
                anchors.bottom: parent.bottom
                anchors.bottomMargin: 10

                navigation.name: "SelectBtn"
                navigation.panel: root.clipNavigationPanel
                navigation.column: 0

                width: 55
                height: 20
                text: !root.clipSelected ? qsTrc("clips", "Select") : qsTrc("clips", "Deselect")
                visible: root.clipNavigationPanel.highlight
                normalColor: "#2b2a33"

                onClicked: {
                    if (!root.clipSelected) {
                        root.requestSelected()
                    } else {
                        root.requestSelectionReset()
                    }
                }
            }

            onIsNearSampleChanged: {
                if(root.isNearSample) {
                    waveView.forceActiveFocus()
                }
            }

            onIsIsolationModeChanged: {
                if (waveView.isIsolationMode) {
                    waveView.forceActiveFocus()
                }
            }

            onIsStemPlotChanged: {
                if (waveView.isStemPlot && hoverArea.containsMouse) {
                    // force mouse position update will update isNearSample
                    waveView.onWaveViewPositionChanged(hoverArea.mouseX, hoverArea.mouseY - header.height)
                }
            }
        }

        RoundedRectangle {
            id: clipBorder
            anchors.fill: parent
            border.width: 1
            border.color: root.clipSelected ? "white" : "black"
            color: "transparent"
            radius: root.radius
            z: 2
        }
    }

    // make sure clip and its handles are visible on top of nearby clips
    onClipSelectedChanged: {
        if (clipSelected) {
            root.parent.z = 1
        } else {
            root.parent.z = 0
        }
    }

    ClipHandles {
        id: clipHandles

        // +1 not to overlap with header
        y: header.height + 1
        width: root.width
        handlesVisible: root.clipSelected && !root.moveActive
        canvas: root.canvas

        clipNavigationPanel: root.clipNavigationPanel

        onClipHandlesMousePositionChanged: function(xWithinClipHandles, yWithinClipHandles) {
            var xWithinClipItem = xWithinClipHandles
            var yWithinClipItem = header.height + 1 + yWithinClipHandles
            clipItemMousePositionChanged(xWithinClipItem, yWithinClipItem)
        }

        onClipStartEditRequested: function() {
            root.clipStartEditRequested()
        }

        onClipEndEditRequested: function() {
            root.clipEndEditRequested()
        }

        onTrimLeftRequested: function(completed, action) {
            root.clipLeftTrimRequested(completed, action)
        }

        onTrimRightRequested: function(completed, action) {
            root.clipRightTrimRequested(completed, action)
        }

        onStretchLeftRequested: function(completed, action) {
            root.clipLeftStretchRequested(completed, action)
        }

        onStretchRightRequested: function(completed, action) {
            root.clipRightStretchRequested(completed, action)
        }

        onStopAutoScroll: {
            root.stopAutoScroll()
        }
    }

    state: "NORMAL"
    states: [
        State {
            name: "NORMAL"
            when: !root.clipSelected && !headerDragArea.containsMouse
            PropertyChanges { target: header; color: root.normalHeaderColor}
            PropertyChanges { target: titleLabel; color: "#000000"}
            PropertyChanges { target: pitchBtn; textColor: "#000000"; iconColor: "#000000" }
            PropertyChanges { target: speedBtn; textColor: "#000000"; iconColor: "#000000" }
            PropertyChanges { target: menuBtn; iconColor: "#000000"}
        },

        State {
            name: "SELECTED"
            when: root.clipSelected && !headerDragArea.containsMouse
            PropertyChanges { target: header; color: root.selectedHeaderColor }
            PropertyChanges { target: titleLabel; color: "#000000" }
            PropertyChanges { target: pitchBtn; textColor: "#000000"; iconColor: "#000000" }
            PropertyChanges { target: speedBtn; textColor: "#000000"; iconColor: "#000000" }
            PropertyChanges { target: menuBtn; iconColor: "#000000"}
        },

        State {
            name: "NORMAL_HEADER_HOVERED"
            when: !root.clipSelected && headerDragArea.containsMouse
            PropertyChanges { target: header; color: root.normalHeaderHoveredColor }
            PropertyChanges { target: titleLabel; color: "#000000"}
            PropertyChanges { target: pitchBtn; textColor: "#000000"; iconColor: "#000000" }
            PropertyChanges { target: speedBtn; textColor: "#000000"; iconColor: "#000000" }
            PropertyChanges { target: menuBtn; iconColor: "#000000"}
        },

        State {
            name: "SELECTED_HEADER_HOVERED"
            when: root.clipSelected && headerDragArea.containsMouse
            PropertyChanges { target: header; color: root.selectedHeaderHoveredColor }
            PropertyChanges { target: titleLabel; color: "#000000"}
            PropertyChanges { target: pitchBtn; textColor: "#000000"; iconColor: "#000000" }
            PropertyChanges { target: speedBtn; textColor: "#000000"; iconColor: "#000000" }
            PropertyChanges { target: menuBtn; iconColor: "#000000"}
        }
    ]
}
