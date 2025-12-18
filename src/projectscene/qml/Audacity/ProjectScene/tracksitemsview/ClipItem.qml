import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents
import Muse.GraphicalEffects

import Audacity.ProjectScene
import Audacity.Playback
import Audacity.Spectrogram

Rectangle {
    id: root

    property alias context: waveView.context
    property alias clipKey: waveView.clipKey
    property alias clipTime: waveView.clipTime
    property alias title: titleLabel.text
    required property bool isWaveformViewVisible
    required property bool isSpectrogramViewVisible
    property int pitch: 0
    property int speedPercentage: 0
    property bool showChannelSplitter: false
    property alias channelHeightRatio: waveChannelSplitter.channelHeightRatio
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
    property bool clipIntersectsSelection: false
    property bool isDataSelected: false
    property bool isMultiSelectionActive: false
    property bool multiClipsSelected: root.isMultiSelectionActive && root.clipSelected
    property bool moveActive: false
    property bool isAudible: true
    property bool isLinear: false
    property real dbRange: -60.0
    property var displayBounds: ({ "min": -1.0, "max": 1.0 })
    property real selectionStart: 0
    property real selectionWidth: 0
    property bool selectionInProgress: false
    property bool enableCursorInteraction: !selectionInProgress && !isBrush
    property bool isContrastFocusBorderEnabled: false

    property real distanceToLeftNeighbor: -1
    property real distanceToRightNeighbor: -1

    property real leftVisibleMargin: 0
    property real rightVisibleMargin: 0

    property bool collapsed: false

    property bool multiSampleEdit: false

    property bool asymmetricStereoHeightsPossible: false

    signal clipStartEditRequested
    signal clipEndEditRequested
    signal cancelClipDragEditRequested

    signal clipLeftTrimRequested(bool completed, int action)
    signal clipRightTrimRequested(bool completed, int action)
    signal clipLeftStretchRequested(bool completed, int action)
    signal clipRightStretchRequested(bool completed, int action)

    signal requestSelected
    signal requestSelectionReset
    signal splitterPositionChangeRequested(int position)

    signal pitchChangeRequested
    signal pitchResetRequested

    signal speedChangeRequested
    signal speedResetRequested

    signal titleEditStarted
    signal titleEditAccepted(var newTitle)
    signal titleEditCanceled

    signal startAutoScroll
    signal stopAutoScroll

    // mouse position event is not propagated on overlapping mouse areas
    // so we are handling it manually
    signal clipItemMousePositionChanged(real x, real y)

    property alias navigation: navCtrl

    radius: 4
    color: clipSelected ? "white" : clipColor
    border.color: "#000000"
    opacity: root.moveActive && (clipSelected || clipIntersectsSelection) ? 0.5 : isAudible ? 1.0 : 0.3

    onMoveActiveChanged: {
        root.parent.z = moveActive && (clipSelected || clipIntersectsSelection) ? 1 : 0
    }

    property int borderWidth: 1
    property bool hover: root.containsMouse || root.headerHovered
    property bool headerHovered: false
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
    property bool enableFocusBorder: (navCtrl ? navCtrl.highlight : false) || root.clipSelected
    property bool enableDefaultBorder: !isContrastFocusBorderEnabled && enableFocusBorder
    property bool enableContrastBorder: isContrastFocusBorderEnabled && enableFocusBorder

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

        onActiveChanged: function (active) {
            // Make sure the focus navigation border is visible on top of other clips
            root.parent.z = active ? 1 : 0
            if (active) {
                root.forceActiveFocus()
            }
        }

        onTriggered: {
            clipNavigationPanel.requestActive()
        }
    }

    Rectangle {
        id: borderRect

        anchors.fill: parent
        color: "transparent"
        border.width: root.enableContrastBorder ? 2 : 1
        border.color: "#000000"
        radius: root.enableContrastBorder ? 0 : 4
        z: root.parent.z + 1
    }

    NavigationFocusBorder {
        id: focusBorder

        navigationCtrl: navCtrl

        border.color: ui.theme.fontPrimaryColor
        border.width: 2
        radius: 4

        visible: root.enableDefaultBorder
    }

    NavigationFocusBorder {
        id: contrastFocusBorder

        navigationCtrl: navCtrl

        border.color: "white"
        border.width: 2
        radius: 4

        visible: root.enableContrastBorder
    }

    QtObject {
        id: prv

        readonly property int doubleClickInterval: 400
        readonly property int doubleClickMaxDistance: 5
    }

    // panel for navigating within the clip's items
    property NavigationPanel clipNavigationPanel: NavigationPanel {
        name: "ClipNavigationPanel"
        enabled: navCtrl.active
        direction: NavigationPanel.Horizontal
        section: navigation.panel ? navigation.panel.section : null
        onActiveChanged: function (active) {
            if (active) {
                root.forceActiveFocus()
            }
        }

        onNavigationEvent: function (event) {
            if (event.type === NavigationEvent.Escape && !clipHandles.leftTrimActive && !clipHandles.rightTrimActive && !clipHandles.leftStretchActive && !clipHandles.rightStretchActive) {
                navCtrl.requestActive()
            }
        }
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
        root.altPressed ? waveView.smoothLastClickPos(x, y - header.height) : waveView.setLastClickPos(x, y - header.height, x, y - header.height)
        waveView.update()
    }

    function mouseReleased() {
        waveView.isNearSample = false
        waveView.onWaveViewPositionChanged(lastSample.x, lastSample.y)
    }

    function setLastSample(x, y) {
        lastSample = {
            x: x,
            y: y - header.height
        }
    }

    function setContainsMouse(containsMouse) {
        if (!root.enableCursorInteraction) {
            return
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

        onHandleMenuItem: function (itemId) {
            singleClipContextMenuModel.handleMenuItem(itemId)
        }
    }

    MultiClipContextMenuModel {
        id: multiClipContextMenuModel
    }

    ContextMenuLoader {
        id: multiClipContextMenuLoader

        onHandleMenuItem: function (itemId) {
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
        root.headerHovered = false
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

        onVisibleChanged: {
            root.setContainsMouse(containsMouse)
        }

        onClicked: function (e) {
            if (root.multiClipsSelected) {
                multiClipContextMenuLoader.show(Qt.point(e.x, e.y), multiClipContextMenuModel.items)
            } else {
                singleClipContextMenuLoader.show(Qt.point(e.x, e.y), singleClipContextMenuModel.items)
                root.requestSelected()
            }
        }

        onPositionChanged: function (e) {
            clipItemMousePositionChanged(e.x, e.y);

            // propagate mouse position to the wave view adjusting the y position
            waveView.onWaveViewPositionChanged(e.x, e.y - header.height)
        }

        onContainsMouseChanged: {
            if (!root.visible) {
                return
            }

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

        onPressed: function (e) {
            root.clipStartEditRequested()
        }

        onReleased: function (e) {
            if (e.modifiers & (Qt.AltModifier | Qt.MetaModifier)) {
                root.clipLeftStretchRequested(true, ClipBoundaryAction.Shrink)
            } else {
                root.clipLeftTrimRequested(true, ClipBoundaryAction.Shrink)
            }

            root.stopAutoScroll()
            // this needs to be always at the very end
            root.clipEndEditRequested()
        }

        onPositionChanged: function (e) {
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

        onCanceled: e => {
            root.cancelClipDragEditRequested()
        }
    }

    MouseArea {
        id: rightTrimStretchEdgeHover

        x: root.width - 5
        z: headerDragArea.z + 1
        width: distanceToRightNeighbor >= -0.5 && distanceToRightNeighbor <= 10 ? 6 + Math.min(distanceToRightNeighbor / 2, 5) : 11
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

        onPressed: function (e) {
            root.clipStartEditRequested()
        }

        onReleased: function (e) {
            if (e.modifiers & (Qt.AltModifier | Qt.MetaModifier)) {
                root.clipRightStretchRequested(true, ClipBoundaryAction.Shrink)
            } else {
                root.clipRightTrimRequested(true, ClipBoundaryAction.Shrink)
            }

            root.stopAutoScroll()
            // this needs to be always at the very end
            root.clipEndEditRequested()
        }

        onPositionChanged: function (e) {
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

        onCanceled: e => {
            root.cancelClipDragEditRequested()
        }
    }

    Rectangle {
        id: inner

        anchors.fill: parent

        layer.enabled: true
        layer.effect: RoundedCornersEffect {
            radius: root.radius
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

                onContainsMouseChanged: {
                    if (!root.visible) {
                        return
                    }

                    root.headerHovered = containsMouse
                }

                visible: root.enableCursorInteraction

                acceptedButtons: Qt.LeftButton
                hoverEnabled: true
                cursorShape: Qt.OpenHandCursor

                property var lastClickTime: 0
                property point doubleClickStartPosition

                //! IMPORTANT NOTE: clip moving is handled in TracksItemsView (because
                // Clip UI element will be destroyed along with its MouseArea if the clip is moved
                // to the other track and we're gonna loose the ability to handle this MouseArea's events)
                // hence we need to let simple events pass (e.accepted = false). Unfortunately this breaks
                // detecting composed events like doubleClick so we need to take care of it manually.
                onPressed: function (e) {
                    var currentTime = Date.now()
                    if (currentTime - lastClickTime < prv.doubleClickInterval) {
                        //! NOTE Handle doubleClick logic
                        root.editTitle()
                    } else {
                        //! NOTE Handle singleClick logic
                        if (!root.multiClipsSelected && !isWithinRange(e.x, headerSelectionRectangle.x, headerSelectionRectangle.width)) {
                            root.requestSelected()
                        }

                        lastClickTime = currentTime
                        doubleClickStartPosition = Qt.point(e.x, e.y)
                    }

                    e.accepted = false
                }

                onPositionChanged: function (e) {
                    // Reset double click timer if the mouse has moved,
                    // to prevent rapid clip movement activate title editing
                    if (Math.abs(e.x - doubleClickStartPosition.x) > prv.doubleClickMaxDistance || Math.abs(e.y - doubleClickStartPosition.y) > prv.doubleClickMaxDistance) {
                        lastClickTime = 0
                    }

                    root.clipItemMousePositionChanged(e.x, e.y)

                    e.accepted = false
                }

                onReleased: function (e) {
                    e.accepted = false
                }

                function isWithinRange(val, x, w) {
                    return val >= x && val <= (x + w)
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

                    onTextChanged: function (text) {
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

                    visible: root.pitch !== 0 && header.width > (60 + pitchBtn.implicitWidth + speedBtn.implicitWidth * (root.speedPercentage !== 100.0) + menuBtn.implicitWidth)

                    onClicked: function (mouse) {
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

                    visible: root.speedPercentage !== 100.0 && header.width > (60 + speedBtn.implicitWidth + menuBtn.implicitWidth)

                    onClicked: function (mouse) {
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

                    visible: header.width > (60 + menuBtn.implicitWidth)

                    navigation.name: "ClipMenuBtn"
                    navigation.panel: root.clipNavigationPanel
                    navigation.column: 4

                    onHandleMenuItem: function (itemId) {
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

        ColumnLayout {
            id: viewsColumn

            anchors.top: (!root.collapsed && header.visible) ? header.bottom : parent.top
            anchors.left: parent.left
            anchors.right: parent.right
            anchors.bottom: parent.bottom

            spacing: 0

            WaveView {
                id: waveView
                visible: root.isWaveformViewVisible

                Layout.fillWidth: true
                Layout.fillHeight: true

                channelHeightRatio: showChannelSplitter ? root.channelHeightRatio : 1

                clipColor: root.clipColor
                clipSelected: root.clipSelected
                isIsolationMode: root.isIsolationMode
                multiSampleEdit: root.multiSampleEdit
                isBrush: root.isBrush
                isLinear: root.isLinear
                dbRange: root.dbRange
                displayBounds: root.displayBounds

                function onWaveViewPositionChanged(x, y) {
                    if (waveView.isIsolationMode) {
                        waveView.setIsolatedPoint(x, y)
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
                    id: waveChannelSplitter

                    anchors.fill: parent

                    visible: root.showChannelSplitter

                    editable: root.enableCursorInteraction && root.asymmetricStereoHeightsPossible
                    asymmetricStereoHeightsPossible: root.asymmetricStereoHeightsPossible

                    color: "#000000"
                    opacity: 0.10

                    onPositionChangeRequested: function (position) {
                        root.splitterPositionChangeRequested(position)
                    }
                }

                onIsNearSampleChanged: {
                    if (root.isNearSample) {
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

            SpectrogramView {
                visible: root.isSpectrogramViewVisible

                Layout.fillWidth: true
                Layout.fillHeight: true

                clipId: root.clipKey.itemId()
                trackId: root.clipKey.trackId()

                timelineIndentWidth: root.canvas.anchors.leftMargin
                channelHeightRatio: showChannelSplitter ? root.channelHeightRatio : 1
                zoom: root.context.zoom
                frameStartTime: root.context.frameStartTime
                frameEndTime: root.context.frameEndTime
                selectionStartTime: root.context.selectionStartTime
                selectionEndTime: root.context.selectionEndTime
                spectralSelectionStartFrequency: root.context.spectralSelectionStartFrequency
                spectralSelectionEndFrequency: root.context.spectralSelectionEndFrequency

                ChannelSplitter {
                    id: spectrogramChannelSplitter

                    anchors.fill: parent

                    visible: root.showChannelSplitter

                    channelHeightRatio: parent.channelHeightRatio
                    editable: root.enableCursorInteraction && root.asymmetricStereoHeightsPossible
                    asymmetricStereoHeightsPossible: root.asymmetricStereoHeightsPossible

                    color: "#000000"
                    opacity: 0.10

                    onPositionChangeRequested: function (position) {
                        root.splitterPositionChangeRequested(position)
                    }
                }
            }
        }

        FlatButton {
            id: accessibilitySelectBtn

            anchors.horizontalCenter: viewsColumn.horizontalCenter
            anchors.bottom: viewsColumn.bottom
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

        onClipHandlesMousePositionChanged: function (xWithinClipHandles, yWithinClipHandles) {
            var xWithinClipItem = xWithinClipHandles
            var yWithinClipItem = header.height + 1 + yWithinClipHandles
            clipItemMousePositionChanged(xWithinClipItem, yWithinClipItem)
        }

        onClipStartEditRequested: function () {
            root.clipStartEditRequested()
        }

        onClipEndEditRequested: function () {
            root.clipEndEditRequested()
        }

        onCancelClipDragEditRequested: function () {
            root.cancelClipDragEditRequested()
        }

        onTrimLeftRequested: function (completed, action) {
            root.clipLeftTrimRequested(completed, action)
        }

        onTrimRightRequested: function (completed, action) {
            root.clipRightTrimRequested(completed, action)
        }

        onStretchLeftRequested: function (completed, action) {
            root.clipLeftStretchRequested(completed, action)
        }

        onStretchRightRequested: function (completed, action) {
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
            PropertyChanges {
                target: header
                color: root.normalHeaderColor
            }
            PropertyChanges {
                target: titleLabel
                color: "#000000"
            }
            PropertyChanges {
                target: pitchBtn
                textColor: "#000000"
                iconColor: "#000000"
            }
            PropertyChanges {
                target: speedBtn
                textColor: "#000000"
                iconColor: "#000000"
            }
            PropertyChanges {
                target: menuBtn
                iconColor: "#000000"
            }
        },
        State {
            name: "SELECTED"
            when: root.clipSelected && !headerDragArea.containsMouse
            PropertyChanges {
                target: header
                color: root.selectedHeaderColor
            }
            PropertyChanges {
                target: titleLabel
                color: "#000000"
            }
            PropertyChanges {
                target: pitchBtn
                textColor: "#000000"
                iconColor: "#000000"
            }
            PropertyChanges {
                target: speedBtn
                textColor: "#000000"
                iconColor: "#000000"
            }
            PropertyChanges {
                target: menuBtn
                iconColor: "#000000"
            }
        },
        State {
            name: "NORMAL_HEADER_HOVERED"
            when: !root.clipSelected && headerDragArea.containsMouse
            PropertyChanges {
                target: header
                color: root.normalHeaderHoveredColor
            }
            PropertyChanges {
                target: titleLabel
                color: "#000000"
            }
            PropertyChanges {
                target: pitchBtn
                textColor: "#000000"
                iconColor: "#000000"
            }
            PropertyChanges {
                target: speedBtn
                textColor: "#000000"
                iconColor: "#000000"
            }
            PropertyChanges {
                target: menuBtn
                iconColor: "#000000"
            }
        },
        State {
            name: "SELECTED_HEADER_HOVERED"
            when: root.clipSelected && headerDragArea.containsMouse
            PropertyChanges {
                target: header
                color: root.selectedHeaderHoveredColor
            }
            PropertyChanges {
                target: titleLabel
                color: "#000000"
            }
            PropertyChanges {
                target: pitchBtn
                textColor: "#000000"
                iconColor: "#000000"
            }
            PropertyChanges {
                target: speedBtn
                textColor: "#000000"
                iconColor: "#000000"
            }
            PropertyChanges {
                target: menuBtn
                iconColor: "#000000"
            }
        }
    ]
}
