import QtQuick
import QtQuick.Shapes

import Muse.Ui

import Audacity.ProjectScene

//! A fade of a clip, with its handle. Spans the area in which the fade is drawn.
Item {
    id: root

    required property bool isFadeIn
    required property real fadeWidth
    //! Where the fade starts: possibly far outside this item, which only spans the visible part of the clip
    required property real fadeLeft
    required property int fadeType
    //! Above it, the fade isn't drawn
    property real drawingTop: 0

    property alias handleX: handle.x
    property alias handleY: handle.y
    property real handleSize: 32
    property alias handleVisible: handle.handleVisible

    readonly property alias pressed: handle.pressed

    signal fadeEditStarted
    signal fadeWidthChangeRequested(real width, bool completed)
    signal fadeEditCanceled

    // in this item's coordinates
    signal mousePositionChanged(real x, real y)

    QtObject {
        id: prv

        //! The part of the fade within this item: when zoomed in, drawing all of it would mean huge coordinates
        readonly property real visibleLeft: Math.max(root.fadeLeft, 0)
        readonly property real visibleRight: Math.min(root.fadeLeft + root.fadeWidth, root.width)

        readonly property var linePoints: {
            const width = root.fadeWidth
            const visibleWidth = visibleRight - visibleLeft
            if (width <= 0 || visibleWidth <= 0) {
                return []
            }
            // PathPolyline will linearly interpolate between datapoints, so we can afford some
            // downsampling here:
            const segmentCount = Math.max(1, Math.ceil(visibleWidth / 2))
            const positions = []
            for (let i = 0; i <= segmentCount; ++i) {
                positions.push((visibleLeft + i / segmentCount * visibleWidth - root.fadeLeft) / width)
            }
            // From the silent end of the fade
            const gains = ClipFadeGains.gainsAt(root.fadeType, root.isFadeIn ? positions : positions.map(p => 1 - p))
            return positions.map((p, i) => Qt.point(root.fadeLeft + p * width, root.height * (1 - gains[i])))
        }

        //! The faded-out part, above the line: the line closed by the top of this item
        readonly property var shadingPoints: linePoints.length ? [Qt.point(visibleLeft, 0)].concat(linePoints).concat([Qt.point(visibleRight, 0)]) : []
    }

    // the boundary line is drawn just inside the fade
    ClipFadeBoundaryLine {
        y: root.drawingTop
        height: root.height - root.drawingTop
        x: root.isFadeIn ? root.fadeLeft + root.fadeWidth - 1 : root.fadeLeft
        visible: root.fadeWidth >= 1
        highlighted: handle.pressed
    }

    // Spans the area below `drawingTop`
    Item {
        y: root.drawingTop
        width: root.width
        height: Math.max(0, root.height - root.drawingTop)
        clip: true

        // Drawn in this item's coordinates: the line spans the whole fade, from the clip's edge to the fade boundary
        Shape {
            y: -root.drawingTop
            width: root.width
            height: root.height

            visible: prv.linePoints.length > 0
            preferredRendererType: Shape.CurveRenderer

            // Shades the faded-out part, above the line
            ShapePath {
                strokeColor: "transparent"
                fillColor: Qt.rgba(0, 0, 0, 0.2)

                PathPolyline {
                    path: prv.shadingPoints
                }
            }

            ShapePath {
                strokeColor: Qt.rgba(0, 0, 0, 0.5)
                strokeWidth: 1
                capStyle: ShapePath.FlatCap
                fillColor: "transparent"

                PathPolyline {
                    path: prv.linePoints
                }
            }

            // Highlights the line while the handle is dragged
            ShapePath {
                strokeColor: handle.pressed ? ui.theme.extra["white_color"] : "transparent"
                strokeWidth: 1.5
                capStyle: ShapePath.FlatCap
                fillColor: "transparent"

                PathPolyline {
                    path: prv.linePoints
                }
            }
        }
    }

    ClipFadeHandle {
        id: handle

        width: root.handleSize
        height: root.handleSize

        isFadeIn: root.isFadeIn
        fadeWidth: root.fadeWidth
        fadeLeft: root.fadeLeft

        onFadeEditStarted: root.fadeEditStarted()

        onFadeWidthChangeRequested: function (width, completed) {
            root.fadeWidthChangeRequested(width, completed)
        }

        onFadeEditCanceled: root.fadeEditCanceled()

        onMousePositionChanged: function (mouseX, mouseY) {
            root.mousePositionChanged(mouseX, mouseY)
        }
    }
}
