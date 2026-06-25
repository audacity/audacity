import QtQuick 2.15
import Muse.Ui
import Muse.UiComponents

Item {
    id: root

    default property alias content: contentSlot.data
    property bool showGrid: true

    // xTicks / yTicks: arrays of { label: string, position: number } objects,
    // where position is a fraction in [0, 1] along the axis. For x: 0 = left,
    // 1 = right. For y: 0 = bottom, 1 = top.
    required property var xTicks
    required property var yTicks

    enum TickPosition {
        Top,
        Bottom,
        Left,
        Right
    }

    property int xTickPosition: GridPlot.Top
    property int yTickPosition: GridPlot.Right

    // When true, the first/last tick labels along an axis are aligned to the
    // edge of their grid line instead of being centered on it. Useful when the
    // outer ticks correspond to the data range bounds (e.g. EQ frequency axis).
    property bool alignEdgeLabels: false

    // Read-only geometry of the plotting background, in root's coordinate
    // space. Exposed so callers can align external items (e.g. toolbar
    // controls) to the plot edges.
    readonly property alias backgroundX: background.x
    readonly property alias backgroundY: background.y
    readonly property alias backgroundWidth: background.width
    readonly property alias backgroundHeight: background.height

    QtObject {
        id: prv

        readonly property int tickLength: 4
        readonly property int labelHeight: fontMetrics.boundingRect("0").height
        readonly property int labelMargin: 4

        function maxLabelWidth(ticks) {
            var w = 0
            for (var i = 0; i < ticks.length; ++i) {
                var bw = fontMetrics.boundingRect(ticks[i].label).width
                if (bw > w)
                    w = bw
            }
            return w
        }

        readonly property int xLabelMaxWidth: maxLabelWidth(root.xTicks)
        readonly property int yLabelMaxWidth: maxLabelWidth(root.yTicks)
    }

    FontMetrics {
        id: fontMetrics
        font.family: ui.theme.bodyFont.family
        font.pixelSize: ui.theme.bodyFont.pixelSize
    }

    Rectangle {
        id: background

        anchors.fill: parent
        anchors.topMargin: root.xTickPosition === GridPlot.Top ? prv.tickLength + prv.labelMargin + prv.labelHeight : 0
        anchors.bottomMargin: root.xTickPosition === GridPlot.Bottom ? prv.tickLength + prv.labelMargin + prv.labelHeight : 0
        anchors.leftMargin: root.yTickPosition === GridPlot.Left ? prv.tickLength + prv.labelMargin + prv.yLabelMaxWidth : 0
        anchors.rightMargin: root.yTickPosition === GridPlot.Right ? prv.tickLength + prv.labelMargin + prv.yLabelMaxWidth : 0

        color: ui.theme.extra["dynamics_background_color"]

        Repeater {
            id: verticalLines

            model: root.xTicks
            delegate: Item {
                x: background.width * modelData.position
                y: root.xTickPosition === GridPlot.Top ? -prv.tickLength : 0

                StyledTextLabel {
                    readonly property bool isFirst: root.alignEdgeLabels && index === 0
                    readonly property bool isLast: root.alignEdgeLabels && index === root.xTicks.length - 1

                    width: prv.xLabelMaxWidth
                    height: prv.labelHeight
                    horizontalAlignment: isFirst ? Text.AlignLeft : isLast ? Text.AlignRight : Text.AlignHCenter

                    anchors.horizontalCenter: !isFirst && !isLast ? vLine.horizontalCenter : undefined
                    anchors.left: isFirst ? vLine.left : undefined
                    anchors.right: isLast ? vLine.right : undefined
                    anchors.bottom: root.xTickPosition === GridPlot.Top ? vLine.top : undefined
                    anchors.top: root.xTickPosition === GridPlot.Bottom ? vLine.bottom : undefined
                    anchors.bottomMargin: root.xTickPosition === GridPlot.Top ? prv.labelMargin : undefined
                    anchors.topMargin: root.xTickPosition === GridPlot.Bottom ? prv.labelMargin : undefined

                    text: modelData.label
                    elide: Text.ElideNone
                }

                Rectangle {
                    id: vLine

                    width: 1
                    height: background.height + prv.tickLength
                    color: ui.theme.extra["dynamics_grid_color"]
                }
            }
        }

        Repeater {
            id: horizontalLines
            model: root.yTicks
            delegate: Item {
                x: root.yTickPosition === GridPlot.Right ? 0 : -prv.tickLength
                y: background.height * (1 - modelData.position)

                StyledTextLabel {
                    readonly property bool isBottom: root.alignEdgeLabels && index === 0
                    readonly property bool isTop: root.alignEdgeLabels && index === root.yTicks.length - 1

                    width: prv.yLabelMaxWidth
                    height: prv.labelHeight
                    horizontalAlignment: Text.AlignRight
                    anchors.left: root.yTickPosition === GridPlot.Right ? hLine.right : undefined
                    anchors.right: root.yTickPosition === GridPlot.Left ? hLine.left : undefined
                    anchors.leftMargin: root.yTickPosition === GridPlot.Right ? prv.labelMargin : undefined
                    anchors.rightMargin: root.yTickPosition === GridPlot.Left ? prv.labelMargin : undefined
                    y: isTop ? hLine.y : isBottom ? hLine.y - prv.labelHeight + 1 : hLine.y - (fontMetrics.ascent + fontMetrics.descent) / 2
                    text: modelData.label
                    elide: Text.ElideNone
                }

                Rectangle {
                    id: hLine

                    width: background.width + prv.tickLength
                    height: 1
                    color: ui.theme.extra["dynamics_grid_color"]
                }
            }
        }

        Rectangle {
            anchors.fill: parent
            color: background.color
            visible: !root.showGrid
        }

        Item {
            id: contentSlot
            anchors.fill: parent
        }
    }
}
