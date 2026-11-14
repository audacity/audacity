import QtQuick
import Muse.Ui
import Muse.UiComponents
import Audacity.UiComponents

GridPlot {
    id: root

    // An AutoDuckViewModel
    required property var model

    readonly property bool isDragging: prv.draggedPoint !== -1

    // If you render me at this aspect ratio, the grid cells will be square.
    readonly property real preferredAspectRatio: prv.numTSteps / prv.numDbSteps

    showTicks: false
    showBorder: true
    radius: 4

    QtObject {
        id: prv

        // Time axis, in seconds. The duck is placed relative to t = 0, where
        // the control track starts exceeding the threshold, and the recovery
        // relative to t = triggerEnd, where it stops doing so. The bounds
        // leave room for the longest ramps.
        readonly property real tMin: -4
        readonly property real tMax: 11
        readonly property real tStep: 0.5
        readonly property real numTSteps: (tMax - tMin) / tStep
        readonly property real triggerEnd: 7

        // Gain axis, in dB. The bounds leave room for the value labels above
        // 0 dB and below the maximum gain reduction.
        readonly property real dbMin: -30
        readonly property real dbMax: 6
        readonly property real dbStep: 3
        readonly property real numDbSteps: (dbMax - dbMin) / dbStep

        readonly property real pointRadius: 4.5
        readonly property real labelSpacing: 4

        readonly property color curveColor: ui.theme.extra["auto_duck_curve_color"]

        property int draggedPoint: -1

        function xOf(t) {
            return (t - tMin) / (tMax - tMin) * root.backgroundWidth
        }
        function yOf(db) {
            return (dbMax - db) / (dbMax - dbMin) * root.backgroundHeight
        }
        function tOf(x) {
            return tMin + x / root.backgroundWidth * (tMax - tMin)
        }
        function dbOf(y) {
            return dbMax - y / root.backgroundHeight * (dbMax - dbMin)
        }

        function formatTime(t) {
            //: Abbreviation of "seconds", used as a unit suffix
            return t.toFixed(2) + " " + qsTrc("global", "s")
        }
        function formatDb(db) {
            //: Abbreviation of "decibels", used as a unit suffix
            return parseFloat(db.toFixed(1)) + " " + qsTrc("global", "dB")
        }

        // The draggable points, left to right. A delegate reads its point
        // from this list by index, so that it isn't recreated (interrupting a
        // drag) every time a value changes.
        readonly property var points: {
            const m = root.model
            const gain = m.gainReduction
            return [
                {
                    "t": m.duckStart,
                    "db": 0,
                    "label": formatTime(m.duckStart),
                    "labelAbove": true,
                    "horizontal": true,
                    "setValue": function (t, db) {
                        m.duckStart = t
                    }
                },
                {
                    "t": m.duckEnd,
                    "db": gain,
                    "label": formatTime(m.duckEnd),
                    "labelAbove": false,
                    "horizontal": true,
                    "setValue": function (t, db) {
                        m.duckEnd = t
                    }
                },
                {
                    "t": (m.duckEnd + triggerEnd + m.recoveryStart) / 2,
                    "db": gain,
                    "label": formatDb(gain),
                    "labelAbove": true,
                    "horizontal": false,
                    "setValue": function (t, db) {
                        m.gainReduction = db
                    }
                },
                {
                    "t": triggerEnd + m.recoveryStart,
                    "db": gain,
                    "label": formatTime(m.recoveryStart),
                    "labelAbove": false,
                    "horizontal": true,
                    "setValue": function (t, db) {
                        m.recoveryStart = t - triggerEnd
                    }
                },
                {
                    "t": triggerEnd + m.recoveryEnd,
                    "db": 0,
                    "label": formatTime(m.recoveryEnd),
                    "labelAbove": true,
                    "horizontal": true,
                    "setValue": function (t, db) {
                        m.recoveryEnd = t - triggerEnd
                    }
                }
            ]
        }
    }

    xTicks: {
        const result = []
        const n = Math.round((prv.tMax - prv.tMin) / prv.tStep)
        for (let i = 1; i < n; ++i) {
            const t = prv.tMin + i * prv.tStep
            result.push({
                "label": "",
                "position": i / n,
                "emphasized": t === 0 || t === prv.triggerEnd
            })
        }
        return result
    }

    yTicks: {
        const result = []
        for (let db = prv.dbMin + prv.dbStep; db < prv.dbMax; db += prv.dbStep) {
            result.push({
                "label": "",
                "position": (db - prv.dbMin) / (prv.dbMax - prv.dbMin)
            })
        }
        return result
    }

    Canvas {
        id: curve

        anchors.fill: parent

        onPaint: {
            const ctx = getContext("2d")
            ctx.clearRect(0, 0, width, height)

            const points = prv.points
            ctx.strokeStyle = prv.curveColor
            ctx.lineWidth = 2
            ctx.beginPath()
            ctx.moveTo(0, prv.yOf(0))
            for (let i = 0; i < points.length; ++i) {
                ctx.lineTo(prv.xOf(points[i].t), prv.yOf(points[i].db))
            }
            ctx.lineTo(width, prv.yOf(0))
            ctx.stroke()
        }

        Connections {
            target: prv
            function onPointsChanged() {
                curve.requestPaint()
            }
        }
        onWidthChanged: requestPaint()
        onHeightChanged: requestPaint()
    }

    Repeater {
        id: handles

        model: prv.points.length

        delegate: Item {
            id: handle

            readonly property var point: prv.points[index]
            readonly property bool highlighted: mouseArea.containsMouse || mouseArea.pressed

            x: prv.xOf(point.t) - width / 2
            y: prv.yOf(point.db) - height / 2
            width: 18
            height: 18

            Rectangle {
                anchors.centerIn: parent
                width: handle.highlighted ? 12 : 2 * prv.pointRadius
                height: width
                radius: width / 2
                color: prv.curveColor

                Rectangle {
                    anchors.centerIn: parent
                    visible: handle.highlighted
                    width: 8
                    height: 8
                    radius: 4
                    color: ui.theme.extra["black_color"]

                    Rectangle {
                        anchors.centerIn: parent
                        width: 2
                        height: 2
                        radius: 1
                        color: ui.theme.extra["white_color"]
                    }
                }
            }

            MouseArea {
                id: mouseArea

                anchors.fill: parent
                hoverEnabled: true
                cursorShape: handle.point.horizontal ? Qt.SizeHorCursor : Qt.SizeVerCursor

                onPressed: prv.draggedPoint = index
                onReleased: prv.draggedPoint = -1
                onCanceled: prv.draggedPoint = -1

                onPositionChanged: function (mouse) {
                    if (!pressed) {
                        return
                    }
                    const p = mapToItem(curve, mouse.x, mouse.y)
                    const t = Math.round(prv.tOf(p.x) * 100) / 100
                    const db = Math.round(prv.dbOf(p.y) * 10) / 10
                    handle.point.setValue(t, db)
                }
            }
        }
    }

    // Value labels are always shown, on top of the curve and points.
    Repeater {
        model: prv.points.length

        delegate: Rectangle {
            readonly property var point: prv.points[index]
            readonly property real pointX: prv.xOf(point.t)
            readonly property real pointY: prv.yOf(point.db)

            x: Math.max(0, Math.min(curve.width - width, pointX - width / 2))
            y: point.labelAbove ? pointY - prv.pointRadius - prv.labelSpacing - height : pointY + prv.pointRadius + prv.labelSpacing
            width: labelText.implicitWidth + 8
            height: labelText.implicitHeight + 4
            radius: 4
            color: ui.theme.extra["black_color"]

            StyledTextLabel {
                id: labelText

                anchors.centerIn: parent
                text: point.label
                color: ui.theme.extra["white_color"]
            }
        }
    }
}
