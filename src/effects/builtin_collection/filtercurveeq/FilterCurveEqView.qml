import QtQuick
import QtQuick.Controls
import Muse.Ui
import Muse.UiComponents
import Audacity.Effects
import Audacity.BuiltinEffects
import Audacity.BuiltinEffectsCollection
import Audacity.UiComponents

BuiltinEffectBase {
    id: root

    property string title: qsTrc("effects/filtercurveeq", "Filter Curve EQ")
    property bool isApplyAllowed: true

    width: column.width
    implicitHeight: column.height

    builtinEffectModel: FilterCurveEqViewModelFactory.createModel(root, root.instanceId)
    numNavigationPanels: 1
    property alias filterCurveEq: root.builtinEffectModel
    property NavigationPanel buttonsNavigationPanel: NavigationPanel {
        name: "FilterCurveEqButtons"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 1
    }

    // Esc otherwise both cancels the drag and closes the dialog.
    Binding {
        target: root.dialogView
        property: "closeOnEscape"
        value: !curve.isDragging
        when: root.dialogView !== null
        restoreMode: Binding.RestoreBindingOrValue
    }

    Column {
        id: column

        bottomPadding: 16
        spacing: 8

        Item {
            id: buttonsRow

            width: gridPlot.width
            height: Math.max(leftGroup.height, rightGroup.height)

            Row {
                id: leftGroup

                spacing: 16
                topPadding: 6
                bottomPadding: 6

                anchors.left: parent.left
                anchors.leftMargin: gridPlot.backgroundX
                anchors.verticalCenter: parent.verticalCenter

                CheckBox {
                    id: linFreqScaleCheckBox

                    anchors.verticalCenter: parent.verticalCenter

                    navigation.panel: root.buttonsNavigationPanel
                    navigation.order: 0

                    text: qsTrc("effects/filtercurveeq", "Linear frequency scale")

                    checked: filterCurveEq.linFreqScale

                    onClicked: filterCurveEq.linFreqScale = !filterCurveEq.linFreqScale
                }

                CheckBox {
                    id: showGridlinesCheckBox

                    anchors.verticalCenter: parent.verticalCenter

                    navigation.panel: root.buttonsNavigationPanel
                    navigation.order: linFreqScaleCheckBox.navigation.order + 1

                    text: qsTrc("effects/filtercurveeq", "Grid lines")

                    checked: filterCurveEq.gridlinesVisible

                    onClicked: filterCurveEq.gridlinesVisible = !filterCurveEq.gridlinesVisible
                }
            }

            Row {
                id: rightGroup

                spacing: 4
                topPadding: 6
                bottomPadding: 6

                anchors.right: parent.right
                anchors.rightMargin: gridPlot.width - (gridPlot.backgroundX + gridPlot.backgroundWidth)
                anchors.verticalCenter: parent.verticalCenter

                FlatButton {
                    id: resetButton

                    width: 64
                    height: 28

                    navigation.panel: root.buttonsNavigationPanel
                    navigation.order: showGridlinesCheckBox.navigation.order + 1

                    text: qsTrc("effects/filtercurveeq", "Reset")

                    onClicked: filterCurveEq.curveModel.flatten()
                }

                FlatButton {
                    id: invertButton

                    width: 64
                    height: 28

                    navigation.panel: root.buttonsNavigationPanel
                    navigation.order: resetButton.navigation.order + 1

                    text: qsTrc("effects/filtercurveeq", "Invert")

                    onClicked: filterCurveEq.curveModel.invert()
                }

                FlatButton {
                    id: zoomInButton

                    width: 28
                    height: 28

                    navigation.panel: root.buttonsNavigationPanel
                    navigation.order: invertButton.navigation.order + 1

                    icon: IconCode.ZOOM_IN
                    toolTipTitle: qsTrc("effects/filtercurveeq", "Zoom in")
                    enabled: filterCurveEq.canZoomIn

                    onClicked: filterCurveEq.zoomIn()
                }

                FlatButton {
                    id: zoomOutButton

                    width: 28
                    height: 28

                    navigation.panel: root.buttonsNavigationPanel
                    navigation.order: zoomInButton.navigation.order + 1

                    icon: IconCode.ZOOM_OUT
                    toolTipTitle: qsTrc("effects/filtercurveeq", "Zoom out")
                    enabled: filterCurveEq.canZoomOut

                    onClicked: filterCurveEq.zoomOut()
                }
            }
        }

        GridPlot {
            id: gridPlot

            width: 808
            height: 384

            xTickPosition: GridPlot.Bottom
            yTickPosition: GridPlot.Left

            alignEdgeLabels: true

            xTicks: filterCurveEq.xTicks
            showGrid: filterCurveEq.gridlinesVisible

            // Feed the model the dimensions it needs to prune overlapping
            // ticks. labelWidth is a conservative estimate based on a
            // representative wide label (e.g. "10.5k"); axisWidth tracks the
            // plot background.
            FontMetrics {
                id: labelFm
                font.family: ui.theme.bodyFont.family
                font.pixelSize: ui.theme.bodyFont.pixelSize
            }
            Binding {
                target: filterCurveEq
                property: "labelWidth"
                value: labelFm.boundingRect("10.5k").width
            }
            Binding {
                target: filterCurveEq
                property: "axisWidth"
                value: gridPlot.backgroundWidth
            }

            yTicks: (function () {
                    const result = []
                    const span = filterCurveEq.dbMax - filterCurveEq.dbMin
                    if (span <= 0)
                        return result
                    for (let i = filterCurveEq.dbMin; i <= filterCurveEq.dbMax; i += 6) {
                        result.push({
                            label: String(i),
                            position: (i - filterCurveEq.dbMin) / span
                        })
                    }
                    return result
                })()

            Item {
                // Clips the plot so points whose dB falls outside [dbMin, dbMax]
                // (the visible band) don't render their handles, while the
                // polyline still exits the band with the correct slope.
                id: plotClip

                anchors.fill: parent
                clip: true

                PolylinePlot {
                    id: curve

                    property bool isDragging: false

                    width: plotClip.width
                    height: plotClip.height

                    lineColor: ui.theme.accentColor
                    lineWidth: 2

                    pointRadius: 4.0
                    pointOutlineColor: ui.theme.accentColor
                    pointCentreColor: ui.theme.accentColor
                    pointOutlineWidth: 2.0

                    isSnapEnabled: false

                    ghostPointRadius: 3.0
                    ghostPointOutlineColor: ui.theme.accentColor

                    drawBackground: false

                    points: filterCurveEq.curveModel.points
                    defaultValue: filterCurveEq.curveModel.defaultValue

                    xRangeFrom: 0.0
                    xRangeTo: 1.0

                    yRangeFrom: filterCurveEq.dbMin
                    yRangeTo: filterCurveEq.dbMax
                    yAxisInverse: false

                    Component.onCompleted: {
                        curve.init()
                    }

                    function activePointFreq() {
                        const norm = curve.width > 0 ? (curve.activePointX / curve.width) : 0
                        if (filterCurveEq.linFreqScale) {
                            return filterCurveEq.loFreq + norm * (filterCurveEq.hiFreq - filterCurveEq.loFreq)
                        }
                        const loLog = Math.log(filterCurveEq.loFreq) / Math.LN10
                        const hiLog = Math.log(filterCurveEq.hiFreq) / Math.LN10
                        return Math.pow(10, norm * (hiLog - loLog) + loLog)
                    }

                    onPointMoved: function (index, x, y, completed) {
                        filterCurveEq.curveModel.setPoint(index, x, y, completed)
                        tooltip.gain = y
                        tooltip.freq = curve.activePointFreq()
                        tooltip.show(true)
                        curve.isDragging = !completed
                    }

                    onPointAdded: function (x, y, completed) {
                        filterCurveEq.curveModel.addPoint(x, y, completed)
                        curve.isDragging = !completed
                    }

                    onPointRemoved: function (index, completed) {
                        filterCurveEq.curveModel.removePoint(index, completed)
                        if (completed) {
                            curve.isDragging = false
                        }
                    }

                    onDragCancelled: {
                        filterCurveEq.curveModel.cancelDrag()
                        tooltip.hide(true)
                        // Defer so the closeOnEscape binding keeps the dialog open during the same Esc dispatch.
                        Qt.callLater(function () {
                            curve.isDragging = false
                        })
                    }

                    onInteractionFinished: function () {
                        if (!curve.hasActivePoint) {
                            tooltip.hide(true)
                        }
                        curve.isDragging = false
                    }

                    onActivePointChanged: {
                        if (curve.hasActivePoint) {
                            fake.x = curve.activePointX
                            fake.y = curve.activePointY - (curve.pointRadius + 2)
                            tooltip.gain = curve.activePointValue
                            tooltip.freq = curve.activePointFreq()
                            tooltip.show(true)
                        } else {
                            tooltip.hide(true)
                        }
                    }

                    Item {
                        // fakeItem the tooltip popup is anchored to.
                        id: fake

                        width: 1
                        height: 1
                        enabled: false  // don't steal mouse events

                        FilterCurveEqTooltip {
                            id: tooltip
                        }
                    }
                }
            }
        }
    }
}
