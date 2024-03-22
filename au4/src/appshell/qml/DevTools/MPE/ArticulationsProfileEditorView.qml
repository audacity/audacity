/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

import QtQuick 2.15
import QtQuick.Layouts 1.12
import MuseScore.Ui 1.0
import MuseScore.UiComponents 1.0
import MuseScore.Mpe 1.0

Rectangle {

    color: ui.theme.backgroundPrimaryColor

    ArticulationsProfileEditorModel {
        id: editorModel
    }

    RowLayout {
        id: contentRow

        anchors.fill: parent

        spacing: 24

        Column {
            id: leftColumn

            Layout.alignment: Qt.AlignLeft
            Layout.fillHeight: true
            Layout.preferredWidth: parent.width * 0.15
            Layout.margins: 12

            spacing: 36

            Row {
                id: filePickerRow

                width: parent.width

                spacing: 4

                TextInputField {
                    id: pathField

                    width: parent.width - openButton.width - saveButton.width - createButton.width

                    //! Make these strings translatable when we expose this tool to users
                    hint: /*qsTrc*/ "Profile path"
                    currentText: editorModel.currentPath
                }

                FlatButton {
                    id: openButton

                    icon: IconCode.OPEN_FILE

                    onClicked: {
                        editorModel.requestToOpenProfile()
                    }
                }

                FlatButton {
                    id: createButton

                    icon: IconCode.NEW_FILE

                    enabled: !pathField.hasText

                    onClicked: {
                        editorModel.requestToCreateProfile()
                    }
                }

                FlatButton {
                    id: saveButton

                    icon: IconCode.SAVE

                    enabled: pathField.hasText

                    onClicked: {
                        editorModel.requestToSaveProfile()
                    }
                }
            }

            Column {

                anchors.left: parent.left
                anchors.leftMargin: 8

                height: parent.height
                width: parent.width

                spacing: 18

                StyledTextLabel {
                    id: scopeListLabel

                    width: parent.width

                    horizontalAlignment: Qt.AlignLeft
                    font: ui.theme.largeBodyBoldFont
                    text: /*qsTrc*/ "Articulation pattern list"
                }

                ArticulationPatternsTab {
                    id: scopeListView

                    height: 400
                    width: parent.width

                    model: editorModel
                }
            }
        }

        Column {
            id: centralColumn

            Layout.alignment: Qt.AlignHCenter
            Layout.fillHeight: true
            Layout.fillWidth: true

            spacing: 24

            ArticulationPatternPlot {
                id: patternPlot

                height: parent.height * 0.80
                width: height

                patternModel: editorModel ? editorModel.selectedItem.currentPatternSegment : null

                showArrangement: editorModel ? editorModel.isArrangementVisible : false
                showPitch: editorModel ? editorModel.isPitchVisible : false
                showExpression: editorModel ? editorModel.isExpressionVisible : false
            }

            Column {
                height: parent.height * 0.15
                width: parent.width

                spacing: 36

                StyledTextLabel {
                    id: patternSegmentsLabel

                    width: parent.width

                    horizontalAlignment: Qt.AlignLeft
                    font: ui.theme.largeBodyBoldFont
                    text: /*qsTrc*/ "Pattern segments:"
                }

                ArticulationPatternSegmentsList {
                    id: patternSegmentsList

                    height: parent.height
                    width: parent.width

                    model: editorModel ? editorModel.selectedItem : 0
                }
            }
        }

        Column {
            id: rightColumn

            Layout.alignment: Qt.AlignRight
            Layout.fillHeight: true
            Layout.preferredWidth: parent.width * 0.25
            Layout.margins: 12

            spacing: 36

            Column {
                anchors.left: parent.left
                anchors.leftMargin: 8

                width: parent.width

                spacing: 18

                StyledTextLabel {
                    id: displayModesLabel

                    width: parent.width

                    horizontalAlignment: Qt.AlignLeft
                    font: ui.theme.largeBodyBoldFont
                    text: /*qsTrc*/ "Appearance"
                }

                Column {
                    width: parent.width

                    spacing: 8

                    CheckBox {
                        width: parent.width

                        checked: editorModel ? editorModel.isArrangementVisible : false
                        text: /*qsTrc*/ "Show arrangement"

                        onClicked: {
                            if (editorModel) {
                                editorModel.isArrangementVisible = !editorModel.isArrangementVisible
                            }
                        }
                    }

                    CheckBox {
                        width: parent.width

                        checked: editorModel ? editorModel.isPitchVisible : false
                        text: /*qsTrc*/ "Show pitch"

                        onClicked: {
                            if (editorModel) {
                                editorModel.isPitchVisible = !editorModel.isPitchVisible
                            }
                        }
                    }

                    CheckBox {
                        width: parent.width

                        checked: editorModel ? editorModel.isExpressionVisible : false
                        text: /*qsTrc*/ "Show expression"

                        onClicked: {
                            if (editorModel) {
                                editorModel.isExpressionVisible = !editorModel.isExpressionVisible
                            }
                        }
                    }
                }
            }

            Column {
                anchors.left: parent.left
                anchors.leftMargin: 8

                width: parent.width

                spacing: 18

                StyledTextLabel {
                    id: patternSegmentPropertiesLabel

                    width: parent.width

                    horizontalAlignment: Qt.AlignLeft
                    font: ui.theme.largeBodyBoldFont
                    text: /*qsTrc*/ "Pattern segment positioning"
                }

                Column {
                    width: parent.width

                    spacing: 12

                    RowLayout {
                        spacing: 12

                        StyledTextLabel {
                            id: positionFromLabel

                            Layout.alignment: Qt.AlignLeft | Qt.AlignVCenter
                            Layout.preferredWidth: 48

                            horizontalAlignment: Qt.AlignLeft

                            text: /*qsTrc*/ "From"
                        }

                        IncrementalPropertyControl {
                            id: positionFrom

                            Layout.alignment: Qt.AlignRight | Qt.AlignVCenter
                            Layout.preferredWidth: 64

                            currentValue: editorModel ? editorModel.selectedItem.currentPatternSegment.positionFrom / 100 : 0
                            minValue: 0
                            maxValue: 100
                            decimals: 0
                            step: 1

                            validator: IntValidator {
                                top: positionFrom.maxValue
                                bottom: positionFrom.minValue
                            }

                            onValueEdited: function(newValue) {
                                if (editorModel) {
                                    editorModel.selectedItem.currentPatternSegment.positionFrom = newValue * 100
                                }
                            }
                        }
                    }

                    RowLayout {
                        spacing: 12

                        StyledTextLabel {
                            id: positionToLabel

                            Layout.alignment: Qt.AlignLeft | Qt.AlignVCenter
                            Layout.preferredWidth: 48

                            horizontalAlignment: Qt.AlignLeft

                            text: /*qsTrc*/ "To"
                        }

                        IncrementalPropertyControl {
                            id: positionTo

                            Layout.alignment: Qt.AlignRight | Qt.AlignVCenter
                            Layout.preferredWidth: 64

                            currentValue: editorModel ? editorModel.selectedItem.currentPatternSegment.positionTo / 100 : 0
                            minValue: 0
                            maxValue: 100
                            decimals: 0
                            step: 1

                            validator: IntValidator {
                                top: positionFrom.maxValue
                                bottom: positionFrom.minValue
                            }

                            onValueEdited: function(newValue) {
                                if (editorModel) {
                                    editorModel.selectedItem.currentPatternSegment.positionTo = newValue * 100
                                }
                            }
                        }
                    }
                }
            }

            Column {
                anchors.left: parent.left
                anchors.leftMargin: 8
                anchors.right: parent.right
                anchors.rightMargin: 8

                spacing: 18

                StyledTextLabel {
                    id: patternControlsLabel

                    width: parent.width

                    horizontalAlignment: Qt.AlignLeft
                    font: ui.theme.largeBodyBoldFont
                    text: /*qsTrc*/ "Pattern parameters"
                }

                ArticulationPatternControlsTab {
                    id: patternControlsTab

                    patternModel: editorModel ? editorModel.selectedItem.currentPatternSegment : null

                    height: 120
                    width: parent.width
                }
            }
        }
    }
}
