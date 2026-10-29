/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

Column {
    id: root

    property string title: ""

    //! Negative when mixed
    property real length: 0.0
    property real maxLength: 0.0
    //! -1 when mixed
    property int fadeType: -1

    //! One panel per control, so that Tab goes through all of them
    property NavigationSection navigationSection: null
    property int navigationOrderStart: 0
    readonly property int navigationPanelCount: 2

    property NavigationPanel lengthNavigationPanel: NavigationPanel {
        name: root.title + " Length"
        section: root.navigationSection
        order: root.navigationOrderStart
        accessible.name: root.title + " " + lengthLabel.text
    }

    property NavigationPanel shapeNavigationPanel: NavigationPanel {
        name: root.title + " Shape"
        section: root.navigationSection
        order: root.navigationOrderStart + 1
        accessible.name: root.title + " " + shapeLabel.text
    }

    signal lengthChangeRequested(real newLength)
    signal fadeTypeChangeRequested(int newType)

    spacing: 8

    function requestActiveFocus() {
        lengthControl.navigation.requestActive()
    }

    QtObject {
        id: prv

        readonly property bool isLengthMixed: root.length < 0
        readonly property string mixedText: qsTrc("projectscene", "Mixed")
    }

    StyledTextLabel {
        width: parent.width

        text: root.title
        horizontalAlignment: Text.AlignLeft
        font: ui.theme.bodyBoldFont
    }

    RowLayout {
        width: parent.width
        spacing: 8

        Column {
            Layout.fillWidth: true
            Layout.preferredWidth: 1
            spacing: 4

            StyledTextLabel {
                id: lengthLabel

                width: parent.width
                text: qsTrc("projectscene", "Length")
                horizontalAlignment: Text.AlignLeft
            }

            IncrementalPropertyControl {
                id: lengthControl

                width: parent.width

                // A mixed value shows as a placeholder, rather than the `isIndeterminate` "--"
                currentValue: prv.isLengthMixed ? 0.0 : root.length
                currentText: prv.isLengthMixed ? "" : ui.df.formatReal(root.length, decimals)
                hint: prv.isLengthMixed ? prv.mixedText : ""
                measureUnitsSymbol: prv.isLengthMixed ? "" : qsTrc("global", "s")

                minValue: 0.0
                maxValue: root.maxLength
                step: 0.1
                decimals: 5

                // None while mixed: leaving the field, e.g. by tabbing, the default one would fix the empty text up into
                // "0", which counts as an edit and would set all the clips' lengths to 0
                validator: prv.isLengthMixed ? null : lengthValidator

                DoubleInputValidator {
                    id: lengthValidator

                    bottom: lengthControl.minValue
                    top: lengthControl.maxValue
                    decimal: lengthControl.decimals
                }

                navigation.name: root.title + " Length"
                navigation.panel: root.lengthNavigationPanel
                navigation.order: 1
                navigation.accessible.name: lengthLabel.text + " " + (prv.isLengthMixed ? prv.mixedText : root.length)

                onValueEdited: function (newValue) {
                    root.lengthChangeRequested(newValue)
                }
            }
        }

        Column {
            Layout.fillWidth: true
            Layout.preferredWidth: 1
            spacing: 4

            StyledTextLabel {
                id: shapeLabel

                width: parent.width
                text: qsTrc("projectscene", "Shape")
                horizontalAlignment: Text.AlignLeft
            }

            StyledDropdown {
                id: shapeDropdown

                width: parent.width

                model: FadeTypeOptions.all
                currentIndex: indexOfValue(root.fadeType)
                indeterminateText: prv.mixedText

                navigation.name: root.title + " Shape"
                navigation.panel: root.shapeNavigationPanel
                navigation.order: 1
                navigation.accessible.name: shapeLabel.text + " " + currentText

                onActivated: function (index, value) {
                    root.fadeTypeChangeRequested(value)
                }
            }
        }
    }
}
