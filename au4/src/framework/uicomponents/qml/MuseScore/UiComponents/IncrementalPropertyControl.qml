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

import MuseScore.UiComponents 1.0
import MuseScore.Ui 1.0

Item {
    id: root

    property int iconMode: !iconImage.isEmpty ? IncrementalPropertyControl.Left : IncrementalPropertyControl.Hidden
    property int iconBackgroundSize: 20
    property alias icon: iconImage.iconCode

    property alias isIndeterminate: textInputField.isIndeterminate
    property alias currentValue: textInputField.currentText

    property real step: 0.5
    property int decimals: 2
    property real maxValue: 999
    property real minValue: -999
    property alias validator: textInputField.validator
    property bool wrap: false

    property alias measureUnitsSymbol: textInputField.measureUnitsSymbol
    property alias hint: textInputField.hint

    property alias navigation: textInputField.navigation

    readonly property int spacing: 8

    property bool canIncrease: root.wrap || root.currentValue < root.maxValue
    property var onIncrement: null

    property bool canDecrease: root.wrap || root.currentValue > root.minValue
    property var onDecrement: null

    signal valueEdited(var newValue)
    signal valueEditingFinished(var newValue)

    implicitHeight: 30
    implicitWidth: parent.width

    navigation.name: Boolean(root.objectName) ? root.objectName : "IncrementalControl"

    function increment() {
        var newValue
        if (Boolean(onIncrement)) {
            newValue = onIncrement()
        } else {
            var value = root.isIndeterminate ? 0.0 : currentValue
            if (value + step >= root.maxValue) {
                newValue = wrap ? root.minValue + (value + step - root.maxValue) : root.maxValue
            } else {
                newValue = value + step
            }

            if (newValue === value) {
                return
            }

            newValue = +newValue.toFixed(decimals)
        }

        root.valueEdited(newValue)
        root.valueEditingFinished(newValue)
    }

    function decrement() {
        var newValue
        if (Boolean(onDecrement)) {
            newValue = onDecrement()
        } else {
            var value = root.isIndeterminate ? 0.0 : currentValue
            if (value - step < root.minValue) {
                newValue = wrap ? root.maxValue + (value - step + root.minValue) : root.minValue
            } else {
                newValue = value - step
            }

            if (newValue === value) {
                return
            }

            newValue = +newValue.toFixed(decimals)
        }

        root.valueEdited(newValue)
        root.valueEditingFinished(newValue)
    }

    enum IconMode {
        Hidden,
        Left,
        Right
    }

    QtObject {
        id: prv

        property bool isCustom: Boolean(onIncrement) && Boolean(onDecrement)
    }

    Rectangle {
        id: iconBackground

        anchors.verticalCenter: parent.verticalCenter

        height: root.iconBackgroundSize
        width: root.iconBackgroundSize

        color: ui.theme.buttonColor
        opacity: root.enabled ? ui.theme.buttonOpacityNormal : ui.theme.itemOpacityDisabled

        visible: !iconImage.isEmpty

        StyledIconLabel {
            id: iconImage
            anchors.fill: parent
        }
    }

    TextInputField {
        id: textInputField

        property int scrolled: 0

        anchors.top: parent.top
        anchors.bottom: parent.bottom

        navigation.onNavigationEvent: function(event) {
            switch (event.type) {
            case NavigationEvent.Up:
                root.increment()
                event.accepted = true
                break
            case NavigationEvent.Down:
                root.decrement()
                event.accepted = true
                break
            }
        }

        DoubleInputValidator {
            id: doubleInputValidator
            top: root.maxValue
            bottom: root.minValue
            decimal: root.decimals
        }

        IntInputValidator {
            id: intInputValidator
            top: root.maxValue
            bottom: root.minValue
        }

        validator: !prv.isCustom ? (root.decimals > 0 ? doubleInputValidator : intInputValidator) : null

        containsMouse: mouseArea.containsMouse || valueAdjustControl.containsMouse

        ValueAdjustControl {
            id: valueAdjustControl

            anchors.margins: textInputField.background.border.width
            anchors.top: parent.top
            anchors.right: parent.right
            anchors.bottom: parent.bottom

            radius: textInputField.background.radius - anchors.margins

            canIncrease: root.canIncrease
            canDecrease: root.canDecrease

            onIncreaseButtonClicked: { root.increment() }
            onDecreaseButtonClicked: { root.decrement() }
        }

        mouseArea.onWheel: function(wheel) {
            if (!textInputField.activeFocus) {
                wheel.accepted = false
                return
            }

            let pixelY = wheel.pixelDelta.y
            let angleY = wheel.angleDelta.y

            // This is set below. For angleY, make sure it is <= 120,
            // because in many actual mouse wheels, one scroll sets
            // angleY to +/- 120.
            let oneScroll = 0

            if (pixelY !== 0) {
                scrolled += pixelY
                oneScroll = 60
            } else if (angleY !== 0) {
                scrolled += angleY
                oneScroll = 120
            }

            if (scrolled >= oneScroll) {
                root.increment()
                scrolled = 0
            } else if (scrolled <= -oneScroll) {
                root.decrement()
                scrolled = 0
            }
        }

        mouseArea.onExited: {
            scrolled = 0
        }

        onTextChanged: function(newTextValue) {
            if (prv.isCustom) {
                root.valueEdited(newTextValue)
                return
            }

            var newVal = parseFloat(newTextValue)

            if (isNaN(newVal)) {
                newVal = 0
            }

            root.valueEdited(+newVal.toFixed(root.decimals))
        }

        onTextEditingFinished: function(newTextValue) {
            if (prv.isCustom) {
                root.valueEditingFinished(newTextValue)
                return
            }

            var newVal = parseFloat(newTextValue)

            if (isNaN(newVal)) {
                newVal = 0
            }

            root.valueEditingFinished(+newVal.toFixed(root.decimals))
        }
    }

    states: [
        State {
            name: "ICON_MODE_HIDDEN"
            when: root.iconMode === IncrementalPropertyControl.Hidden

            AnchorChanges {
                target: textInputField
                anchors.left: root.left
                anchors.right: root.right
            }

            PropertyChanges {
                target: iconBackground
                visible: false
            }
        },

        State {
            name: "ICON_ALIGN_LEFT"
            when: root.iconMode === IncrementalPropertyControl.Left

            PropertyChanges {
                target: textInputField
                anchors.leftMargin: root.spacing
            }

            AnchorChanges {
                target: textInputField
                anchors.left: iconBackground.right
                anchors.right: root.right
            }

            PropertyChanges {
                target: iconBackground
                visible: true
            }

            AnchorChanges {
                target: iconBackground
                anchors.left: root.left
            }
        },

        State {
            name: "ICON_ALIGN_RIGHT"
            when: root.iconMode === IncrementalPropertyControl.Right

            PropertyChanges {
                target: textInputField
                anchors.rightMargin: root.spacing
            }

            AnchorChanges {
                target: textInputField
                anchors.left: root.left
                anchors.right: iconBackground.left
            }

            PropertyChanges {
                target: iconBackground
                visible: true
            }

            AnchorChanges {
                target: iconBackground
                anchors.right: root.right
            }
        }
    ]
}
