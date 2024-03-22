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

import MuseScore.Ui 1.0

Row {
    id: root

    property date time
    property date maxTime
    property alias maxMillisecondsNumber: millisecondsField.maxValue

    property var font: ui.theme.largeBodyFont

    signal timeEdited(var newTime)

    spacing: 0

    opacity: enabled ? 1 : ui.theme.itemOpacityDisabled

    NumberInputField {
        id: hoursField

        maxValue: root.maxTime.getHours()
        value: root.time.getHours()

        font: root.font

        onValueEdited: function(newValue) {
            var newTime = root.time
            newTime.setHours(newValue)
            root.timeEdited(newTime)
        }
    }

    StyledTextLabel {
        anchors.verticalCenter: parent.verticalCenter
        text: ":"
        font: root.font
    }

    NumberInputField {
        id: minutesField

        maxValue: hoursField.value === root.maxTime.getHours() ? root.maxTime.getMinutes() : 60
        value: root.time.getMinutes()

        displayedNumberLength: 2
        font: root.font

        onValueEdited: function(newValue) {
            var newTime = root.time
            newTime.setMinutes(newValue)
            root.timeEdited(newTime)
        }
    }

    StyledTextLabel {
        anchors.verticalCenter: parent.verticalCenter
        text: ":"
        font: root.font
    }

    NumberInputField {
        id: secondsField

        maxValue: minutesField.value === root.maxTime.getMinutes() ? root.maxTime.getSeconds() : 60
        value: root.time.getSeconds()

        displayedNumberLength: 2
        font: root.font

        onValueEdited: function(newValue) {
            var newTime = root.time
            newTime.setSeconds(newValue)
            root.timeEdited(newTime)
        }
    }

    StyledTextLabel {
        anchors.verticalCenter: parent.verticalCenter
        text: ":"
        font: root.font
    }

    NumberInputField {
        id: millisecondsField

        readonly property int maxDigitCount: maxValue.toString().length
        readonly property int precision: Math.max(1000 / Math.pow(10, maxDigitCount), 1)

        maxValue: secondsField.value === root.maxTime.getSeconds() ? root.maxTime.getMilliseconds() : 1000
        value: root.time.getMilliseconds() / precision

        font: root.font

        onValueEdited: function(newValue) {
            var newTime = root.time
            newTime.setMilliseconds(newValue * precision)
            root.timeEdited(newTime)
        }
    }
}
