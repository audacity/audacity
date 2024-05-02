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

import Muse.UiComponents 1.0

BaseSection {
    id: root

    property alias scoreInversionEnabled: scoreInversionEnable.checked

    signal resetThemeToDefaultRequested()
    signal scoreInversionEnableChangeRequested(bool enable)

    CheckBox {
        id: scoreInversionEnable
        width: parent.width

        text: qsTrc("appshell/preferences", "Invert score")

        navigation.name: "ScoreInversionBox"
        navigation.panel: root.navigation
        navigation.row: 0

        onClicked: {
            root.scoreInversionEnableChangeRequested(!checked)
        }
    }

    FlatButton {
        text: qsTrc("appshell/preferences", "Reset to default")

        navigation.name: "ResetButton"
        navigation.panel: root.navigation
        navigation.row: 1

        onClicked: {
            root.resetThemeToDefaultRequested()
        }
    }
}
