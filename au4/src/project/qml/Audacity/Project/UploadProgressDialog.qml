/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * Music Composition & Notation
 *
 * Copyright (C) 2024 Audacity BVBA and others
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
import QtQuick.Layouts 1.15

import Muse.UiComponents 1.0
import Audacity.Project 1.0

StyledDialogView {
    id: root

    contentWidth: 314
    contentHeight: 52
    margins: 12

    modal: true
    frameless: true
    closeOnEscape: false

    ColumnLayout {
        anchors.fill: parent

        spacing: 8

        StyledTextLabel {
            Layout.fillWidth: true
            Layout.alignment: Qt.AlignHCenter

            text: qsTrc("project", "Saving online…")
            font: ui.theme.largeBodyBoldFont
        }
    }
}
