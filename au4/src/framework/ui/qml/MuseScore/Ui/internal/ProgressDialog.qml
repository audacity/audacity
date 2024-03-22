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
import QtQuick.Layouts 1.15

import MuseScore.Ui 1.0
import MuseScore.UiComponents 1.0

StyledDialogView {
    id: root

    property alias title: titleLabel.text
    property var progress: undefined

    modal: true
    frameless: true
    closeOnEscape: false

    contentWidth: content.width
    contentHeight: content.height

    margins: 16

    ProgressDialogModel {
        id: model

        onFinished: {
            root.close()
        }
    }

    Component.onCompleted: {
        model.load(root.progress)
    }

    ColumnLayout {
        id: content

        readonly property int defaultWidth: 320
        readonly property int defaultHeight: progressStatusLabel.visible ? 140 : 120

        width: Math.max(childrenRect.width, content.defaultWidth)
        height: Math.max(childrenRect.height, content.defaultHeight)

        spacing: 16

        StyledTextLabel {
            id: titleLabel

            font: ui.theme.largeBodyBoldFont
            horizontalAlignment: Text.AlignLeft

            visible: !isEmpty
        }

        StyledTextLabel {
            id: progressStatusLabel

            horizontalAlignment: Text.AlignLeft

            visible: !isEmpty
            text: model.statusMessage
        }

        ProgressBar {
            Layout.fillWidth: true
            Layout.preferredHeight: 30

            from: model.from
            value: model.value
            to: model.to

            progressStatus: model.to != 0 ? Math.round(model.value * 100 / model.to) + "%" : "0%"
        }

        FlatButton {
            Layout.alignment: Qt.AlignRight

            text: qsTrc("global", "Cancel")

            onClicked: {
                model.cancel()
                root.reject()
            }
        }
    }
}
