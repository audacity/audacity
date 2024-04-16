/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2023 MuseScore BVBA and others
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

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import MuseScore.Project 1.0

Item {
    id: root

    property string path: ""
    property string suffix: ""
    property string thumbnailUrl: ""

    ScoreThumbnailLoader {
        id: thumbnailLoader

        scorePath: root.path
    }

    Image {
        id: image
        anchors.fill: parent
        visible: status == Image.Ready
        source: root.thumbnailUrl
    }

    Loader {
        anchors.fill: parent
        visible: !image.visible
        active: visible

        sourceComponent: {
            if (thumbnailLoader.isThumbnailValid) {
                return scoreThumbnailComp
            }

            return genericThumbnailComp
        }

        Component {
            id: scoreThumbnailComp

            PixmapScoreThumbnailView {
                anchors.fill: parent
                thumbnail: thumbnailLoader.thumbnail
            }
        }

        Component {
            id: genericThumbnailComp

            Rectangle {
                anchors.fill: parent
                color: "white"

                Image {
                    anchors.centerIn: parent

                    width: 80/172 * parent.width
                    height: 110/80 * width

                    source: {
                        switch (root.suffix) {
                        case "gtp":
                        case "gp3":
                        case "gp4":
                        case "gp5":
                        case "gpx":
                        case "gp":
                        case "ptb":
                            return "qrc:/resources/Placeholder_GP.png"
                        case "mid":
                        case "midi":
                        case "kar":
                            return "qrc:/resources/Placeholder_MIDI.png"
                        case "mxl":
                        case "musicxml":
                        case "xml":
                            return "qrc:/resources/Placeholder_MXML.png"
                        case "mei":
                            return "qrc:/resources/Placeholder_MEI.png"
                        default:
                            return "qrc:/resources/Placeholder_Other.png"
                        }
                    }

                    fillMode: Image.PreserveAspectFit

                    // Prevent image from looking pixelated on low-res screens
                    mipmap: true
                }
            }
        }
    }
}
