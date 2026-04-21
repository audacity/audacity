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

import Muse.Ui 1.0
import Muse.UiComponents
import Audacity.Project 1.0

Item {
    id: root

    property string path: ""
    property string suffix: ""
    property string placeholder: ""

    ProjectThumbnailLoader {
        id: thumbnailLoader

        projectPath: root.path
    }

    Loader {
        anchors.fill: parent
        active: visible

        sourceComponent: {
            if (thumbnailLoader.isThumbnailValid) {
                return projectThumbnailComp
            }

            return genericThumbnailComp
        }

        Component {
            id: projectThumbnailComp

            PixmapProjectThumbnailView {
                anchors.fill: parent
                thumbnail: thumbnailLoader.thumbnail
            }
        }

        Component {
            id: genericThumbnailComp

            Rectangle {
                anchors.fill: parent
                color: ui.theme.backgroundSecondaryColor

                Image {
                    anchors.centerIn: parent

                    width: parent.width / 2

                    source: {
                        switch (root.suffix) {
                        default:
                            return root.placeholder || "qrc:/resources/ProjectPlaceholder.svg"
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
