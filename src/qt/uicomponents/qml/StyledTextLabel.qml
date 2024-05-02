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
import QtQuick

import Audacity.Ui

Text {
   id: root

   readonly property bool isEmpty: text.length === 0

   color: ui.theme.fontColor1
   linkColor: "gray"
   opacity: root.enabled ? 1.0 : 0.5

   elide: Text.ElideRight
   verticalAlignment: Text.AlignVCenter
   horizontalAlignment: Text.AlignHCenter

   font.pixelSize: 12

   onLinkActivated: function(link) {
      Qt.openUrlExternally(link)
   }

   onHoveredLinkChanged: {
      if (Boolean(hoveredLink)) {
         mouseAreaLoader.active = true
      }
   }

   Loader {
      id: mouseAreaLoader
      anchors.fill: parent
      active: false

      sourceComponent: MouseArea {
         anchors.fill: parent
         acceptedButtons: Qt.NoButton
         cursorShape: root.hoveredLink ? Qt.PointingHandCursor : Qt.ArrowCursor
      }
   }
}
