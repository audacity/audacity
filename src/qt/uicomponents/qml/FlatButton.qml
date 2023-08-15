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
import QtQuick.Layouts

import Audacity.UiComponents
import Audacity.UiThemes

FocusScope {
   id: root

   property int icon: IconCode.NONE
   property string text: ""
   property int textFormat: Text.AutoText

   property font iconFont: appConfig.iconFont
   property font textFont: appConfig.bodyFont

   property bool transparent: false
   property bool accentButton: false

   property color normalColor:
      transparent ? "transparent" : accentButton ? accentColor : UiTheme.buttonColor
   property color hoverHitColor: accentButton ? accentColor : UiTheme.buttonColor
   property color accentColor: UiTheme.accentColor

   property color iconColor: UiTheme.fontColor1

   property real radius: 3

   property bool isNarrow: buttonType === FlatButton.Horizontal
   property real margins: isNarrow ? 12 : 16
   property real minWidth: isNarrow ? 24 : 132

   property bool drawFocusBorderInsideRect: false

   property int orientation: Qt.Vertical
   readonly property bool isVertical: root.orientation === Qt.Vertical

   property alias mouseArea: mouseArea

   property bool isClickOnKeyNavTriggered: true

   property Component backgroundItem: defaultBackgroundComponent

   enum ButtonType {
      TextOnly,
      IconOnly,
      Horizontal,
      Vertical
   }

   // Can be overridden, for buttons that have a custom content component
   // but should be sized as one of the default types
   property int buttonType: {
      if (icon !== IconCode.NONE) {
         if (Boolean(text)) {
            return isVertical ? FlatButton.Vertical : FlatButton.Horizontal
         }

         return FlatButton.IconOnly
      }

      return FlatButton.TextOnly
   }

   signal clicked()

   objectName: root.text

   implicitWidth: contentLoader.implicitWidth + 2 * margins
   implicitHeight: Math.max(contentLoader.implicitHeight, UiTheme.defaultButtonSize)

   opacity: root.enabled ? 1.0 : UiTheme.itemOpacityDisabled

   Loader {
      anchors.fill: parent

      sourceComponent: root.backgroundItem
   }

   Component {
      id: defaultBackgroundComponent

      Rectangle {
         id: background

         color: root.normalColor
         opacity: UiTheme.buttonOpacityNormal

         radius: root.radius
         border.width: UiTheme.borderWidth
         border.color: UiTheme.strokeColor

         states: [
            State {
               name: "PRESSED"
               when: mouseArea.pressed

               PropertyChanges {
                  target: background
                  color: root.hoverHitColor
                  opacity: UiTheme.buttonOpacityHit
               }
            },

            State {
               name: "HOVERED"
               when: root.enabled && mouseArea.containsMouse && !mouseArea.pressed

               PropertyChanges {
                  target: background
                  color: root.hoverHitColor
                  opacity: UiTheme.buttonOpacityHover
               }
            }
         ]
      }
   }

   Loader {
      id: contentLoader

      anchors.verticalCenter: parent ? parent.verticalCenter : undefined
      anchors.horizontalCenter: parent ? parent.horizontalCenter : undefined

      sourceComponent: defaultContentComponent
      readonly property Component defaultContentComponent: root.isVertical ? verticalContentComponent : horizontalContentComponent
   }

   Component {
      id: verticalContentComponent

      ColumnLayout {
         spacing: 4

         StyledIconLabel {
            Layout.alignment: Qt.AlignHCenter
            iconCode: root.icon
            font: root.iconFont
            color: root.iconColor
            visible: !isEmpty
         }

         StyledTextLabel {
            Layout.alignment: Qt.AlignHCenter
            text: root.text
            font: root.textFont
            textFormat: root.textFormat
            maximumLineCount: 1
            visible: !isEmpty
         }
      }
   }

   Component {
      id: horizontalContentComponent

      RowLayout {
         spacing: 8

         StyledIconLabel {
            Layout.alignment: Qt.AlignVCenter
            iconCode: root.icon
            font: root.iconFont
            color: root.iconColor
            visible: !isEmpty
         }

         StyledTextLabel {
            Layout.alignment: Qt.AlignVCenter
            text: root.text
            font: root.textFont
            textFormat: root.textFormat
            maximumLineCount: 1
            visible: !isEmpty
         }
      }
   }

   states: [
      State {
         name: "ICON_ONLY"
         when: root.buttonType === FlatButton.IconOnly

         PropertyChanges {
            target: root
            implicitWidth: UiTheme.defaultButtonSize
            implicitHeight: UiTheme.defaultButtonSize
         }
      },

      State {
         name: "TEXT_ONLY"
         when: root.buttonType === FlatButton.TextOnly

         PropertyChanges {
            target: root
            implicitWidth: Math.max(contentLoader.implicitWidth + 2 * root.margins,
                                    root.minWidth)
            implicitHeight: UiTheme.defaultButtonSize
         }
      },

      State {
         name: "HORIZONTAL"
         when: root.buttonType === FlatButton.Horizontal

         PropertyChanges {
            target: root
            implicitHeight: UiTheme.defaultButtonSize
         }
      },

      State {
         name: "VERTICAL"
         when: root.buttonType === FlatButton.Vertical

         PropertyChanges {
            target: root
            implicitHeight: 48
         }
      }
   ]

   MouseArea {
      id: mouseArea
      anchors.fill: parent
      hoverEnabled: true

      onClicked: function() {
         root.clicked()
      }
   }
}
