import QtQuick
import QtQuick.Layouts
import QtQuick.Controls

import Audacity.UiComponents
import Audacity.Ui
import Audacity.NumericFormats

Item {

   id : root

   property string type
   property int formatIndex
   property NumericConverterFormatterAdapter formatter : formatterFactory.create(type, formatIndex)
   property real value : 0
   property color iconColor: ui.theme.fontColor1
   property int icon: IconCode.TIMECODE

   implicitWidth : mainLayout.Layout.minimumWidth

   ListModel {
      id : formatsModel
   }

   Connections
   {
      target : formatter
      function onFormatChanged() {
         root.updateLayout()
      }
   }

   // Dropdown menu that offers format selection.
   // Filled with items from itemSourceModel
   Component {
      id : formatSelectMenuComponent
      Menu {
         property ListModel itemSourceModel
         property NumericTextControl numericTextControl
         Repeater {
            model : itemSourceModel
            MenuItem {
               text : name
               checkable : true
               checked : numericTextControl.formatIndex == index
               autoExclusive : true
               onTriggered : formatIndex = index
            }
         }
      }
   }

   // Format units (like "m", "seconds", "bars"...)
   Component {
      id : timeUnitComponent
      Text {
         color: ui.theme.fontColor2
         font {
            family: ui.theme.bodyFont.family
            pointSize: 14
         }
         leftPadding : 1
         rightPadding : 1
         opacity: 0.7
      }
   }

   // Format unit separators (",", "."...)
   Component {
      id : separatorSymbolComponent
      Text {

         color: ui.theme.fontColor2
         font {
            family: ui.theme.timecodeFont.family
            pointSize: 14
            bold : true
         }
         leftPadding : 1
         rightPadding : 1
      }
   }

   // Shows format individual digit
   Component {
      id : digitSlotComponent
      Item
      {
         id : root

         implicitWidth : 12

         property alias text : text.text
         readonly property int pos : 0

         Rectangle
         {
            anchors.fill : parent

            id : highlight
            color : ui.theme.backgroundColor3
            opacity : 0
         }

         Text
         {
            anchors.baseline : parent.bottom
            anchors.baselineOffset : -8

            id : text
            color: ui.theme.fontColor2
            font {
               family : ui.theme.timecodeFont.family
               pointSize : 14
               bold : true
            }
         }

         MouseArea
         {
            anchors.fill : parent

            id : bbox
            acceptedButtons: Qt.LeftButton
            hoverEnabled: true

            onClicked : {
               root.focus = true
            }
         }

         states : [
            State {
               name: "hover"
               when: bbox.containsMouse

               PropertyChanges {
                  target: highlight
                  opacity: 1.0
               }
            },
            State {
               name: "selected"
               when: activeFocus

               PropertyChanges {
                  target : highlight
                  opacity: 0.8
               }
            }
         ]
      }
   }

   RowLayout
   {
      id : mainLayout

      anchors.fill : parent
      spacing : 1

      Rectangle
      {
         Layout.fillHeight : true

         implicitWidth : timeLayout.Layout.minimumWidth + timeLayout.anchors.leftMargin + timeLayout.anchors.rightMargin

         radius : 3
         color : ui.theme.backgroundColor4

         RowLayout
         {
            id : timeLayout
            anchors.fill : parent
            anchors.leftMargin : 8
            anchors.rightMargin : 8
            spacing : 0

            property var fields
         }
      }

      Rectangle
      {
         Layout.fillHeight : true

         implicitWidth : 16

         radius : 3
         color : ui.theme.backgroundColor4
         Text
         {
            anchors.fill : parent

            verticalAlignment: Text.AlignVCenter
            horizontalAlignment: Text.AlignHCenter
            color: ui.theme.fontColor2
            text: String.fromCharCode(IconCode.SMALL_ARROW_DOWN)

            font {
               family: ui.theme.iconFont.family
               pointSize: 16
            }
         }

         MouseArea {
            id: changeFormatArea
            anchors.fill: parent
            acceptedButtons: Qt.LeftButton
            hoverEnabled: true
            onClicked: {
               var menu = formatSelectMenuComponent.createObject(
                  root,
                  {
                     itemSourceModel : formatsModel,
                     numericTextControl : root
                  }
               )
               menu.popup(mouseX, mouseY)
            }
         }
      }
   }

   onTypeChanged : {
      formatsModel.clear()
      var formats = formatterFactory.formats(type)
      for(var i = 0; i < formats.length; ++i)
         formatsModel.append({ name : formats[i] })
      formatIndex = 0
   }

   onFormatterChanged : {
      if(!formatter)
         return
      updateLayout()
   }

   onValueChanged : updateDigits()

   function setFormatIndex(index) {
      if(formatIndex != index)
      {
         formatIndex = index
         updateLayout()
      }
   }

   function updateDigits() {
      var digits = formatter.valueToDigits(value)
      for(var i = 0; i < digits.length; ++i)
      {
         var digit = digits[i]
         timeLayout.fields[digit.field][digit.index].text = digit.value
      }
   }

   function updateLayout() {
      var layout = formatter.formatLayout()
      timeLayout.children = []
      timeLayout.fields = []
      var pos = 0
      for(var i = 0; i < layout.length; ++i)
      {
         timeLayout.fields[i] = []
         var field = layout[i]
         for(var j = 0; j < field.digits; ++j)
         {
            var slot = digitSlotComponent.createObject(timeLayout, { text : '0', pos : pos } )
            slot.Layout.fillHeight = true
            timeLayout.fields[i].push(slot)

            ++pos
         }
         var label
         if(field.label === ',' || field.label === '.' || field.label === '/')
            label = separatorSymbolComponent.createObject(timeLayout, { text : field.label.trim() })
         else
            label = timeUnitComponent.createObject(timeLayout, { text : field.label.trim() })
         label.anchors.baseline = Qt.binding(function() { return timeLayout.bottom })
         label.anchors.baselineOffset = -8
         label.Layout.fillHeight = true
      }
      updateDigits()
   }

   function singleStep(forward)
   {
      var fields = timeLayout.fields
      for(var i = 0; i < fields.length; ++i)
      {
         for(var j = 0; j < fields[i].length; ++j)
         {
            if(fields[i][j].activeFocus)
            {
               value = formatter.singleStep(value, fields[i][j].pos, forward)
               return true
            }
         }
      }
      return false
   }


   Keys.onUpPressed : (event) => {
      if(singleStep(true))
         event.accepted = true
   }

   Keys.onDownPressed : (event) => {
      if(singleStep(false))
         event.accepted = true
   }

   Keys.onLeftPressed : (event) => {
      var prev
      var fields = timeLayout.fields
      for(var i = 0; i < fields.length; ++i)
      {
         for(var j = 0; j < fields[i].length; ++j)
         {
            if(fields[i][j].activeFocus && prev)
            {
               prev.focus = true
               event.accepted = true
               return
            }
            prev = fields[i][j]
         }
      }
   }

   Keys.onRightPressed : (event) => {
      var prev
      var fields = timeLayout.fields
      for(var i = fields.length - 1; i >= 0; --i)
      {
         for(var j = fields[i].length - 1; j >= 0; --j)
         {
            if(fields[i][j].activeFocus && prev)
            {
               prev.focus = true
               event.accepted = true
               return
            }
            prev = fields[i][j]
         }
      }
   }
}
