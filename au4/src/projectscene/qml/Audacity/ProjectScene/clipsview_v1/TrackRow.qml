import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Qt.labs.qmlmodels

import Muse.UiComponents
import Muse.Ui

import Audacity.ProjectScene

Item
{
   id : container

   readonly property ListView listView : ListView.view

   property alias sidebarWidth : trackControlSlot.width
   property alias trackControl : trackControlSlot.data
   property alias trackView : trackViewSlot.data

   width : listView.width
   height : 126

   //z : dragArea.drag.active ? 1 : 0
   //opacity : dragArea.drag.active ? ui.theme.opacityLight : ui.theme.opacityOpaque

   // Drag.active : dragArea.drag.active
   // Drag.hotSpot.y : height / 2

   // MouseArea {
   //    id : dragArea
   //    anchors.fill : trackControlSlot

   //    cursorShape : undefined

   //    drag.target : container
   //    drag.axis : Drag.YAxis

   //    property real containerInitialY : 0

   //    onPressed : {
   //       containerInitialY = container.y
   //    }

   //    onReleased : {
   //       if(drag.active)
   //       {
   //          if(drag.target == null || !container.Drag.drop())
   //             container.y = containerInitialY
   //       }
   //    }
   // }

   Rectangle
   {
      id : trackControlSlot
      width : 280
      anchors {
         top : parent.top
         left : parent.left
         bottom : parent.bottom
      }
      //color : ui.theme.backgroundColor3

      // Rectangle {
      //    id: bottomBorder
      //    anchors {
      //       bottom : parent.bottom
      //       left : parent.left
      //       right : parent.right
      //    }

      //    height: 2

      //    color: dropArea.entered ? ui.theme.brandColor : ui.theme.strokeColor1
      //    opacity: ui.theme.opacityLight
      // }

      // MouseArea
      // {
      //    id : resizeArea
      //    anchors {
      //       left : parent.left
      //       right : parent.right
      //       bottom : parent.bottom
      //    }
      //    cursorShape : Qt.SizeVerCursor
      //    height : 4
      //    hoverEnabled : true
      //    preventStealing : true

      //    onMouseYChanged : {
      //       if(pressed)
      //          container.height = container.height + mouseY
      //    }
      // }
   }

   Item {
      id : trackViewSlot
      anchors {
         left : trackControlSlot.right
         right : parent.right
         top : parent.top
         bottom : parent.bottom
      }
   }

   DropArea {
      id : dropArea
      anchors.fill : trackControlSlot
      property bool entered : false
      onEntered : (drag) => {
         entered = drag.source.DelegateModel.itemsIndex != DelegateModel.itemsIndex
      }
      onExited : {
         entered = false
      }
      onDropped : (drop) => {
         entered = false
         proxyModel.model.move(drop.source.DelegateModel.itemsIndex, DelegateModel.itemsIndex)
         drop.accept()
      }
   }
}
