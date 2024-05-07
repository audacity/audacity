import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Qt.labs.qmlmodels

import Muse.UiComponents
import Muse.Ui

import Audacity.ProjectScene

TimelineContext
{
   id : view

   tracksOriginOffset : 280

   property alias model : proxyModel.model
   property alias color : background.color

   model: TracksListClipsModel {
       id: trackListClipsModel
   }

   Component.onCompleted: {
       trackListClipsModel.load()
   }

   Rectangle {

      anchors.fill : parent

      id : background
   }

   Item {
      id : header

      anchors {
         top : parent.top
         left : parent.left
         right : parent.right
      }

      height : 32
   }

   ListView
   {
      id : trackListView

      anchors
      {
         top : header.bottom
         left : parent.left
         right : parent.right
         bottom : parent.bottom
      }

      clip : true

      pixelAligned : true

      flickableDirection : Flickable.VerticalFlick

      footer : Item { height : 100 }

      model : DelegateModel
      {
         id : proxyModel

         delegate : DelegateChooser
         {
            role: "type"
            DelegateChoice
            {
               roleValue : "waveTrack"
               TrackRow {
                  id : row
                  required property WaveTrackAdapter track

                  sidebarWidth : view.tracksOriginOffset

                  // trackControl : WaveTrackControlPanel {
                  //    anchors.fill : parent
                  //    track : row.track
                  // }
                  trackView : WaveTrackView {
                     anchors.fill : parent
                     context : view
                     track : row.track
                  }
               }
            }
         }
      }
   }

   MouseArea
   {
      anchors.fill : trackListView

      cursorShape : undefined

      acceptedButtons : Qt.NoButton
      preventStealing : true

      onWheel: (wheel) => {
         var steps = wheel.angleDelta.y / 120
         if (wheel.modifiers & Qt.ControlModifier)
         {
            var xx = wheel.x - view.tracksOriginOffset
            var zoom = view.zoom * Math.pow(2, steps / 4)
            var pivot = view.positionToTime(xx)

            view.zoom = zoom
            view.offset = Math.max(
               0,
               view.positionToTime(view.timeToPosition(pivot) - xx)
            )
            view.zoom = zoom
         }
         else if(wheel.modifiers & Qt.ShiftModifier)
         {
            view.offset = Math.max(
               0,
               view.offset + (view.width - view.tracksOriginOffset) / view.zoom / 10 * steps
            );
         }
         else
            wheel.accepted = false
      }
   }

   MouseArea
   {
      id : resizeArea

      x : view.tracksOriginOffset - width / 2
      width : 5
      height : parent.height

      hoverEnabled : true
      cursorShape : Qt.SizeHorCursor
      preventStealing : true

      onMouseXChanged : {
         if(pressed)
            view.tracksOriginOffset = view.tracksOriginOffset + mouseX
      }
   }
}
