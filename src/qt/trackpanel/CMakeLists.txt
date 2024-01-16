set(MODULE_NAME trackpanel)

set(MODULE_SOURCES
   handles/TimeSelectionHandle.cpp
   handles/TimeSelectionHandle.h
   handles/WaveClipShiftHandle.cpp
   handles/WaveClipShiftHandle.h
   handles/WaveClipTrimHandle.cpp
   handles/WaveClipTrimHandle.h
   items/WaveClipItem.cpp
   items/WaveClipItem.h
   TimelineContext.cpp
   TimelineContext.h
   TimelineView.cpp
   TimelineView.h
   TimelineViewItem.cpp
   TimelineViewItem.h
   TimelineViewUIHandle.cpp
   TimelineViewUIHandle.h
   TrackAdapter.cpp
   TrackAdapter.h
   TrackListModel.cpp
   TrackListModel.h
   WaveformCache.cpp
   WaveformCache.h
   WaveTrackAdapter.cpp
   WaveTrackAdapter.h
   WaveTrackView.cpp
   WaveTrackView.h
)

audacity_qml_module(${MODULE_NAME}
   URI "Audacity.TrackPanel"
   STATIC
   VERSION 1.0
   DEPENDENCIES
      QtQuick
      Audacity.UiThemes
   SOURCES
      ${MODULE_SOURCES}
   LIBRARIES
      Qt6::Quick
      lib-track-interface
      lib-graphics-interface
      lib-wave-track-interface
      lib-wave-track-paint-interface
)
