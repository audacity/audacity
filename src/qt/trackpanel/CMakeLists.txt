set(MODULE_NAME trackpanel)

set(MODULE_SOURCES
   TrackAdapter.cpp
   TrackAdapter.h
   TrackListModel.cpp
   TrackListModel.h
   WaveTrackAdapter.cpp
   WaveTrackAdapter.h
)

audacity_qml_module(${MODULE_NAME}
   URI "Audacity.TrackPanel"
   STATIC
   VERSION 1.0
   DEPENDENCIES
      QtQuick
   SOURCES
      ${MODULE_SOURCES}
   LIBRARIES
      Qt6::Quick
      lib-track-interface
      lib-wave-track-interface
)
