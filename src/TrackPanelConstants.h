/*!********************************************************************
 
 Audacity: A Digital Audio Editor
 
 TrackPanelConstants.h
 
 Paul Licameli split from ViewInfo.h
 
 **********************************************************************/

#ifndef  __AUDACITY_TRACK_PANEL_CONSTANTS__
#define  __AUDACITY_TRACK_PANEL_CONSTANTS__

#include "ZoomInfo.h"

// See big pictorial comment in TrackPanel.cpp for explanation of these numbers
//! constants related to y coordinates in the track panel
enum : int {
   kAffordancesAreaHeight = 20,
   kTopInset = 4,
   kTopMargin = kTopInset + kBorderThickness,
   kBottomMargin = kShadowThickness + kBorderThickness,
   kTrackSeparatorThickness = kBottomMargin + kTopMargin,
   kChannelSeparatorThickness = kTrackSeparatorThickness,
};

#endif
