/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackViewConstants.h

Paul Licameli split from class WaveTrack

**********************************************************************/

#ifndef __AUDACITY_WAVE_TRACK_VIEW_CONSTANTS__
#define __AUDACITY_WAVE_TRACK_VIEW_CONSTANTS__

namespace WaveTrackViewConstants
{
   enum Display : int {

      // DO NOT REORDER OLD VALUES!  Replace obsoletes with placeholders.

      Waveform = 0,
      MinDisplay = Waveform,

      obsoleteWaveformDBDisplay,

      Spectrum,

      obsolete1, // was SpectrumLogDisplay
      obsolete2, // was SpectralSelectionDisplay
      obsolete3, // was SpectralSelectionLogDisplay
      obsolete4, // was PitchDisplay

      // Add values here, and update MaxDisplay.

      MaxDisplay = Spectrum,

      NoDisplay,            // Preview track has no display
   };

   // Only two types of sample display for now, but
   // others (eg sinc interpolation) may be added later.
   enum SampleDisplay {
      LinearInterpolate = 0,
      StemPlot
   };

   // Various preset zooming levels.
   enum ZoomPresets {
      kZoomToFit = 0,
      kZoomToSelection,
      kZoomDefault,
      kZoomMinutes,
      kZoomSeconds,
      kZoom5ths,
      kZoom10ths,
      kZoom20ths,
      kZoom50ths,
      kZoom100ths,
      kZoom500ths,
      kZoomMilliSeconds,
      kZoomSamples,
      kZoom4To1,
      kMaxZoom,
   };

   enum ZoomActions {
      // Note that these can be with or without spectrum view which
      // adds a constant.
      kZoom1to1 = 1,
      kZoomTimes2,
      kZoomDiv2,
      kZoomHalfWave,
      kZoomInByDrag,
      kZoomIn,
      kZoomOut,
      kZoomReset
   };

   // Handle remapping of enum values from 2.1.0 and earlier
   Display ConvertLegacyDisplayValue(int oldValue);

   // Handle restriction of range of values of the enum from future versions
   Display ValidateWaveTrackDisplay(Display display);
}

#endif
