/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackViewConstants.cpp

Paul Licameli split from class WaveTrack

**********************************************************************/

#include "WaveTrackViewConstants.h"

// static
WaveTrackViewConstants::Display
WaveTrackViewConstants::ConvertLegacyDisplayValue(int oldValue)
{
   // Remap old values.
   enum class OldValues {
      Waveform,
      WaveformDB,
      Spectrogram,
      SpectrogramLogF,
      Pitch,
   };

   Display newValue;
   switch ((OldValues)oldValue) {
   default:
   case OldValues::Waveform:
      newValue = Waveform; break;
   case OldValues::WaveformDB:
      newValue = obsoleteWaveformDBDisplay; break;
   case OldValues::Spectrogram:
   case OldValues::SpectrogramLogF:
   case OldValues::Pitch:
      newValue = Spectrum; break;
      /*
   case SpectrogramLogF:
      newValue = WaveTrack::SpectrumLogDisplay; break;
   case Pitch:
      newValue = WaveTrack::PitchDisplay; break;
      */
   }
   return newValue;
}

// static
WaveTrackViewConstants::Display
WaveTrackViewConstants::ValidateWaveTrackDisplay(Display display)
{
   switch (display) {
      // non-obsolete codes
   case Waveform:
   case obsoleteWaveformDBDisplay:
   case Spectrum:
      return display;

      // obsolete codes
   case obsolete1: // was SpectrumLogDisplay
   case obsolete2: // was SpectralSelectionDisplay
   case obsolete3: // was SpectralSelectionLogDisplay
   case obsolete4: // was PitchDisplay
      return Spectrum;

      // codes out of bounds (from future prefs files?)
   default:
      return MinDisplay;
   }
}
