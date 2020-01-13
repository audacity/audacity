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
