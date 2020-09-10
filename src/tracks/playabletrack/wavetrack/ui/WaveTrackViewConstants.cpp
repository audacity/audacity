/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackViewConstants.cpp

Paul Licameli split from class WaveTrack

**********************************************************************/

#include "../../../../Internat.h"
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

namespace {
   class Registry {
   public:
      using Type = WaveTrackSubViewType;
      using Types = std::vector< Type >;

      void Append( Type type )
      {
         types.emplace_back( std::move( type ) );
         sorted = false;
      }

      Types &Get()
      {
         if ( !sorted ) {
            auto begin = types.begin(), end = types.end();
            std::sort( begin, end );
            // We don't want duplicate ids!
            wxASSERT( end == std::adjacent_find( begin, end ) );
            sorted = true;
         }
         return types;
      }

   private:
      Types types;
      bool sorted = false;
   };

   Registry &GetRegistry()
   {
      static Registry result;
      return result;
   }
}

WaveTrackSubViewType::RegisteredType::RegisteredType( WaveTrackSubViewType type )
{
   GetRegistry().Append( std::move( type ) );
}

// static
auto WaveTrackSubViewType::All()
   -> const std::vector<WaveTrackSubViewType> &
{
   return GetRegistry().Get();
}

// static
auto WaveTrackSubViewType::Default() -> Display
{
   auto &all = All();
   if (all.empty())
      return WaveTrackViewConstants::Waveform;
   return all[0].id;
}

const EnumValueSymbol WaveTrackViewConstants::MultiViewSymbol{
   wxT("Multiview"), XXO("&Multi-view")
};
