/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportUtils.h
 
  Dominic Mazzoni

  Vitaly Sverchinsky split from ExportPlugin.h

**********************************************************************/

#pragma once

#include <memory>

#include "SampleFormat.h"
#include "ExportTypes.h"
#include "ExportPlugin.h"

class TrackList;
class WaveTrack;
class Mixer;
class TranslatableString;
class wxFileNameWrapper;

namespace MixerOptions
{
class Downmix;
}

template <typename TrackType> struct TrackIterRange;

class ExportUtils final
{
public:

   static std::unique_ptr<Mixer> CreateMixer(const TrackList &tracks,
         bool selectionOnly,
         double startTime, double stopTime,
         unsigned numOutChannels, size_t outBufferSize, bool outInterleaved,
         double outRate, sampleFormat outFormat,
         MixerOptions::Downmix *mixerSpec);

   ///\return Mixer current position relative to the [t0, t1]
   static double EvalExportProgress(Mixer& mixer, double t0, double t1);
   
   static TrackIterRange<const WaveTrack> FindExportWaveTracks(const TrackList& tracks, bool selectedOnly);

   static ExportPlugin::Parameters ParametersFromEditor(const ExportOptionsEditor& editor);

   template<typename T>
   static T GetParameterValue(const ExportPlugin::Parameters& parameters, int id, T defaultValue = T())
   {
      auto it = std::find_if(
         parameters.begin(),
         parameters.end(),
         [=](const auto& t) { return std::get<0>(t) == id; });
      if(it != parameters.end())
      {
         if(auto value = std::get_if<T>(&std::get<1>(*it)))
            return *value;
      }
      return defaultValue;
   }
};

