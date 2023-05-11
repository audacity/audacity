/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportUtils.h
 
  Dominic Mazzoni

  Vitaly Sverchinsky split from ExportPlugin.h

**********************************************************************/

#pragma once

#include <memory>

#include "SampleFormat.h"

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
};

