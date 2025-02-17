/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportPluginHelpers.h

  Dominic Mazzoni

  Vitaly Sverchinsky split from ExportPlugin.h

**********************************************************************/

#pragma once

#include <memory>

#include "ExportPlugin.h"
#include "ExportTypes.h"
#include "SampleFormat.h"

class TrackList;
class WaveTrack;
class Mixer;

namespace MixerOptions {
class Downmix;
}

///\brief Utility class that provides helper functions for ExportPlugin
class IMPORT_EXPORT_API ExportPluginHelpers final
{
public:
    static std::unique_ptr<Mixer> CreateMixer(
        const AudacityProject& project, bool selectionOnly, double startTime, double stopTime, unsigned numOutChannels,
        size_t outBufferSize, bool outInterleaved, double outRate, sampleFormat outFormat, MixerOptions::Downmix* mixerSpec);

    ///\brief Sends progress update to delegate and retrieves state update from it.
    ///Typically used inside each export iteration.
    static ExportResult UpdateProgress(ExportProcessorDelegate& delegate, Mixer& mixer, double t0, double t1);

    template<typename T>
    static T GetParameterValue(const ExportProcessor::Parameters& parameters, int id, T defaultValue = T())
    {
        auto it = std::find_if(
            parameters.begin(),
            parameters.end(),
            [=](const auto& t) { return std::get<0>(t) == id; });
        if (it != parameters.end()) {
            if (auto value = std::get_if<T>(&std::get<1>(*it))) {
                return *value;
            }
        }
        return defaultValue;
    }
};
