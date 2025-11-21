/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MusicInformationRetrieval.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "MusicInformationRetrieval.h"
#include "DecimatingMirAudioReader.h"
#include "GetMeterUsingTatumQuantizationFit.h"
#include "MirProjectInterface.h"
#include "MirTypes.h"
#include "MirUtils.h"
#include "StftFrameProvider.h"

#include "MemoryX.h"

#include <array>
#include <cassert>
#include <cmath>
#include <numeric>
#include <regex>

namespace MIR {
namespace {
// Normal distribution parameters obtained by fitting a gaussian in the GTZAN
// dataset tempo values.
static constexpr auto bpmExpectedValue = 126.3333;

constexpr auto numTimeSignatures = static_cast<int>(TimeSignature::_count);

auto RemovePathPrefix(const std::string& filename)
{
    return filename.substr(filename.find_last_of("/\\") + 1);
}

// When we get time-signature estimate, we may need a map for that, since 6/8
// has 1.5 quarter notes per beat.
constexpr std::array<double, numTimeSignatures> quarternotesPerBeat { 2., 1.,
                                                                      1., 1.5 };
} // namespace

std::optional<ProjectSyncInfo>
GetProjectSyncInfo(const ProjectSyncInfoInput& in)
{
    if (in.tags.has_value() && in.tags->isOneShot) {
        // That's a one-shot file, we don't want to sync it.
        return {}
    }

    std::optional<double> bpm;
    std::optional<TimeSignature> timeSignature;
    std::optional<TempoObtainedFrom> usedMethod;

    if (in.tags.has_value() && in.tags->bpm.has_value() && *in.tags->bpm > 30.) {
        bpm = in.tags->bpm;
        usedMethod = TempoObtainedFrom::Header;
    } else if ((bpm = GetBpmFromFilename(in.filename))) {
        usedMethod = TempoObtainedFrom::Title;
    } else if (
        const auto meter = GetMusicalMeterFromSignal(
            in.source,
            in.viewIsBeatsAndMeasures ? FalsePositiveTolerance::Lenient
            : FalsePositiveTolerance::Strict,
            in.progressCallback)) {
        bpm = meter->bpm;
        timeSignature = meter->timeSignature;
        usedMethod = TempoObtainedFrom::Signal;
    } else {
        return {}
    }

    const auto qpm = *bpm * quarternotesPerBeat[static_cast<int>(
                                                    timeSignature.value_or(TimeSignature::FourFour))];

    auto recommendedStretch = 1.0;
    if (!in.projectWasEmpty) {
        // There already is content in this project, meaning that its tempo won't
        // be changed. Change speed by some power of two to minimize stretching.
        recommendedStretch
            =std::pow(2., std::round(std::log2(in.projectTempo / qpm)));
    }

    auto excessDurationInQuarternotes = 0.;
    auto numQuarters = in.source.GetDuration() * qpm / 60.;
    const auto roundedNumQuarters = std::round(numQuarters);
    const auto delta = numQuarters - roundedNumQuarters;
    // If there is an excess less than a 32nd, we treat it as an edit error.
    if (0 < delta && delta < 1. / 8) {
        excessDurationInQuarternotes = delta;
    }

    return ProjectSyncInfo {
        qpm,
        *usedMethod,
        timeSignature,
        recommendedStretch,
        excessDurationInQuarternotes,
    };
}

std::optional<double> GetBpmFromFilename(const std::string& filename)
{
    // regex matching a forward or backward slash:

    // Regex: <(anything + (directory) separator) or nothing> <2 or 3 digits>
    // <optional separator> <bpm (case-insensitive)> <separator or nothing>
    const std::regex bpmRegex {
        R"((?:.*(?:_|-|\s|\.|/|\\))?(\d+)(?:_|-|\s|\.)?bpm(?:(?:_|-|\s|\.).*)?)",
        std::regex::icase
    };
    std::smatch matches;
    if (std::regex_match(filename, matches, bpmRegex)) {
        try
        {
            const auto value = std::stoi(matches[1]);
            return 30 <= value && value <= 300 ? std::optional<double> { value }
                   : std::nullopt;
        }
        catch (const std::invalid_argument& e)
        {
            assert(false);
        }
    }
    return {};
}

std::optional<MusicalMeter> GetMusicalMeterFromSignal(
    const MirAudioReader& audio, FalsePositiveTolerance tolerance,
    const std::function<void(double)>& progressCallback,
    QuantizationFitDebugOutput* debugOutput)
{
    if (audio.GetSampleRate() <= 0) {
        return {}
    }
    const auto duration = 1. * audio.GetNumSamples() / audio.GetSampleRate();
    if (duration > 60) {
        // A file longer than 1 minute is most likely not a loop, and processing
        // it would be costly.
        return {}
    }
    DecimatingMirAudioReader decimatedAudio { audio };
    return GetMeterUsingTatumQuantizationFit(
        decimatedAudio, tolerance, progressCallback, debugOutput);
}

void SynchronizeProject(
    const std::vector<std::shared_ptr<AnalyzedAudioClip> >& clips,
    ProjectInterface& project, bool projectWasEmpty)
{
    const auto isBeatsAndMeasures = project.ViewIsBeatsAndMeasures();

    if (!projectWasEmpty && !isBeatsAndMeasures) {
        return;
    }

    const auto projectTempo
        =!projectWasEmpty ? std::make_optional(project.GetTempo()) : std::nullopt;

    if (!std::any_of(
            clips.begin(), clips.end(),
            [](const std::shared_ptr<AnalyzedAudioClip>& clip) {
        return clip->GetSyncInfo().has_value();
    })) {
        return;
    }

    Finally Do = [&] {
        // Re-evaluate if we are in B&M view - we might have convinced the user to
        // switch:
        if (!project.ViewIsBeatsAndMeasures()) {
            return;
        }
        std::for_each(
            clips.begin(), clips.end(),
            [&](const std::shared_ptr<AnalyzedAudioClip>& clip) {
            clip->Synchronize();
        });
        project.OnClipsSynchronized();
    };

    if (!projectWasEmpty && isBeatsAndMeasures) {
        return;
    }

    const auto [loopIndices, oneshotIndices] = [&] {
        std::vector<size_t> loopIndices;
        std::vector<size_t> oneshotIndices;
        for (size_t i = 0; i < clips.size(); ++i) {
            if (clips[i]->GetSyncInfo().has_value()) {
                loopIndices.push_back(i);
            } else {
                oneshotIndices.push_back(i);
            }
        }
        return std::make_pair(loopIndices, oneshotIndices);
    }();

    // Favor results based on reliability. We assume that header info is most
    // reliable, followed by title, followed by DSP.
    std::unordered_map<TempoObtainedFrom, size_t> indexMap;
    std::for_each(loopIndices.begin(), loopIndices.end(), [&](size_t i) {
        const auto usedMethod = clips[i]->GetSyncInfo()->usedMethod;
        if (!indexMap.count(usedMethod)) {
            indexMap[usedMethod] = i;
        }
    });

    const auto chosenIndex = indexMap.count(TempoObtainedFrom::Header)
                             ? indexMap.at(TempoObtainedFrom::Header)
                             : indexMap.count(TempoObtainedFrom::Title)
                             ? indexMap.at(TempoObtainedFrom::Title)
                             : indexMap.at(TempoObtainedFrom::Signal);

    const auto& chosenSyncInfo = *clips[chosenIndex]->GetSyncInfo();
    const auto isSingleFileImport = clips.size() == 1;
    if (!project.ShouldBeReconfigured(
            chosenSyncInfo.rawAudioTempo, isSingleFileImport)) {
        return;
    }

    project.ReconfigureMusicGrid(
        chosenSyncInfo.rawAudioTempo, chosenSyncInfo.timeSignature);

    // Reset tempo of one-shots to this new project tempo, so that they don't
    // get stretched:
    std::for_each(oneshotIndices.begin(), oneshotIndices.end(), [&](size_t i) {
        clips[i]->SetRawAudioTempo(chosenSyncInfo.rawAudioTempo);
    });
}
} // namespace MIR
