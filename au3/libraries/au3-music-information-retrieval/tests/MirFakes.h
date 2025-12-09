/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MirFakes.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "MirProjectInterface.h"
#include "MirTypes.h"

namespace MIR {
class EmptyMirAudioReader : public MirAudioReader
{
    double GetSampleRate() const override
    {
        return 0;
    }

    long long GetNumSamples() const override
    {
        return 0;
    }

    void
    ReadFloats(float* buffer, long long start, size_t numFrames) const override
    {
    }
};

class SquareWaveMirAudioReader : public MirAudioReader
{
    const int period = 8;

    double GetSampleRate() const override
    {
        return 10;
    }

    long long GetNumSamples() const override
    {
        return period * 10;
    }

    void
    ReadFloats(float* buffer, long long where, size_t numFrames) const override
    {
        for (size_t i = 0; i < numFrames; ++i) {
            buffer[i] = (where + i) % period < period / 2 ? 1.f : -1.f;
        }
    }
};

class FakeProjectInterface final : public ProjectInterface
{
public:
    FakeProjectInterface(double tempo)
        : projectTempo{tempo}
    {
    }

    ~FakeProjectInterface() override = default;

    double projectTempo;
    bool isBeatsAndMeasures = false;
    bool clipsWereSynchronized = false;
    bool shouldBeReconfigured = false;

    bool wasReconfigured = false;

    bool ViewIsBeatsAndMeasures() const override
    {
        return isBeatsAndMeasures;
    }

    void ReconfigureMusicGrid(
        double newTempo, std::optional<MIR::TimeSignature> timeSignature) override
    {
        projectTempo = newTempo;
        isBeatsAndMeasures = true;
        wasReconfigured = true;
    }

    double GetTempo() const override
    {
        return projectTempo;
    }

    bool ShouldBeReconfigured(double qpm, bool isSingleFileImport) override
    {
        return shouldBeReconfigured;
    }

    void OnClipsSynchronized() override
    {
        clipsWereSynchronized = true;
    }
};

class FakeAnalyzedAudioClip final : public AnalyzedAudioClip
{
public:
    struct Params
    {
        const double tempo;
        const TempoObtainedFrom method;
    };

    FakeAnalyzedAudioClip(std::optional<Params> params)
        : syncInfo{params.has_value() ? std::make_optional<ProjectSyncInfo>(
                       { params->tempo, params->method })
                   : std::nullopt}
    {
    }

    ~FakeAnalyzedAudioClip() override = default;

    const std::optional<MIR::ProjectSyncInfo> syncInfo;
    std::optional<double> rawAudioTempo;
    bool synchronizeCalled = false;

    const std::optional<MIR::ProjectSyncInfo>& GetSyncInfo() const override
    {
        return syncInfo;
    }

    void SetRawAudioTempo(double tempo) override
    {
        rawAudioTempo = tempo;
    }

    void Synchronize() override
    {
        synchronizeCalled = true;
    }
};
} // namespace MIR
