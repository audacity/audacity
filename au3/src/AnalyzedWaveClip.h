/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AnalyzedWaveClip.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "MirTypes.h"
#include <memory>

class ClipMirAudioReader;

class AnalyzedWaveClip : public MIR::AnalyzedAudioClip
{
public:
    AnalyzedWaveClip(
        std::shared_ptr<ClipMirAudioReader> reader, std::optional<MIR::ProjectSyncInfo> syncInfo);

    const std::optional<MIR::ProjectSyncInfo>& GetSyncInfo() const override;
    void SetRawAudioTempo(double tempo) override;
    void Synchronize() override;

private:
    const std::shared_ptr<ClipMirAudioReader> mReader;
    const std::optional<MIR::ProjectSyncInfo> mSyncInfo;
};
