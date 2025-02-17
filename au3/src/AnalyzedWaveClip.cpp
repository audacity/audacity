/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AnalyzedWaveClip.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "AnalyzedWaveClip.h"

#include "ClipMirAudioReader.h"
#include "WaveClip.h"

AnalyzedWaveClip::AnalyzedWaveClip(
    std::shared_ptr<ClipMirAudioReader> reader,
    std::optional<MIR::ProjectSyncInfo> syncInfo)
    : mReader{std::move(reader)}
    , mSyncInfo{syncInfo}
{
    assert(mReader);
}

const std::optional<MIR::ProjectSyncInfo>& AnalyzedWaveClip::GetSyncInfo() const
{
    return mSyncInfo;
}

void AnalyzedWaveClip::SetRawAudioTempo(double tempo)
{
    if (mReader) {
        mReader->clip->SetRawAudioTempo(tempo);
    }
}

void AnalyzedWaveClip::Synchronize()
{
    if (!mReader || !mSyncInfo) {
        return;
    }
    auto& clip = *mReader->clip;
    clip.SetRawAudioTempo(mSyncInfo->rawAudioTempo);
    clip.TrimQuarternotesFromRight(mSyncInfo->excessDurationInQuarternotes);
    clip.StretchBy(mSyncInfo->stretchMinimizingPowOfTwo);
}
