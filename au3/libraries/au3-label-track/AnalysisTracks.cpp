/**********************************************************************

  Audacity: A Digital Audio Editor

  AnalysisTracks.cpp

  Dominic Mazzoni
  Vaughan Johnson
  Martyn Shaw

  Paul Licameli split from Effect.cpp

*******************************************************************//**

\class Effect
\brief Base class for many of the effects in Audacity.

*//*******************************************************************/
#include "AnalysisTracks.h"
#include "Effect.h"
#include "LabelTrack.h"

AddedAnalysisTrack::AddedAnalysisTrack(Effect* pEffect, const wxString& name)
    : mpEffect(pEffect)
{
    if (!name.empty()) {
        mpTrack = LabelTrack::Create(*pEffect->mTracks, name);
    } else {
        mpTrack = LabelTrack::Create(*pEffect->mTracks);
    }
}

AddedAnalysisTrack::AddedAnalysisTrack(AddedAnalysisTrack&& that)
{
    mpEffect = that.mpEffect;
    mpTrack = that.mpTrack;
    that.Commit();
}

void AddedAnalysisTrack::Commit()
{
    mpEffect = nullptr;
}

AddedAnalysisTrack::~AddedAnalysisTrack()
{
    if (mpEffect) {
        // not committed -- DELETE the label track
        mpEffect->mTracks->Remove(*mpTrack);
    }
}

std::shared_ptr<AddedAnalysisTrack> AddAnalysisTrack(
    Effect& effect, const wxString& name)
{
    return std::shared_ptr<AddedAnalysisTrack>
           { safenew AddedAnalysisTrack{ &effect, name } };
}

ModifiedAnalysisTrack::ModifiedAnalysisTrack(
    Effect* pEffect, const LabelTrack& origTrack, const wxString& name)
    : mpEffect(pEffect)
{
    // copy LabelTrack here, so it can be undone on cancel
    const auto startTime = origTrack.GetStartTime();
    auto newTrack = origTrack.Copy(startTime, origTrack.GetEndTime());

    mpTrack = static_cast<LabelTrack*>(newTrack.get());

    // Why doesn't LabelTrack::Copy complete the job? :
    mpTrack->MoveTo(startTime);
    if (!name.empty()) {
        mpTrack->SetName(name);
    }

    // mpOrigTrack came from mTracks which we own but expose as const to subclasses
    // So it's okay that we cast it back to const
    mpOrigTrack
        =pEffect->mTracks->ReplaceOne(const_cast<LabelTrack&>(origTrack),
                                      std::move(*TrackList::Temporary(nullptr, newTrack)));
}

ModifiedAnalysisTrack::ModifiedAnalysisTrack(ModifiedAnalysisTrack&& that)
{
    mpEffect = that.mpEffect;
    mpTrack = that.mpTrack;
    mpOrigTrack = std::move(that.mpOrigTrack);
    that.Commit();
}

void ModifiedAnalysisTrack::Commit()
{
    mpEffect = nullptr;
}

ModifiedAnalysisTrack::~ModifiedAnalysisTrack()
{
    if (mpEffect && mpTrack) {
        // not committed -- DELETE the label track
        // mpOrigTrack came from mTracks which we own but expose as const to subclasses
        // So it's okay that we cast it back to const
        mpEffect->mTracks->ReplaceOne(*mpTrack,
                                      std::move(*TrackList::Temporary(nullptr, mpOrigTrack)));
    }
}

ModifiedAnalysisTrack ModifyAnalysisTrack(
    Effect& effect, const LabelTrack& origTrack, const wxString& name)
{
    return{ &effect, origTrack, name };
}
