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
#include "LabelTrack.h"

AddedAnalysisTrack::AddedAnalysisTrack(
   TrackList &trackList, const wxString &name)
   : mpTrackList(&trackList)
{
   if(!name.empty())
      mpTrack = LabelTrack::Create(*mpTrackList, name);
   else
      mpTrack = LabelTrack::Create(*mpTrackList);
}

AddedAnalysisTrack::AddedAnalysisTrack(AddedAnalysisTrack &&that)
{
   mpTrackList = that.mpTrackList;
   mpTrack = that.mpTrack;
   that.Commit();
}

void AddedAnalysisTrack::Commit()
{
   mpTrackList = nullptr;
}

AddedAnalysisTrack::~AddedAnalysisTrack()
{
   if (mpTrackList) {
      // Label tracks are always leaders
      assert(mpTrack->IsLeader());
      // not committed -- DELETE the label track
      mpTrackList->Remove(*mpTrack);
   }
}

std::shared_ptr<AddedAnalysisTrack> AddAnalysisTrack(
   TrackList &trackList, const wxString &name)
{
   return std::shared_ptr<AddedAnalysisTrack>
      { safenew AddedAnalysisTrack{ trackList, name } };
}

ModifiedAnalysisTrack::ModifiedAnalysisTrack(
   TrackList &trackList, const LabelTrack &origTrack, const wxString &name
)  : mpTrackList(&trackList)
{
   // copy LabelTrack here, so it can be undone on cancel
   const auto startTime = origTrack.GetStartTime();
   auto list = origTrack.Copy(startTime, origTrack.GetEndTime());
   auto newTrack = (*list->begin())->SharedPointer();

   mpTrack = static_cast<LabelTrack*>(newTrack.get());

   // Why doesn't LabelTrack::Copy complete the job? :
   mpTrack->MoveTo(startTime);
   if (!name.empty())
      mpTrack->SetName(name);

   // mpOrigTrack came from mTracks which we own but expose as const to subclasses
   // So it's okay that we cast it back to const
   mpOrigTrack =
      mpTrackList->ReplaceOne(const_cast<LabelTrack&>(origTrack),
         std::move(*list));
}

ModifiedAnalysisTrack::ModifiedAnalysisTrack(ModifiedAnalysisTrack &&that)
{
   mpTrackList = that.mpTrackList;
   mpTrack = that.mpTrack;
   mpOrigTrack = std::move(that.mpOrigTrack);
   that.Commit();
}

void ModifiedAnalysisTrack::Commit()
{
   mpTrackList = nullptr;
}

ModifiedAnalysisTrack::~ModifiedAnalysisTrack()
{
   if (mpTrackList && mpTrack) {
      // not committed -- DELETE the label track
      // mpOrigTrack came from mTracks which we own but expose as const to
      // subclasses
      // So it's okay that we cast it back to const
      mpTrackList->ReplaceOne(*mpTrack, std::move(*mpOrigTrack));
   }
}

ModifiedAnalysisTrack ModifyAnalysisTrack(
   TrackList &trackList, const LabelTrack &origTrack, const wxString &name)
{
   return{ trackList, origTrack, name };
}
