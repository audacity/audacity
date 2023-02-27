/**********************************************************************

  Audacity: A Digital Audio Editor

  AnalysisTracks.h

  Dominic Mazzoni
  Vaughan Johnson

  Paul Licameli split from Effect.h

**********************************************************************/
#ifndef __AUDACITY_ANALYSIS_TRACKS__
#define __AUDACITY_ANALYSIS_TRACKS__

#include <wx/string.h>
#include <memory>

class LabelTrack;
class Track;
class TrackList;

// For the use of analyzers, which don't need to make output wave tracks,
// but may need to add label tracks.
class LABEL_TRACK_API AddedAnalysisTrack {
   AddedAnalysisTrack(const AddedAnalysisTrack&) = delete;

public:
   AddedAnalysisTrack(TrackList &trackList, const wxString &name);

   // So you can have a vector of them
   AddedAnalysisTrack(AddedAnalysisTrack &&that);

   LabelTrack *get() const { return mpTrack; }

   // Call this to indicate successful completion of the analyzer.
   void Commit();

   // Destructor undoes the addition of the analysis track if not committed.
   ~AddedAnalysisTrack();

private:
   TrackList *mpTrackList{};
   LabelTrack *mpTrack{};
};

// Set name to given value if that is not empty, else use default name
LABEL_TRACK_API
std::shared_ptr<AddedAnalysisTrack> AddAnalysisTrack(TrackList &trackList,
   const wxString &name = wxString());

// For the use of analyzers, which don't need to make output wave tracks,
// but may need to modify label tracks.
class LABEL_TRACK_API ModifiedAnalysisTrack {
   ModifiedAnalysisTrack(const ModifiedAnalysisTrack&) = delete;

public:
   ModifiedAnalysisTrack(
      TrackList &trackList, const LabelTrack &origTrack, const wxString &name);
   ModifiedAnalysisTrack();

   // So you can have a vector of them
   ModifiedAnalysisTrack(ModifiedAnalysisTrack &&that);

   LabelTrack *get() const { return mpTrack; }

   // Call this to indicate successful completion of the analyzer.
   void Commit();

   // Destructor undoes the modification of the analysis track if not committed.
   ~ModifiedAnalysisTrack();

private:
   TrackList *mpTrackList{};
   LabelTrack *mpTrack{};
   std::shared_ptr<TrackList> mpOrigTrack{};
};

// Set name to given value if that is not empty, else use default name
LABEL_TRACK_API
ModifiedAnalysisTrack ModifyAnalysisTrack(TrackList &trackList,
   const LabelTrack &origTrack, const wxString &name = wxString());

#endif
