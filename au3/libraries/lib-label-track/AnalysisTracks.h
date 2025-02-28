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

class Effect;
class LabelTrack;
class Track;
class TrackList;

// For the use of analyzers, which don't need to make output wave tracks,
// but may need to add label tracks.
class LABEL_TRACK_API AddedAnalysisTrack
{
    AddedAnalysisTrack(const AddedAnalysisTrack&) = delete;

public:
    AddedAnalysisTrack(Effect* pEffect, const wxString& name);
    AddedAnalysisTrack() {}

    // So you can have a vector of them
    AddedAnalysisTrack(AddedAnalysisTrack&& that);

    LabelTrack* get() const { return mpTrack; }

    // Call this to indicate successful completion of the analyzer.
    void Commit();

    // Destructor undoes the addition of the analysis track if not committed.
    ~AddedAnalysisTrack();

private:
    Effect* mpEffect{};
    LabelTrack* mpTrack{};
};

// Set name to given value if that is not empty, else use default name
std::shared_ptr<AddedAnalysisTrack> LABEL_TRACK_API AddAnalysisTrack(Effect& effect, const wxString& name = wxString());

// For the use of analyzers, which don't need to make output wave tracks,
// but may need to modify label tracks.
class LABEL_TRACK_API ModifiedAnalysisTrack
{
    ModifiedAnalysisTrack(const ModifiedAnalysisTrack&) = delete;

public:
    ModifiedAnalysisTrack(
        Effect* pEffect, const LabelTrack& origTrack, const wxString& name);
    ModifiedAnalysisTrack();

    // So you can have a vector of them
    ModifiedAnalysisTrack(ModifiedAnalysisTrack&& that);

    LabelTrack* get() const { return mpTrack; }

    // Call this to indicate successful completion of the analyzer.
    void Commit();

    // Destructor undoes the modification of the analysis track if not committed.
    ~ModifiedAnalysisTrack();

private:
    Effect* mpEffect{};
    LabelTrack* mpTrack{};
    std::shared_ptr<Track> mpOrigTrack{};
};

// Set name to given value if that is not empty, else use default name
ModifiedAnalysisTrack LABEL_TRACK_API ModifyAnalysisTrack(Effect& effect, const LabelTrack& origTrack, const wxString& name = wxString());

#endif
