/**********************************************************************

Audacity: A Digital Audio Editor

@file DropoutDetector.cpp
@brief Attaches recording dropout event handler to each project

Paul Licameli split from ProjectAudioManager.cpp

**********************************************************************/

#include "ClientData.h"
#include "LabelTrack.h"
#include "Observer.h"
#include "Project.h"
#include "ProjectAudioManager.h"
#include "ProjectHistory.h"
#include "ProjectWindows.h"
#include "widgets/Warning.h"
#include <wx/app.h>
#include <wx/frame.h>

namespace {
struct DropoutSubscription : ClientData::Base {
    DropoutSubscription(AudacityProject& project)
    {
        mSubscription = ProjectAudioManager::Get(project).Subscribe(
            [&project](const RecordingDropoutEvent& evt){
            // Make a track with labels for recording errors
            auto& tracks = TrackList::Get(project);

            /* i18n-hint:  A name given to a track, appearing as its menu button.
             The translation should be short or else it will not display well.
             At most, about 11 Latin characters.
             Dropout is a loss of a short sequence of audio sample data from the
             recording */
            auto pTrack = LabelTrack::Create(tracks, tracks.MakeUniqueTrackName(_("Dropouts")));
            long counter = 1;
            for (auto& interval : evt.intervals) {
                pTrack->AddLabel(
                    SelectedRegion { interval.first,
                                     interval.first + interval.second },
                    wxString::Format(wxT("%ld"), counter++));
            }

            auto& history = ProjectHistory::Get(project);
            history.ModifyState(true); // this might fail and throw

            // CallAfter so that we avoid any problems of yielding
            // to the event loop while still inside the timer callback,
            // entering StopStream() recursively
            auto& window = GetProjectFrame(project);
            wxTheApp->CallAfter([&window] {
                ShowWarningDialog(&window, wxT("DropoutDetected"),
                                  XO(
                                      "\
Recorded audio was lost at the labeled locations. Possible causes:\n\
\n\
Other applications are competing with Audacity for processor time\n\
\n\
You are saving directly to a slow external storage device\n\
"
                                      ),
                                  false,
                                  XXO("Turn off dropout detection"));
            });
        });
    }

    Observer::Subscription mSubscription;
};
}

static AudacityProject::AttachedObjects::RegisteredFactory sKey {
    []( AudacityProject& project ) {
        return std::make_shared<DropoutSubscription>(project);
    }
};
