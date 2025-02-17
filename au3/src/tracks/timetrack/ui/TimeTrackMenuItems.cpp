/**********************************************************************

Audacity: A Digital Audio Editor

@file TimeTrackMenuItems.cpp
@brief Injects menu items using TimeTrack but not the views of it

Paul Licameli split from TrackMenus.cpp

**********************************************************************/

#include "CommonCommandFlags.h"
#include "CommandContext.h"
#include "ProjectHistory.h"

#include "SelectUtilities.h"
#include "TimeTrack.h"
#include "TrackFocus.h"
#include "Viewport.h"
#include "CommandContext.h"
#include "MenuRegistry.h"
#include "AudacityMessageBox.h"

namespace {
using namespace MenuRegistry;

void OnNewTimeTrack(const CommandContext& context)
{
    auto& project = context.project;
    auto& tracks = TrackList::Get(project);

    auto t = tracks.AddToHead(std::make_shared<TimeTrack>());

    SelectUtilities::SelectNone(project);

    t->SetSelected(true);

    ProjectHistory::Get(project)
    .PushState(XO("Created new time track"), XO("New Track"));

    TrackFocus::Get(project).Set(t);
    Viewport::Get(project).ShowTrack(*t);
}

const ReservedCommandFlag& TimeTrackDoesNotExistFlag()
{
    static ReservedCommandFlag flag{
        [](const AudacityProject& project){
            return TrackList::Get(project).Any<const TimeTrack>().empty();
        }
    };
    return flag;
}

AttachedItem sAttachment{
    Command(wxT("NewTimeTrack"), XXO("&Time Track"),
            OnNewTimeTrack, AudioIONotBusyFlag() | TimeTrackDoesNotExistFlag()
            ),
    wxT("Tracks/Add/Add")
};
}
