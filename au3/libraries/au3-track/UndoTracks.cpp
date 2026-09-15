/**********************************************************************

 Audacity: A Digital Audio Editor

 @file UndoTracks.cpp

 Paul Licameli

 **********************************************************************/
#include "UndoTracks.h"
#include "PendingTracks.h"
#include "Track.h"
#include "au3-project-history/UndoManager.h"
#include "au3-realtime-effects/RealtimeEffectList.h"

// Undo/redo handling of selection changes
namespace {
struct TrackListRestorer final : UndoStateExtension {
    TrackListRestorer(AudacityProject& project)
        : mpTracks{TrackList::Get(project).Duplicate()}
    {
        // Effect settings are captured separately by RealtimeEffectRestorer.
        const auto& tracks = TrackList::Get(project);
        for (auto pTrack : *mpTracks) {
            RealtimeEffectList::ShareStates(*pTrack, *tracks.FindById(pTrack->GetId()));
        }
    }

    void RestoreUndoRedoState(AudacityProject& project) override
    {
        auto& dstTracks = TrackList::Get(project);
        constexpr auto synchrony = TrackList::EventPublicationSynchrony::Asynchronous;
        dstTracks.BeginUndoRedo(synchrony);
        constexpr auto sendEvent = true;
        dstTracks.Clear(sendEvent);
        for (auto pTrack : *mpTracks) {
            auto restored = pTrack->Duplicate(Track::DuplicateOptions {}.Backup());
            RealtimeEffectList::ShareStates(*restored, *pTrack);
            dstTracks.Add(std::move(restored), TrackList::DoAssignId::No, synchrony);
        }
        dstTracks.EndUndoRedo(synchrony);
    }

    bool CanUndoOrRedo(const AudacityProject& project) override
    {
        return !PendingTracks::Get(project).HasPendingTracks();
    }

    const std::shared_ptr<TrackList> mpTracks;
};

UndoRedoExtensionRegistry::Entry<TrackListRestorer> sEntry { [] (AudacityProject& project)->std::shared_ptr<UndoStateExtension> {
        return std::make_shared<TrackListRestorer>(project);
    }
};
}

TrackList* UndoTracks::Find(const UndoStackElem& state)
{
    auto& exts = state.state.extensions;
    auto end = exts.end(),
         iter = std::find_if(exts.begin(), end, [](const std::pair<const std::type_index, std::shared_ptr<UndoStateExtension> >& entry){
        return dynamic_cast<TrackListRestorer*>(entry.second.get());
    });
    if (iter != end) {
        return static_cast<TrackListRestorer*>(iter->second.get())->mpTracks.get();
    }
    return nullptr;
}
