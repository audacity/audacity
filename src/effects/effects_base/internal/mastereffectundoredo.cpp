/*
 * Audacity: A Digital Audio Editor
 */
#include "mastereffectundoredo.h"

#include "irealtimeeffectservice.h"

#include "libraries/lib-project/Project.h"
#include "libraries/lib-project-history/UndoManager.h"
#include "libraries/lib-realtime-effects/RealtimeEffectList.h"
#include "libraries/lib-realtime-effects/RealtimeEffectState.h"
#include "libraries/lib-wave-track/WaveTrack.h"

namespace au::effects {
namespace {
bool operator!=(const RealtimeEffectList& a, const RealtimeEffectList& b)
{
    if (a.GetStatesCount() != b.GetStatesCount()) {
        return true;
    }
    for (auto i = 0; i < static_cast<int>(a.GetStatesCount()); ++i) {
        if (a.GetStateAt(i) != b.GetStateAt(i)) {
            return true;
        }
    }
    return false;
}

class MasterEffectListRestorer final : public UndoStateExtension
{
public:
    struct RealtimeEffectStackChanged : public ClientData::Base
    {
        static RealtimeEffectStackChanged& Get(AudacityProject& project);
        IRealtimeEffectService* service = nullptr;
    };

    MasterEffectListRestorer(au3::Au3Project& project)
        : m_masterEffectList{std::make_unique<RealtimeEffectList>(RealtimeEffectList::Get(project))}
    {
        append(m_items, RealtimeEffectList::Get(project));
        const auto tracks = TrackList::Get(project).Any<WaveTrack>();
        for (WaveTrack* track : tracks) {
            append(m_items, RealtimeEffectList::Get(*track));
        }
    }

    void RestoreUndoRedoState(au3::Au3Project& project) override
    {
        for ( auto& item : m_items) {
            const auto access = item.state->GetAccess();
            EffectSettings settings = item.settings;
            access->Set(std::move(settings));
            access->Flush();
        }
        IRealtimeEffectService* const service = RealtimeEffectStackChanged::Get(project).service;
        if (!m_items.empty()) {
            service->notifyAboutEffectSettingsChanged();
        }
        // Use this `UndoStateExtension` to detect changes in the master effect list.
        auto& currentList = RealtimeEffectList::Get(project);
        if (currentList != *m_masterEffectList) {
            currentList = *m_masterEffectList;
            service->notifyAboutEffectStackChanged(IRealtimeEffectService::masterTrackId);
        }
        // At the moment, the changes in the track effect lists are managed in the RealtimeEffectService
        // (See use of `m_modifiedTracks`). Maybe it'd be better to have this extension also manage these?
    }

private:
    struct StateAndSettings {
        StateAndSettings(RealtimeEffectStatePtr state, EffectSettings settings)
            : state(std::move(state)), settings(std::move(settings))
        {
        }

        const RealtimeEffectStatePtr state;
        EffectSettings settings;
    };

    static void append(std::vector<StateAndSettings>& items, RealtimeEffectList& list)
    {
        for (auto i = 0; i < static_cast<int>(list.GetStatesCount()); ++i) {
            const auto state = list.GetStateAt(i);
            items.emplace_back(state, state->GetSettings());
        }
    }

    // TODO make const
    std::vector<StateAndSettings> m_items;
    const std::unique_ptr<RealtimeEffectList> m_masterEffectList;
};

static UndoRedoExtensionRegistry::Entry sEntry {
    [](au3::Au3Project& project) -> std::shared_ptr<UndoStateExtension>
    {
        return std::make_shared<MasterEffectListRestorer>(project);
    }
};

static const AudacityProject::AttachedObjects::RegisteredFactory key{
    [](au3::Au3Project&)
    {
        return std::make_shared<MasterEffectListRestorer::RealtimeEffectStackChanged>();
    } };
}

MasterEffectListRestorer::RealtimeEffectStackChanged& MasterEffectListRestorer::RealtimeEffectStackChanged::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get<RealtimeEffectStackChanged>(key);
}
}

void au::effects::setNotificationChannelForMasterEffectUndoRedo(au::au3::Au3Project& project, IRealtimeEffectService* service)
{
    MasterEffectListRestorer::RealtimeEffectStackChanged::Get(project).service = service;
}
