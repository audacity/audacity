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
        muse::async::Channel<TrackId> channel;
    };

    MasterEffectListRestorer(au3::Au3Project& project)
    {
        append(m_items, RealtimeEffectList::Get(project));
        const auto tracks = TrackList::Get(project).Any<WaveTrack>();
        for (WaveTrack* track : tracks) {
            append(m_items, RealtimeEffectList::Get(*track));
        }
    }

    void RestoreUndoRedoState(au3::Au3Project&) override
    {
        for ( auto& item : m_items) {
            const auto access = item.state->GetAccess();
            EffectSettings settings = item.settings;
            access->Set(std::move(settings));
            access->Flush();
        }
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

    std::vector<StateAndSettings> m_items;
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

void au::effects::setNotificationChannelForMasterEffectUndoRedo(au::au3::Au3Project& project, muse::async::Channel<TrackId> channel)
{
    MasterEffectListRestorer::RealtimeEffectStackChanged::Get(project).channel = channel;
}
