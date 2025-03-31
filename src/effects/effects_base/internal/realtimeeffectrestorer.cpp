/*
 * Audacity: A Digital Audio Editor
 */
#include "realtimeeffectrestorer.h"

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

struct Signals : public ClientData::Base
{
    static Signals& Get(au3::Au3Project& project);
    muse::async::Channel<TrackId> stackChanged;
    muse::async::Notification settingsChanged;
};

class RealtimeEffectRestorer final : public UndoStateExtension
{
public:
    RealtimeEffectRestorer(au3::Au3Project& project)
        : m_items(getStateAndSettingsItems(project)),
        m_masterEffectList{std::make_unique<RealtimeEffectList>(RealtimeEffectList::Get(project))}
    {
    }

    void RestoreUndoRedoState(au3::Au3Project& project) override
    {
        for (const StateAndSettings& item : m_items) {
            const auto access = item.state->GetAccess();
            EffectSettings settings = item.settings;
            item.state->SetActive(item.settings.extra.GetActive());
            access->Set(std::move(settings));
            access->Flush();
        }

        // Use this `UndoStateExtension` to detect changes in the master effect list.
        auto& currentList = RealtimeEffectList::Get(project);
        if (currentList != *m_masterEffectList) {
            currentList = *m_masterEffectList;
            Signals::Get(project).stackChanged.send(IRealtimeEffectService::masterTrackId);
        }
        // At the moment, the changes in the track effect lists are managed in the RealtimeEffectService
        // (See use of `m_modifiedTracks`). Maybe it'd be better to have this extension also manage these?

        if (!m_items.empty()) {
            Signals::Get(project).settingsChanged.notify();
        }
    }

private:
    struct StateAndSettings {
        StateAndSettings(RealtimeEffectStatePtr state, EffectSettings settings)
            : state(std::move(state)), settings(std::move(settings))
        {
        }

        const RealtimeEffectStatePtr state;
        const EffectSettings settings;
    };

    static void append(std::vector<StateAndSettings>& items, RealtimeEffectList& list)
    {
        for (auto i = 0; i < static_cast<int>(list.GetStatesCount()); ++i) {
            const auto state = list.GetStateAt(i);
            items.emplace_back(state, state->GetSettings());
        }
    }

    static std::vector<StateAndSettings> getStateAndSettingsItems(au3::Au3Project& project)
    {
        std::vector<StateAndSettings> items;
        append(items, RealtimeEffectList::Get(project));
        const auto tracks = TrackList::Get(project).Any<WaveTrack>();
        for (WaveTrack* track : tracks) {
            append(items, RealtimeEffectList::Get(*track));
        }
        return items;
    }

    const std::vector<StateAndSettings> m_items;
    const std::unique_ptr<RealtimeEffectList> m_masterEffectList;
};

static UndoRedoExtensionRegistry::Entry sEntry {
    [](au3::Au3Project& project) -> std::shared_ptr<UndoStateExtension>
    {
        return std::make_shared<RealtimeEffectRestorer>(project);
    }
};

static const au3::Au3Project::AttachedObjects::RegisteredFactory key{
    [](au3::Au3Project&)
    {
        return std::make_shared<Signals>();
    } };
}

Signals& Signals::Get(au3::Au3Project& project)
{
    return project.AttachedObjects::Get<Signals>(key);
}
}

void au::effects::setRealtimeEffectRestorerSignals(au::au3::Au3Project& project, RealtimeEffectRestorerSignals sigs)
{
    auto& restorerSignals = Signals::Get(project);
    restorerSignals.settingsChanged = std::move(sigs.settingsChanged);
    restorerSignals.stackChanged = std::move(sigs.stackChanged);
}
