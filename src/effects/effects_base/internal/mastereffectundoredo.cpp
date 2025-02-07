/*
 * Audacity: A Digital Audio Editor
 */
#include "mastereffectundoredo.h"

#include "irealtimeeffectservice.h"

#include "libraries/lib-project/Project.h"
#include "libraries/lib-project-history/UndoManager.h"
#include "libraries/lib-realtime-effects/RealtimeEffectList.h"
#include "libraries/lib-realtime-effects/RealtimeEffectState.h"

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
        : m_targetList(std::make_unique<RealtimeEffectList>(RealtimeEffectList::Get(project)))
    {
    }

    void RestoreUndoRedoState(au3::Au3Project& project) override
    {
        auto& changed = RealtimeEffectStackChanged::Get(project).channel;
        auto& currentList = RealtimeEffectList::Get(project);
        if (currentList != *m_targetList) {
            currentList = *m_targetList;
            changed.send(IRealtimeEffectService::masterTrackId);
        }
    }

    muse::async::Channel<TrackId> realtimeEffectStackChanged;

private:
    const std::unique_ptr<RealtimeEffectList> m_targetList;
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
