#include "realtimeeffectservice.h"

#include "libraries/lib-audio-io/AudioIO.h"

#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-effects/EffectManager.h"
#include "libraries/lib-realtime-effects/RealtimeEffectState.h"
#include "libraries/lib-realtime-effects/RealtimeEffectList.h"
#include "libraries/lib-project-history/UndoManager.h"

#include "au3wrap/au3types.h"
#include "au3wrap/internal/domaccessor.h"

#include "project/iaudacityproject.h"

#include "global/types/translatablestring.h"

#include <unordered_map>

namespace au::effects {
std::string RealtimeEffectService::getEffectName(const RealtimeEffectState& state) const
{
    return effectsProvider()->effectName(state.GetID().ToStdString());
}

void RealtimeEffectService::init()
{
    globalContext()->currentProjectChanged().onNotify(this, [this]
    {
        updateSubscriptions(globalContext()->currentProject());
    });

    updateSubscriptions(globalContext()->currentProject());
}

void RealtimeEffectService::updateSubscriptions(const au::project::IAudacityProjectPtr& project)
{
    m_rtEffectSubscriptions.clear();

    if (!project) {
        return;
    }

    auto au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());

    auto& trackList = TrackList::Get(*au3Project);
    for (WaveTrack* track : trackList.Any<WaveTrack>()) {
        onWaveTrackAdded(*track);
    }

    m_tracklistSubscription = trackList.Subscribe([this, w = weak_from_this()](const TrackListEvent& e)
    {
        const auto lifeguard = w.lock();
        if (lifeguard) {
            onTrackListEvent(e);
        }
    });

    // Regular tracks get added to the project and we will get the notifications, but we have to do it proactively for the master track.
    initializeTrackOnStackManager(masterTrackId);
    m_rtEffectSubscriptions[masterTrackId] = subscribeToRealtimeEffectList(masterTrackId, RealtimeEffectList::Get(*au3Project));
}

void RealtimeEffectService::onTrackListEvent(const TrackListEvent& e)
{
    switch (e.mType) {
    case TrackListEvent::ADDITION: {
        if (const auto waveTrack = std::dynamic_pointer_cast<WaveTrack>(e.mpTrack.lock())) {
            onWaveTrackAdded(*waveTrack);
        }
    }
    break;
    case TrackListEvent::DELETION:
    {
        IF_ASSERT_FAILED(e.mId.has_value()) {
            return;
        }
        m_rtEffectSubscriptions.erase(*e.mId);
        stackManager()->remove(*e.mId);
    }
    break;
    }
}

void RealtimeEffectService::onWaveTrackAdded(WaveTrack& track)
{
    // Renew the subscription: `track` may have a registered ID, it may yet be a new object.
    auto& list = RealtimeEffectList::Get(track);
    m_rtEffectSubscriptions[track.GetId()] = subscribeToRealtimeEffectList(track.GetId(), list);
    initializeTrackOnStackManager(track.GetId());
}

void RealtimeEffectService::initializeTrackOnStackManager(au::effects::TrackId trackId)
{
    const auto list = realtimeEffectList(trackId);
    IF_ASSERT_FAILED(list) {
        return;
    }
    stackManager()->remove(trackId);
    for (auto i = 0u; i < list->GetStatesCount(); ++i) {
        stackManager()->insert(trackId, i, list->GetStateAt(i));
    }
}

Observer::Subscription RealtimeEffectService::subscribeToRealtimeEffectList(TrackId trackId, RealtimeEffectList& list)
{
    return list.Subscribe([this, trackId, weakThis = weak_from_this()](const RealtimeEffectListMessage& msg)
    {
        const auto lifeguard = weakThis.lock();
        if (!lifeguard) {
            return;
        }
        const auto list = realtimeEffectList(trackId);
        IF_ASSERT_FAILED(list) {
            return;
        }

        const std::string effectName = getEffectName(*msg.affectedState);
        const std::optional<std::string> trackName = effectTrackName(trackId);
        assert(trackName.has_value());

        switch (msg.type) {
        case RealtimeEffectListMessage::Type::Insert:
            stackManager()->insert(trackId, msg.srcIndex, msg.affectedState);
            projectHistory()->pushHistoryState("Added " + effectName + " to " + trackName.value_or(""), "Add " + effectName);
            break;
        case RealtimeEffectListMessage::Type::Remove:
            stackManager()->remove(msg.affectedState);
            projectHistory()->pushHistoryState("Removed " + effectName + " from " + trackName.value_or(""), "Remove " + effectName);
            break;
        case RealtimeEffectListMessage::Type::DidReplace:
            {
                const std::shared_ptr<RealtimeEffectState> newState = list->GetStateAt(msg.srcIndex);
                IF_ASSERT_FAILED(newState) {
                    return;
                }
                stackManager()->replace(msg.affectedState, newState);
                const auto newEffectName = getEffectName(*newState);
                projectHistory()->pushHistoryState("Replaced " + effectName + " with " + newEffectName, "Replace " + effectName);
            }
            break;
        case RealtimeEffectListMessage::Type::Move:
            stackManager()->reorder(msg.affectedState, msg.dstIndex);
            projectHistory()->pushHistoryState("Moved " + effectName
                                               + (msg.srcIndex < msg.dstIndex ? " up " : " down ") + " in " + trackName.value_or(""),
                                               "Change effect order");
            break;
        }
    });
}

muse::async::Channel<TrackId> RealtimeEffectService::realtimeEffectStackChanged() const
{
    return stackManager()->realtimeEffectStackChanged();
}

std::optional<TrackId> RealtimeEffectService::trackId(const RealtimeEffectStatePtr& stateId) const
{
    return stackManager()->trackId(stateId);
}

std::optional<std::string> RealtimeEffectService::effectTrackName(const RealtimeEffectStatePtr& state) const
{
    const auto trackId = stackManager()->trackId(state);
    if (!trackId.has_value()) {
        return {};
    }
    return effectTrackName(*trackId);
}

std::optional<EffectChainLinkIndex> RealtimeEffectService::effectIndex(const RealtimeEffectStatePtr& state) const
{
    return stackManager()->effectIndex(state);
}

std::optional<std::vector<RealtimeEffectStatePtr> > RealtimeEffectService::effectStack(TrackId trackId) const
{
    return stackManager()->effectStack(trackId);
}

void RealtimeEffectService::reorderRealtimeEffect(const RealtimeEffectStatePtr& state, int newIndex)
{
    const auto trackId = stackManager()->trackId(state);
    if (!trackId.has_value()) {
        return;
    }
    const auto oldIndex = stackManager()->effectIndex(state);
    IF_ASSERT_FAILED(oldIndex.has_value()) {
        return;
    }
    RealtimeEffectList* const list = realtimeEffectList(*trackId);
    list->MoveEffect(*oldIndex, newIndex);
}

std::optional<std::string> RealtimeEffectService::effectTrackName(TrackId trackId) const
{
    if (trackId == masterTrackId) {
        return muse::TranslatableString("effects", "Master").translated().toStdString();
    }
    const auto data = utilData(trackId);
    if (data) {
        return data->trackeditProject->trackName(trackId);
    } else {
        return {};
    }
}

std::optional<RealtimeEffectService::UtilData> RealtimeEffectService::utilData(TrackId trackId) const
{
    const project::IAudacityProjectPtr project = globalContext()->currentProject();
    if (!project) {
        return {};
    }

    const auto au3Project = reinterpret_cast<au3::Au3Project*>(project->au3ProjectPtr());
    const auto isMasterTrack = trackId == masterTrackId;
    const auto au3Track
        = isMasterTrack ? nullptr : dynamic_cast<au3::Au3Track*>(au3::DomAccessor::findTrack(*au3Project,
                                                                                             au3::Au3TrackId(trackId)));
    const auto trackeditProject = globalContext()->currentTrackeditProject().get();
    const auto effectList = au3Track ? &RealtimeEffectList::Get(*au3Track) : &RealtimeEffectList::Get(*au3Project);

    if (!au3Project || !(isMasterTrack || au3Track) || !trackeditProject) {
        return {};
    }
    return UtilData{ au3Project, au3Track, trackeditProject, effectList };
}

RealtimeEffectStatePtr RealtimeEffectService::addRealtimeEffect(TrackId trackId, const muse::String& effectId)
{
    const auto data = utilData(trackId);
    IF_ASSERT_FAILED(data) {
        return nullptr;
    }

    return AudioIO::Get()->AddState(*data->au3Project, data->au3Track, effectId.toStdString());
}

namespace {
std::shared_ptr<RealtimeEffectState> findEffectState(RealtimeEffectList& effectList, RealtimeEffectStatePtr effectStateId)
{
    for (auto i = 0; i < effectList.GetStatesCount(); ++i) {
        const auto state = effectList.GetStateAt(i);
        if (state == effectStateId) {
            return state;
        }
    }
    return nullptr;
}
}

void RealtimeEffectService::removeRealtimeEffect(TrackId trackId, const RealtimeEffectStatePtr& effectStateId)
{
    const auto data = utilData(trackId);
    IF_ASSERT_FAILED(data) {
        return;
    }

    const auto state = findEffectState(*data->effectList, effectStateId);
    IF_ASSERT_FAILED(state) {
        return;
    }

    AudioIO::Get()->RemoveState(*data->au3Project, data->au3Track, state);
}

RealtimeEffectStatePtr RealtimeEffectService::replaceRealtimeEffect(TrackId trackId, int effectListIndex, const muse::String& newEffectId)
{
    const auto data = utilData(trackId);
    IF_ASSERT_FAILED(data) {
        return nullptr;
    }

    return AudioIO::Get()->ReplaceState(*data->au3Project, data->au3Track, effectListIndex, newEffectId.toStdString());
}

bool RealtimeEffectService::isActive(const RealtimeEffectStatePtr& stateId) const
{
    if (stateId == 0) {
        return false;
    }
    return stateId->GetSettings().extra.GetActive();
}

void RealtimeEffectService::setIsActive(const RealtimeEffectStatePtr& stateId, bool isActive)
{
    if (stateId->GetSettings().extra.GetActive() == isActive) {
        return;
    }
    stateId->SetActive(isActive);
    m_isActiveChanged.send(stateId);
}

muse::async::Channel<RealtimeEffectStatePtr> RealtimeEffectService::isActiveChanged() const
{
    return m_isActiveChanged;
}

bool RealtimeEffectService::trackEffectsActive(TrackId trackId) const
{
    const auto list = realtimeEffectList(trackId);
    return list ? list->IsActive() : false;
}

void RealtimeEffectService::setTrackEffectsActive(TrackId trackId, bool active)
{
    const auto list = realtimeEffectList(trackId);
    if (list) {
        list->SetActive(active);
    }
}

const RealtimeEffectList* RealtimeEffectService::realtimeEffectList(TrackId trackId) const
{
    const auto data = utilData(trackId);
    if (!data) {
        return nullptr;
    }
    return data->effectList;
}

RealtimeEffectList* RealtimeEffectService::realtimeEffectList(TrackId trackId)
{
    return const_cast<RealtimeEffectList*>(const_cast<const RealtimeEffectService*>(this)->realtimeEffectList(trackId));
}

namespace {
auto trackEffectMap(au3::Au3Project& project)
{
    std::unordered_map<effects::TrackId, std::unique_ptr<RealtimeEffectList> > map;
    for (WaveTrack* track : TrackList::Get(project).Any<WaveTrack>()) {
        map.emplace(track->GetId(), RealtimeEffectList::Get(*track).Duplicate());
    }
    return map;
}
}

/*!
 * \brief The AU3 `Track` objects have a `RealtimeEffectList` attached.
 * The `TrackListRestorer`, when adding a new history state, stores a copy of the track as well as all its attachments.
 * This way, when undoing/redoing, the RealtimeEffectService gets the list pointers of the previous/next state.
 * Here we do the same, but for the master track.
 */
struct MasterEffectListRestorer final : UndoStateExtension
{
    MasterEffectListRestorer(au3::Au3Project& project)
        : masterEffectList{RealtimeEffectList::Get(project).Duplicate()}, trackEffectLists{trackEffectMap(project)}
    {
    }

    void RestoreUndoRedoState(au3::Au3Project& project) override
    {
        // Shallow copy so as not to get the RealtimeEffectList events ;
        // these are taken care of in this service uniformly for master and regular tracks.
        masterEffectList->ShallowCopyTo(RealtimeEffectList::Get(project));

        TrackList& trackList = TrackList::Get(project);
        for (WaveTrack* track : trackList.Any<WaveTrack>()) {
            const auto it = trackEffectLists.find(track->GetId());
            if (it != trackEffectLists.end()) {
                it->second->ShallowCopyTo(RealtimeEffectList::Get(*track));
            }
        }
    }

    const std::unique_ptr<RealtimeEffectList> masterEffectList;
    const std::unordered_map<effects::TrackId, std::unique_ptr<RealtimeEffectList> > trackEffectLists;
};

static UndoRedoExtensionRegistry::Entry sEntry {
    [](au3::Au3Project& project) -> std::shared_ptr<UndoStateExtension>
    {
        return std::make_shared<MasterEffectListRestorer>(project);
    }
};
}

// Inject a factory for realtime effects
static RealtimeEffectState::EffectFactory::Scope scope{ &EffectManager::GetInstanceFactory };
