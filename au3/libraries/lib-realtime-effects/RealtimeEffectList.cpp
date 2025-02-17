/**********************************************************************

  Audacity: A Digital Audio Editor

  RealtimeEffectList.cpp

 *********************************************************************/

#include "RealtimeEffectList.h"
#include "RealtimeEffectState.h"

#include "Channel.h"
#include "Project.h"
#include "UndoManager.h"

RealtimeEffectList::RealtimeEffectList()
{
}

RealtimeEffectList::RealtimeEffectList(const RealtimeEffectList& other)
{
    *this = other;
}

RealtimeEffectList::~RealtimeEffectList()
{
}

std::unique_ptr<ClientData::Cloneable<> > RealtimeEffectList::Clone() const
{
    return std::make_unique<RealtimeEffectList>(*this);
}

RealtimeEffectList& RealtimeEffectList::operator=(const RealtimeEffectList& other)
{
    mStates.clear();
    for (auto& pState : other.mStates) {
        mStates.push_back(pState);
    }
    SetActive(other.IsActive());
    return *this;
}

// Access for per-project effect list
static const AttachedProjectObjects::RegisteredFactory masterEffects
{
    [](AudacityProject& project)
    {
        return std::make_shared<RealtimeEffectList>();
    }
};

RealtimeEffectList& RealtimeEffectList::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get<RealtimeEffectList>(masterEffects);
}

RealtimeEffectList& RealtimeEffectList::Set(
    AudacityProject& project, const std::shared_ptr<RealtimeEffectList>& list)
{
    auto& result = *list;
    project.AttachedObjects::Assign(masterEffects, list);
    return result;
}

const RealtimeEffectList&
RealtimeEffectList::Get(const AudacityProject& project)
{
    return Get(const_cast<AudacityProject&>(project));
}

static const ChannelGroup::Attachments::RegisteredFactory
    channelGroupEffects
{
    [](auto&)
    {
        return std::make_unique<RealtimeEffectList>();
    }
};

// Access for per-group effect list
RealtimeEffectList& RealtimeEffectList::Get(ChannelGroup& group)
{
    return group.Attachments::Get<RealtimeEffectList>(channelGroupEffects);
}

const RealtimeEffectList& RealtimeEffectList::Get(
    const ChannelGroup& group)
{
    return Get(const_cast<ChannelGroup&>(group));
}

bool
RealtimeEffectList::AddState(std::shared_ptr<RealtimeEffectState> pState)
{
    const auto& id = pState->GetID();
    if (pState->GetEffect() != nullptr) {
        auto shallowCopy = mStates;
        shallowCopy.emplace_back(pState);
        // Lock for only a short time
        (LockGuard{ mLock }, swap(shallowCopy, mStates));

        Publisher<RealtimeEffectListMessage>::Publish({
            RealtimeEffectListMessage::Type::Insert,
            mStates.size() - 1,
            { },
            pState
        });

        return true;
    } else {
        // Effect initialization failed for the id
        return false;
    }
}

bool
RealtimeEffectList::ReplaceState(size_t index,
                                 std::shared_ptr<RealtimeEffectState> pState)
{
    if (index >= mStates.size()) {
        return false;
    }
    const auto& id = pState->GetID();
    if (pState->GetEffect() != nullptr) {
        auto shallowCopy = mStates;

        Publisher<RealtimeEffectListMessage>::Publish({
            RealtimeEffectListMessage::Type::WillReplace,
            index,
            { },
            shallowCopy[index]
        });

        swap(pState, shallowCopy[index]);
        // Lock for only a short time
        (LockGuard{ mLock }, swap(shallowCopy, mStates));

        Publisher<RealtimeEffectListMessage>::Publish({
            RealtimeEffectListMessage::Type::DidReplace,
            index,
            { },
            pState
        });

        return true;
    } else {
        // Effect initialization failed for the id
        return false;
    }
}

void RealtimeEffectList::RemoveState(
    const std::shared_ptr<RealtimeEffectState> pState)
{
    auto shallowCopy = mStates;
    auto end = shallowCopy.end(),
         found = std::find(shallowCopy.begin(), end, pState);
    if (found != end) {
        const auto index = std::distance(shallowCopy.begin(), found);
        shallowCopy.erase(found);

        // Lock for only a short time
        (LockGuard{ mLock }, swap(shallowCopy, mStates));

        Publisher<RealtimeEffectListMessage>::Publish({
            RealtimeEffectListMessage::Type::Remove,
            static_cast<size_t>(index),
            { },
            pState
        });
    }
}

void RealtimeEffectList::Clear()
{
    decltype(mStates) temp;

    // Swap an empty list in as a whole, not removing one at a time
    // Lock for only a short time
    (LockGuard{ mLock }, swap(temp, mStates));

    for (auto index = temp.size(); index--;) {
        Publisher<RealtimeEffectListMessage>::Publish(
            { RealtimeEffectListMessage::Type::Remove, index, {}, temp[index] });
    }
}

std::optional<size_t> RealtimeEffectList::FindState(
    const std::shared_ptr<RealtimeEffectState>& pState) const
{
    const auto begin = mStates.begin(),
               end = mStates.end(),
               iter = std::find(begin, end, pState);
    if (iter == end) {
        return {};
    }
    return std::distance(begin, iter);
}

size_t RealtimeEffectList::GetStatesCount() const noexcept
{
    return mStates.size();
}

std::shared_ptr<RealtimeEffectState>
RealtimeEffectList::GetStateAt(size_t index) noexcept
{
    if (index < mStates.size()) {
        return mStates[index];
    }
    return nullptr;
}

std::shared_ptr<const RealtimeEffectState>
RealtimeEffectList::GetStateAt(size_t index) const noexcept
{
    return const_cast<RealtimeEffectList*>(this)->GetStateAt(index);
}

void RealtimeEffectList::MoveEffect(size_t fromIndex, size_t toIndex)
{
    assert(fromIndex < mStates.size());
    assert(toIndex < mStates.size());

    auto shallowCopy = mStates;
    if (fromIndex == toIndex) {
        return;
    }
    if (fromIndex < toIndex) {
        const auto first = shallowCopy.begin() + fromIndex;
        const auto last = shallowCopy.begin() + toIndex + 1;
        std::rotate(first, first + 1, last);
    } else {
        const auto first
            =shallowCopy.rbegin() + (shallowCopy.size() - (fromIndex + 1));
        const auto last = shallowCopy.rbegin() + (shallowCopy.size() - toIndex);
        std::rotate(first, first + 1, last);
    }
    // Lock for only a short time
    (LockGuard{ mLock }, swap(shallowCopy, mStates));

    Publisher<RealtimeEffectListMessage>::Publish({
        RealtimeEffectListMessage::Type::Move,
        fromIndex,
        toIndex,
        mStates[toIndex]
    });
}

const std::string& RealtimeEffectList::XMLTag()
{
    static const std::string result{ "effects" };
    return result;
}

static constexpr auto activeAttribute = "active";

bool RealtimeEffectList::HandleXMLTag(
    const std::string_view& tag, const AttributesList& attrs)
{
    if (tag == XMLTag()) {
        for (auto&[attr, value] : attrs) {
            if (attr == activeAttribute) {
                SetActive(value.Get<bool>());
            }
        }
        return true;
    }
    return false;
}

XMLTagHandler* RealtimeEffectList::HandleXMLChild(const std::string_view& tag)
{
    if (tag == RealtimeEffectState::XMLTag()) {
        mStates.push_back(RealtimeEffectState::make_shared(PluginID {}));
        return mStates.back().get();
    }
    return nullptr;
}

void RealtimeEffectList::WriteXML(XMLWriter& xmlFile) const
{
    xmlFile.StartTag(XMLTag());
    xmlFile.WriteAttr(activeAttribute, IsActive());
    for (const auto& state : mStates) {
        state->WriteXML(xmlFile);
    }
    xmlFile.EndTag(XMLTag());
}

bool RealtimeEffectList::IsActive() const
{
    return mActive.load(std::memory_order_relaxed);
}

void RealtimeEffectList::SetActive(bool value)
{
    (LockGuard{ mLock }, mActive.store(value, std::memory_order_relaxed));
}
