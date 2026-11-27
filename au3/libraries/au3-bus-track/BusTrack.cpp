/*
 * Audacity: A Digital Audio Editor
 */
#include "BusTrack.h"

#include <algorithm>
#include <cstdio>
#include <set>

#include "au3-project/Project.h"
#include "au3-realtime-effects/RealtimeEffectList.h"
#include "au3-strings/TranslatableString.h"
#include "au3-xml/XMLWriter.h"

namespace {
const std::string xmlTag = "bustrack";
constexpr auto outputAttr = "output";
constexpr auto sendsAttr = "sends";

//! The bus at `index` in the tracks of `track`, if it is one, and not `track`
const BusTrack* busAt(const Track& track, size_t index)
{
    const auto owner = track.GetOwner();
    if (!owner) {
        return nullptr;
    }
    size_t i = 0;
    for (const auto other : *owner) {
        if (i++ == index) {
            const auto bus = dynamic_cast<const BusTrack*>(other);
            return bus != &track ? bus : nullptr;
        }
    }
    return nullptr;
}

//! The position of `bus` in its tracks
std::optional<size_t> indexOf(const BusTrack& bus)
{
    size_t index = 0;
    for (const auto other : *bus.GetOwner()) {
        if (other == &bus) {
            return index;
        }
        ++index;
    }
    return std::nullopt;
}

const BusTrack* findBus(const Track& track, TrackId id)
{
    const auto owner = track.GetOwner();
    return owner ? dynamic_cast<const BusTrack*>(owner->FindById(id)) : nullptr;
}

const Track::TypeInfo& typeInfo()
{
    static const Track::TypeInfo info { { "bus", "bus", TranslatableString("bus-track", "Bus") }, true,
                                        &PlayableTrack::ClassTypeInfo() };
    return info;
}
}

static ProjectFileIORegistry::ObjectReaderEntry readerEntry {
    xmlTag,
    [](AudacityProject& project) -> XMLTagHandler* { return BusTrack::New(project); }
};

static const AttachedTrackObjects::RegisteredFactory trackOutputKey {
    [](Track& track) { return std::make_shared<TrackOutput>(track); }
};

static const AttachedTrackObjects::RegisteredFactory trackSendsKey {
    [](Track& track) { return std::make_shared<TrackSends>(track); }
};

wxString BusTrack::GetDefaultName()
{
    return wxString::FromUTF8(au3::trc("bus-track", "Bus").c_str());
}

BusTrack* BusTrack::New(AudacityProject& project)
{
    auto& tracks = TrackList::Get(project);
    auto result = tracks.Add(std::make_shared<BusTrack>());
    result->AttachedTrackObjects::BuildAll();
    return result;
}

BusTrack::BusTrack()
{
    SetName(GetDefaultName());
}

BusTrack::BusTrack(const BusTrack& orig, ProtectedCreationArg&& a)
    : UniqueChannelTrack{orig, std::move(a)}
{
}

void BusTrack::Init(const BusTrack& orig)
{
    Track::Init(orig);
    SetName(orig.GetName());
    mVolume = orig.GetVolume();
    mPan = orig.GetPan();
}

auto BusTrack::GetTypeInfo() const -> const TypeInfo&
{
    return typeInfo();
}

auto BusTrack::ClassTypeInfo() -> const TypeInfo&
{
    return typeInfo();
}

void BusTrack::SetVolume(float volume)
{
    if (GetVolume() != volume) {
        mVolume = volume;
        Notify(true);
    }
}

void BusTrack::SetPan(float pan)
{
    pan = std::clamp(pan, -1.f, 1.f);
    if (GetPan() != pan) {
        mPan = pan;
        Notify(true);
    }
}

float BusTrack::GetChannelVolume(int channel) const
{
    const auto pan = GetPan();
    const float left = pan > 0 ? 1.f - pan : 1.f;
    const float right = pan < 0 ? pan + 1.f : 1.f;
    return (channel % 2 == 0 ? left : right) * GetVolume();
}

Track::Holder BusTrack::PasteInto(AudacityProject&, TrackList& list) const
{
    auto result = std::make_shared<BusTrack>();
    result->Init(*this);
    list.Add(result);
    return result;
}

Track::Holder BusTrack::Cut(double t0, double t1, bool)
{
    return Copy(t0, t1, false);
}

Track::Holder BusTrack::Copy(double, double, bool) const
{
    return Clone(false);
}

Track::Holder BusTrack::Clone(bool) const
{
    auto result = std::make_shared<BusTrack>(*this, ProtectedCreationArg {});
    result->Init(*this);
    return result;
}

Track::Holder BusTrack::TrackEmptyCopy() const
{
    return Clone(false);
}

bool BusTrack::HandleXMLTag(const std::string_view& tag, const AttributesList& attrs)
{
    if (tag != xmlTag) {
        return false;
    }
    for (const auto& [attr, value] : attrs) {
        double number = 0.0;
        if (Track::HandleCommonXMLAttribute(attr, value) || PlayableTrack::HandleXMLAttribute(attr, value)) {
        } else if (attr == "volume" && value.TryGet(number)) {
            mVolume = static_cast<float>(number);
        } else if (attr == "pan" && value.TryGet(number)) {
            mPan = std::clamp(static_cast<float>(number), -1.f, 1.f);
        }
    }
    return true;
}

XMLTagHandler* BusTrack::HandleXMLChild(const std::string_view& tag)
{
    if (tag == RealtimeEffectList::XMLTag()) {
        return &RealtimeEffectList::Get(*this);
    }
    return nullptr;
}

void BusTrack::WriteXML(XMLWriter& xmlFile) const
{
    xmlFile.StartTag(wxString::FromUTF8(xmlTag));
    Track::WriteCommonXMLAttributes(xmlFile);
    PlayableTrack::WriteXMLAttributes(xmlFile);
    xmlFile.WriteAttr(wxT("volume"), static_cast<double>(GetVolume()));
    xmlFile.WriteAttr(wxT("pan"), static_cast<double>(GetPan()));
    RealtimeEffectList::Get(*this).WriteXML(xmlFile);
    xmlFile.EndTag(wxString::FromUTF8(xmlTag));
}

TrackOutput& TrackOutput::Get(Track& track)
{
    return track.AttachedTrackObjects::Get<TrackOutput>(trackOutputKey);
}

const TrackOutput& TrackOutput::Get(const Track& track)
{
    return Get(const_cast<Track&>(track));
}

TrackOutput::TrackOutput(Track& track)
    : mTrack{track.shared_from_this()}
{
}

void TrackOutput::SetBus(std::optional<TrackId> bus)
{
    mBus = bus;
    mSavedBus.reset();
}

const BusTrack* TrackOutput::FindBus() const
{
    const auto track = mTrack.lock();
    return track && mBus ? findBus(*track, *mBus) : nullptr;
}

void TrackOutput::ResolveSaved()
{
    const auto track = mTrack.lock();
    if (!mSavedBus || !track) {
        return;
    }
    if (const auto bus = busAt(*track, *mSavedBus)) {
        mBus = bus->GetId();
    }
    mSavedBus.reset();
}

bool TrackOutput::WouldFeedBack(const Track& track, const BusTrack& bus)
{
    // Down from the bus, by outputs and sends, until the master, or a loop already there
    std::set<const Track*> visited;
    std::vector<const Track*> next { &bus };
    while (!next.empty()) {
        const Track* current = next.back();
        next.pop_back();
        if (current == &track) {
            return true;
        }
        if (!visited.insert(current).second) {
            continue;
        }
        if (const auto output = Get(*current).FindBus()) {
            next.push_back(output);
        }
        for (const auto& send : TrackSends::Get(*current).GetSends()) {
            if (const auto sent = findBus(*current, send->bus)) {
                next.push_back(sent);
            }
        }
    }
    return false;
}

void TrackOutput::CopyTo(Track& track) const
{
    auto& copy = Get(track);
    copy.mBus = mBus;
    copy.mSavedBus = mSavedBus;
}

void TrackOutput::Reparent(const std::shared_ptr<Track>& parent)
{
    mTrack = parent;
}

void TrackOutput::WriteXMLAttributes(XMLWriter& writer) const
{
    // By the bus's position in the tracks, which a project keeps
    const auto bus = FindBus();
    if (const auto index = bus ? indexOf(*bus) : std::nullopt) {
        writer.WriteAttr(outputAttr, *index);
    }
}

bool TrackOutput::HandleXMLAttribute(const std::string_view& attr, const XMLAttributeValueView& value)
{
    long long index = 0;
    if (attr == outputAttr && value.TryGet(index) && index >= 0) {
        mSavedBus = static_cast<size_t>(index);
        return true;
    }
    return false;
}

TrackSends& TrackSends::Get(Track& track)
{
    return track.AttachedTrackObjects::Get<TrackSends>(trackSendsKey);
}

const TrackSends& TrackSends::Get(const Track& track)
{
    return Get(const_cast<Track&>(track));
}

TrackSends::TrackSends(Track& track)
    : mTrack{track.shared_from_this()}
{
}

std::vector<std::shared_ptr<const TrackSends::Send> > TrackSends::FindSends() const
{
    std::vector<std::shared_ptr<const Send> > result;
    const auto track = mTrack.lock();
    for (const auto& send : mSends) {
        if (track && findBus(*track, send->bus)) {
            result.push_back(send);
        }
    }
    return result;
}

std::shared_ptr<TrackSends::Send> TrackSends::FindSend(TrackId bus) const
{
    const auto it = std::find_if(mSends.begin(), mSends.end(), [bus](const auto& send) { return send->bus == bus; });
    return it == mSends.end() ? nullptr : *it;
}

void TrackSends::AddSend(TrackId bus)
{
    if (!FindSend(bus)) {
        auto send = std::make_shared<Send>();
        send->bus = bus;
        mSends.push_back(std::move(send));
    }
}

void TrackSends::RemoveSend(TrackId bus)
{
    mSends.erase(std::remove_if(mSends.begin(), mSends.end(), [bus](const auto& send) { return send->bus == bus; }), mSends.end());
}

void TrackSends::ResolveSaved()
{
    const auto track = mTrack.lock();
    if (!track) {
        return;
    }
    for (const auto& saved : mSavedSends) {
        if (const auto bus = busAt(*track, saved.bus)) {
            AddSend(bus->GetId());
            const auto send = FindSend(bus->GetId());
            send->level = saved.level;
            send->preFader = saved.preFader;
        }
    }
    mSavedSends.clear();
}

void TrackSends::CopyTo(Track& track) const
{
    auto& copy = Get(track);
    copy.mSends.clear();
    for (const auto& send : mSends) {
        auto copied = std::make_shared<Send>();
        copied->bus = send->bus;
        copied->level = send->level.load();
        copied->preFader = send->preFader;
        copy.mSends.push_back(std::move(copied));
    }
    copy.mSavedSends = mSavedSends;
}

void TrackSends::Reparent(const std::shared_ptr<Track>& parent)
{
    mTrack = parent;
}

void TrackSends::WriteXMLAttributes(XMLWriter& writer) const
{
    // As "bus:level:pre" for each send, the bus by its position in the tracks
    const auto track = mTrack.lock();
    wxString sends;
    for (const auto& send : mSends) {
        const auto bus = track ? findBus(*track, send->bus) : nullptr;
        if (const auto index = bus ? indexOf(*bus) : std::nullopt) {
            sends += wxString::Format("%s%zu:%.9g:%d", sends.empty() ? "" : ";", *index, send->level.load(),
                                      send->preFader ? 1 : 0);
        }
    }
    if (!sends.empty()) {
        writer.WriteAttr(sendsAttr, sends);
    }
}

bool TrackSends::HandleXMLAttribute(const std::string_view& attr, const XMLAttributeValueView& value)
{
    if (attr != sendsAttr) {
        return false;
    }
    mSavedSends.clear();
    const auto text = value.ToString();
    size_t begin = 0;
    while (begin < text.size()) {
        auto end = text.find(';', begin);
        if (end == std::string::npos) {
            end = text.size();
        }
        SavedSend saved;
        int preFader = 0;
        if (std::sscanf(text.substr(begin, end - begin).c_str(), "%zu:%f:%d", &saved.bus, &saved.level, &preFader) == 3) {
            saved.preFader = preFader != 0;
            mSavedSends.push_back(saved);
        }
        begin = end + 1;
    }
    return true;
}
