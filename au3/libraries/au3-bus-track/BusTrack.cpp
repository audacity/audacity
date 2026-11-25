/*
 * Audacity: A Digital Audio Editor
 */
#include "BusTrack.h"

#include <algorithm>
#include <set>

#include "au3-project/Project.h"
#include "au3-realtime-effects/RealtimeEffectList.h"
#include "au3-strings/TranslatableString.h"
#include "au3-xml/XMLWriter.h"

namespace {
const std::string xmlTag = "bustrack";
constexpr auto outputAttr = "output";

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
    const auto owner = track ? track->GetOwner() : nullptr;
    if (!mBus || !owner) {
        return nullptr;
    }
    return dynamic_cast<const BusTrack*>(owner->FindById(*mBus));
}

void TrackOutput::ResolveSaved()
{
    const auto track = mTrack.lock();
    const auto owner = track ? track->GetOwner() : nullptr;
    if (!mSavedBus || !owner) {
        return;
    }
    size_t index = 0;
    for (const auto other : *owner) {
        if (index++ == *mSavedBus) {
            if (dynamic_cast<const BusTrack*>(other) && other != track.get()) {
                mBus = other->GetId();
            }
            break;
        }
    }
    mSavedBus.reset();
}

bool TrackOutput::WouldFeedBack(const Track& track, const BusTrack& bus)
{
    // Down from the bus, until the master, or a loop already there
    std::set<const Track*> visited;
    for (const Track* next = &bus; next && visited.insert(next).second; next = Get(*next).FindBus()) {
        if (next == &track) {
            return true;
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
    if (!bus) {
        return;
    }
    size_t index = 0;
    for (const auto other : *bus->GetOwner()) {
        if (other == bus) {
            writer.WriteAttr(outputAttr, index);
            return;
        }
        ++index;
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
