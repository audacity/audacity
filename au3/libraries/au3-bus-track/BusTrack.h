/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <atomic>
#include <memory>
#include <optional>
#include <vector>

#include "au3-playable-track/PlayableTrack.h"
#include "au3-track/TrackAttachment.h"

class AudacityProject;

//! A track without content that mixes the tracks that output to it, instead of the master: with mute/solo, effects, volume
//! and pan of its own. It outputs to the master, or to another bus.
class BusTrack final : public UniqueChannelTrack<PlayableTrack>
{
public:
    static wxString GetDefaultName();

    //! A new bus, at the end of the project's tracks
    static BusTrack* New(AudacityProject& project);

    BusTrack();
    BusTrack(const BusTrack& orig, ProtectedCreationArg&& a);
    ~BusTrack() override = default;

    const TypeInfo& GetTypeInfo() const override;
    static const TypeInfo& ClassTypeInfo();

    float GetVolume() const { return mVolume.load(std::memory_order_relaxed); }
    void SetVolume(float volume);
    float GetPan() const { return mPan.load(std::memory_order_relaxed); }
    void SetPan(float pan);
    //! Of its volume and pan, as WaveTrack::GetChannelVolume
    float GetChannelVolume(int channel) const;

    // It has no content to edit
    bool SupportsBasicEditing() const override { return false; }
    Track::Holder PasteInto(AudacityProject& project, TrackList& list) const override;
    Track::Holder Cut(double t0, double t1, bool moveClips) override;
    Track::Holder Copy(double t0, double t1, bool forClipboard) const override;
    void Clear(double, double, bool) override {}
    void Paste(double, const Track&, bool) override {}
    void Silence(double, double, ProgressReporter) override {}
    void InsertSilence(double, double) override {}
    void MoveTo(double) override {}
    void ShiftBy(double, double) override {}
    size_t NIntervals() const override { return 0; }

    bool HandleXMLTag(const std::string_view& tag, const AttributesList& attrs) override;
    XMLTagHandler* HandleXMLChild(const std::string_view& tag) override;
    void WriteXML(XMLWriter& xmlFile) const override;

private:
    std::shared_ptr<WideChannelGroupInterval> DoGetInterval(size_t) override { return {}; }
    Track::Holder Clone(bool backup) const override;
    Track::Holder TrackEmptyCopy() const override;
    void Init(const BusTrack& orig);

    //! Atomic because they are read by the audio thread in playback
    std::atomic<float> mVolume { 1.f };
    std::atomic<float> mPan { 0.f };
};

ENUMERATE_TRACK_TYPE(BusTrack);

//! Where a track's output goes: to a bus, or to the master
/*!
 It references the bus by TrackId, which a project keeps only in session: it saves it by the bus's position in the project's
 tracks, which ResolveSaved() turns back into an id once all the tracks are loaded.
 */
class TrackOutput final : public TrackAttachment
{
public:
    static TrackOutput& Get(Track& track);
    static const TrackOutput& Get(const Track& track);

    explicit TrackOutput(Track& track);
    ~TrackOutput() override = default;

    //! The bus it goes to; none for the master
    std::optional<TrackId> GetBus() const { return mBus; }
    void SetBus(std::optional<TrackId> bus);
    //! The bus it goes to, if its tracks still have it; null for the master
    const BusTrack* FindBus() const;

    //! Once all the tracks of its project are loaded: the bus it saved
    void ResolveSaved();

    //! Whether `track` going to `bus`, as its output or by a send, would feed `bus` back into itself
    static bool WouldFeedBack(const Track& track, const BusTrack& bus);

    // TrackAttachment
    void CopyTo(Track& track) const override;
    void Reparent(const std::shared_ptr<Track>& parent) override;
    void WriteXMLAttributes(XMLWriter& writer) const override;
    bool HandleXMLAttribute(const std::string_view& attr, const XMLAttributeValueView& value) override;

private:
    std::weak_ptr<Track> mTrack;
    std::optional<TrackId> mBus;
    //! Read from a project, until resolved
    std::optional<size_t> mSavedBus;
};

//! What a track sends to busses besides its output, as a DAW's sends: at most one send to each bus
/*!
 As TrackOutput, it saves the busses by their positions in the project's tracks, resolved by ResolveSaved().
 */
class TrackSends final : public TrackAttachment
{
public:
    struct Send {
        TrackId bus;
        //! Read by the audio thread in playback
        std::atomic<float> level { 1.f };
        //! Before the track's volume and pan, rather than after
        bool preFader = false;
    };

    static TrackSends& Get(Track& track);
    static const TrackSends& Get(const Track& track);

    explicit TrackSends(Track& track);
    ~TrackSends() override = default;

    //! Shared, so that playback keeps reading their levels while they change
    const std::vector<std::shared_ptr<Send> >& GetSends() const { return mSends; }
    //! Those to busses its tracks still have
    std::vector<std::shared_ptr<const Send> > FindSends() const;
    std::shared_ptr<Send> FindSend(TrackId bus) const;
    //! If it doesn't send to `bus` yet
    void AddSend(TrackId bus);
    void RemoveSend(TrackId bus);

    //! Once all the tracks of its project are loaded: the busses it saved
    void ResolveSaved();

    // TrackAttachment
    void CopyTo(Track& track) const override;
    void Reparent(const std::shared_ptr<Track>& parent) override;
    void WriteXMLAttributes(XMLWriter& writer) const override;
    bool HandleXMLAttribute(const std::string_view& attr, const XMLAttributeValueView& value) override;

private:
    struct SavedSend {
        size_t bus = 0;
        float level = 1.f;
        bool preFader = false;
    };

    std::weak_ptr<Track> mTrack;
    std::vector<std::shared_ptr<Send> > mSends;
    //! Read from a project, until resolved
    std::vector<SavedSend> mSavedSends;
};
