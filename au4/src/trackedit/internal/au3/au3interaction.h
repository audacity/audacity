/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../../itrackeditinteraction.h"

#include "Track.h"
#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

class AudacityProject;

struct TrackData
{
    // Track type from Track.h
    std::shared_ptr<Track> track;
    au::trackedit::ClipKey clipKey;
};

struct Clipboard
{
    std::vector<TrackData> data;
};

namespace au::trackedit {
class Au3Interaction : public ITrackeditInteraction
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    Au3Interaction() = default;

    audio::secs_t clipStartTime(const trackedit::ClipKey& clipKey) const override;

    bool changeClipStartTime(const trackedit::ClipKey& clipKey, double newStartTime, bool completed) override;
    muse::async::Channel<trackedit::ClipKey, double /*newStartTime*/, bool /*completed*/> clipStartTimeChanged() const override;

    bool changeClipTitle(const trackedit::ClipKey& clipKey, const muse::String& newTitle) override;
    void clearClipboard() override;
    bool pasteIntoClipboard(double begin, trackedit::TrackId trackId) override;
    bool copyClipIntoClipboard(const trackedit::ClipKey& clipKey) override;
    bool copyClipDataIntoClipboard(const trackedit::ClipKey& clipKey, double begin, double end) override;
    bool copyTrackDataIntoClipboard(const trackedit::TrackId trackId, double begin, double end) override;
    bool removeClip(const trackedit::ClipKey& clipKey) override;
    bool removeClipData(const trackedit::ClipKey& clipKey, double begin, double end) override;
    audio::secs_t clipDuration(const trackedit::ClipKey& clipKey) const override;

private:
    AudacityProject& projectRef() const;

    muse::async::Channel<trackedit::ClipKey, double /*newStartTime*/, bool /*completed*/> m_clipStartTimeChanged;

    static Clipboard s_clipboard;
};
}
