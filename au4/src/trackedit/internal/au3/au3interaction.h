/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../../itrackeditinteraction.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

class AudacityProject;
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
    bool removeClip(const trackedit::ClipKey& clipKey) override;
    bool removeClipData(const trackedit::ClipKey& clipKey, double begin, double end) override;
    audio::secs_t clipDuration(const trackedit::ClipKey& clipKey) const override;

private:
    AudacityProject& projectRef() const;

    muse::async::Channel<trackedit::ClipKey, double /*newStartTime*/, bool /*completed*/> m_clipStartTimeChanged;
};
}
