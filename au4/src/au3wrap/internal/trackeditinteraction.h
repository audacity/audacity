#pragma once

#include "trackedit/itrackeditinteraction.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

class AudacityProject;
namespace au::au3 {
class TrackeditInteraction : public trackedit::ITrackeditInteraction
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    TrackeditInteraction() = default;

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
