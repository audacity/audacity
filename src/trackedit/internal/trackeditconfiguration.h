/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../itrackeditconfiguration.h"

namespace au::trackedit {
class TrackeditConfiguration : public ITrackeditConfiguration
{
public:
    TrackeditConfiguration() = default;

    void init();

    bool pasteAsNewClip() const override;
    void setPasteAsNewClip(bool value) override;
    muse::async::Notification pasteAsNewClipChanged() const override;

    bool askBeforeConvertingToMonoOrStereo() const override;
    void setAskBeforeConvertingToMonoOrStereo(bool value) override;
    muse::async::Notification askBeforeConvertingToMonoOrStereoChanged() const override;

    DeleteBehavior deleteBehavior() const override;
    void setDeleteBehavior(DeleteBehavior value) override;
    muse::async::Notification deleteBehaviorChanged() const override;

    CloseGapBehavior closeGapBehavior() const override;
    void setCloseGapBehavior(CloseGapBehavior value) override;
    muse::async::Notification closeGapBehaviorChanged() const override;

    PasteBehavior pasteBehavior() const override;
    void setPasteBehavior(PasteBehavior value) override;
    muse::async::Notification pasteBehaviorChanged() const override;

    PasteInsertBehavior pasteInsertBehavior() const override;
    void setPasteInsertBehavior(PasteInsertBehavior value) override;
    muse::async::Notification pasteInsertBehaviorChanged() const override;

private:
    muse::async::Notification m_pasteAsNewClipChanged;
    muse::async::Notification m_askBeforeConvertingToMonoOrStereoChanged;
    muse::async::Notification m_deleteBehaviorChanged;
    muse::async::Notification m_pasteBehaviorChanged;
    muse::async::Notification m_pasteInsertBehaviorChanged;
    muse::async::Notification m_closeGapBehaviorChanged;
};
}
