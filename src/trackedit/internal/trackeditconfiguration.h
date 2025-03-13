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

    bool askBeforeConvertingToMonoOrStereo() const override;
    void setAskBeforeConvertingToMonoOrStereo(bool value) override;
    muse::async::Notification askBeforeConvertingToMonoOrStereoChanged() const override;

private:
    muse::async::Notification m_askBeforeConvertingToMonoOrStereoChanged;
};
}
