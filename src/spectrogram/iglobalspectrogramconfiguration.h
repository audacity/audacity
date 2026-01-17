/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ispectrogramconfiguration.h"

#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/async/channel.h"
#include "framework/global/async/notification.h"

namespace au::spectrogram {
class IGlobalSpectrogramConfiguration : MODULE_GLOBAL_EXPORT_INTERFACE, public ISpectrogramConfiguration
{
    INTERFACE_ID(IGlobalSpectrogramConfiguration)

public:
    virtual ~IGlobalSpectrogramConfiguration() = default;

    virtual bool spectralSelectionEnabled() const = 0;
    virtual void setSpectralSelectionEnabled(bool value) = 0;
    virtual muse::async::Channel<bool> spectralSelectionEnabledChanged() const = 0;

    virtual muse::async::Channel<int> minFreqChanged() const = 0;
    virtual muse::async::Channel<int> maxFreqChanged() const = 0;
    virtual muse::async::Channel<SpectrogramColorScheme> colorSchemeChanged() const = 0;
    virtual muse::async::Channel<int> colorGainDbChanged() const = 0;
    virtual muse::async::Channel<int> colorRangeDbChanged() const = 0;
    virtual muse::async::Channel<int> colorHighBoostDbPerDecChanged() const = 0;
    virtual muse::async::Channel<SpectrogramScale> scaleChanged() const = 0;
    virtual muse::async::Channel<SpectrogramAlgorithm> algorithmChanged() const = 0;
    virtual muse::async::Channel<SpectrogramWindowType> windowTypeChanged() const = 0;
    virtual muse::async::Channel<int> winSizeLog2Changed() const = 0;
    virtual muse::async::Channel<int> zeroPaddingFactorChanged() const = 0;
    virtual muse::async::Notification someSettingChanged() const = 0;
};
} // namespace au::spectrogram
