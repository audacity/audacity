/*
* Audacity: A Digital Audio Editor
*/
#include "waveviewnotifier.h"

#include <map>

using namespace au::projectscene;

WaveViewNotifier* WaveViewNotifier::forContext(const muse::modularity::ContextPtr& ctx)
{
    //! NOTE A context lives as long as its window; its notifier is kept for the lifetime of the application
    static std::map<muse::modularity::IoCID, WaveViewNotifier*> notifiers;

    const muse::modularity::IoCID id = ctx ? ctx->id : static_cast<muse::modularity::IoCID>(-1);
    auto it = notifiers.find(id);
    if (it == notifiers.end()) {
        it = notifiers.emplace(id, new WaveViewNotifier(ctx)).first;
    }
    return it->second;
}

WaveViewNotifier::WaveViewNotifier(const muse::modularity::ContextPtr& ctx)
    : muse::Contextable(ctx)
{
    configuration()->isRMSInWaveformVisibleChanged().onReceive(this, [this](bool) {
        emit waveformSettingsChanged();
    });

    configuration()->isClippingInWaveformVisibleChanged().onReceive(this, [this](bool) {
        emit waveformSettingsChanged();
    });

    configuration()->clipStyleChanged().onReceive(this, [this](ClipStyles::Style) {
        emit clipStyleChanged();
    });

    globalContext()->isRecordingChanged().onNotify(this, [this]() {
        emit isRecordingChanged();
    });
}
