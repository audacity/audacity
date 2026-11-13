/*
* Audacity: A Digital Audio Editor
*/
#include "audioenginetoolbaritem.h"

#include "framework/global/translation.h"

using namespace au::projectscene;
using namespace muse::uicomponents;

namespace {
QString failureMessage(const au::audio::ApplyResult& result)
{
    QString message;
    switch (result.status) {
    case au::audio::ApplyStatus::Busy:
        message = muse::qtrc("projectscene", "Audio settings are already being changed.");
        break;
    case au::audio::ApplyStatus::OwnerUnavailable:
        message = muse::qtrc("projectscene", "The active audio stream could not be stopped.");
        break;
    case au::audio::ApplyStatus::Applied:
    case au::audio::ApplyStatus::NoChange:
        break;
    default:
        message = muse::qtrc("projectscene", "The audio engine could not be activated.");
        break;
    }
    if (result.streamRestorationFailed) {
        if (!message.isEmpty()) {
            message += " ";
        }
        message += muse::qtrc("projectscene", "The audio stream could not be restored after changing the audio engine.");
    }
    return message;
}
}

AudioEngineToolBarItem::AudioEngineToolBarItem(ToolBarItemType::Type type, QObject* parent)
    : ToolBarItem(type, parent)
{
    updateEngines();
    audioBackendSelector()->activeBackendChanged().onNotify(this, [this] { updateEngines(); });
    audioEngine()->deviceLatencyChanged().onNotify(this, [this] { emit latencyChanged(); });
}

void AudioEngineToolBarItem::selectEngine(const QString& id)
{
    const auto backend = id.toStdString();
    if (backend == audioBackendSelector()->activeBackend()) {
        return;
    }
    const auto result = audioBackendSelector()->setActiveBackend(backend);
    if (!result.succeeded() || result.streamRestorationFailed) {
        if (interactive()) {
            interactive()->error(muse::qtrc("projectscene", "Unable to change the audio engine").toStdString(),
                                 failureMessage(result).toStdString());
        }
        // The selection shows the engine still active
        updateEngines();
    }
}

QVariantList AudioEngineToolBarItem::engines() const
{
    return menuItemListToVariantList(m_engines);
}

QString AudioEngineToolBarItem::currentEngine() const
{
    return QString::fromStdString(audioBackendSelector()->backendTitle(audioBackendSelector()->activeBackend()));
}

QString AudioEngineToolBarItem::latency() const
{
    const auto latency = audioEngine()->deviceLatency();
    if (!latency) {
        //: Shown next to the audio engine when no audio device was opened yet
        return muse::qtrc("projectscene", "Latency: no device opened yet");
    }
    const auto ms = [](const std::optional<double>& seconds) {
        //: A latency of the audio device, in milliseconds
        return seconds ? muse::qtrc("projectscene", "%1 ms").arg(QString::number(*seconds * 1000.0, 'f', 1)) : QString("–");
    };
    const auto roundTrip = latency->input && latency->output ? std::optional { *latency->input + *latency->output } : std::nullopt;
    //: Latencies of the audio device: input, output and round trip; "–" where the device has no input or output open
    return muse::qtrc("projectscene", "In %1 · Out %2 · Round trip %3").arg(ms(latency->input), ms(latency->output), ms(roundTrip));
}

void AudioEngineToolBarItem::updateEngines()
{
    qDeleteAll(m_engines);
    m_engines.clear();
    const auto active = audioBackendSelector()->activeBackend();
    for (const auto& id : audioBackendSelector()->backends()) {
        auto* engine = new MenuItem(this);
        engine->setId(QString::fromStdString(id));
        engine->setTitle(muse::TranslatableString::untranslatable(QString::fromStdString(audioBackendSelector()->backendTitle(id))));
        engine->setEnabled(true);
        engine->setSelectable(true);
        engine->setSelected(id == active);
        m_engines << engine;
    }
    emit enginesChanged();
}
