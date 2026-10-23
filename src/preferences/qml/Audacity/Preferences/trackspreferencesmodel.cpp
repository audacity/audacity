/*
 * Audacity: A Digital Audio Editor
 */
#include "trackspreferencesmodel.h"

#include "translation.h"

using namespace au::appshell;
using namespace au::projectscene;
using namespace au::trackedit;

TracksPreferencesModel::TracksPreferencesModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void TracksPreferencesModel::init()
{
    projectsceneConfiguration()->defaultTrackViewTypeChanged().onNotify(this, [this] {
        emit defaultTrackVisualizationChanged();
    });

    projectsceneConfiguration()->defaultRulerTypeChanged().onNotify(this, [this] {
        emit defaultRulerFormatChanged();
    });

    trackeditConfiguration()->defaultAudioTrackNameChanged().onNotify(this, [this] {
        emit defaultTrackNameChanged();
    });
}

int TracksPreferencesModel::defaultTrackVisualization() const
{
    return static_cast<int>(projectsceneConfiguration()->defaultTrackViewType());
}

void TracksPreferencesModel::setDefaultTrackVisualization(int mode)
{
    projectsceneConfiguration()->setDefaultTrackViewType(static_cast<TrackViewType>(mode));
}

int TracksPreferencesModel::defaultRulerFormat() const
{
    return static_cast<int>(projectsceneConfiguration()->defaultRulerType());
}

void TracksPreferencesModel::setDefaultRulerFormat(int scale)
{
    projectsceneConfiguration()->setDefaultRulerType(static_cast<TrackRulerType>(scale));
}

QString TracksPreferencesModel::defaultTrackName() const
{
    return QString::fromStdString(trackeditConfiguration()->defaultAudioTrackName());
}

void TracksPreferencesModel::setDefaultTrackName(const QString& name)
{
    trackeditConfiguration()->setDefaultAudioTrackName(name.toStdString());
    // An empty name falls back to the default one, so refresh the field either way
    emit defaultTrackNameChanged();
}

QVariantList TracksPreferencesModel::trackVisualizations() const
{
    return {
        QVariantMap{ { "title", muse::qtrc("preferences", "Waveform") }, { "value", static_cast<int>(TrackViewType::Waveform) } },
        QVariantMap{ { "title", muse::qtrc("preferences", "Spectrogram") }, { "value", static_cast<int>(TrackViewType::Spectrogram) } },
        QVariantMap{ { "title", muse::qtrc("preferences", "Multi-view") },
            { "value", static_cast<int>(TrackViewType::WaveformAndSpectrogram) } },
    };
}

QVariantList TracksPreferencesModel::rulerFormats() const
{
    return {
        QVariantMap{ { "title", muse::qtrc("preferences", "Linear (amp)") }, { "value", static_cast<int>(TrackRulerType::Linear) } },
        QVariantMap{ { "title", muse::qtrc("preferences", "Logarithmic (dB)") }, { "value", static_cast<int>(TrackRulerType::DbLog) } },
        QVariantMap{ { "title", muse::qtrc("preferences", "Linear (dB)") }, { "value", static_cast<int>(TrackRulerType::DbLinear) } },
    };
}
