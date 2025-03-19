/*
* Audacity: A Digital Audio Editor
*/

#include "settings.h"

#include "playbackconfiguration.h"

static const QString PLAYBACK_TIME_ITEM_FORMAT("playbackToolbar/playbackTimeItemFormat");

using namespace muse;
using namespace au::playback;

static const std::string moduleName("playback");

static const muse::Settings::Key PLAYBACK_QUALITY("au3wrap", "/Quality/LibsoxrSampleRateConverterChoice");
static const muse::Settings::Key DITHERING("au3wrap", "Quality/DitherAlgorithmChoice");
static const muse::Settings::Key SOLO_BEHAVIOR(moduleName, "playback/soloBehavior");
static const muse::Settings::Key SEEK_SHORT_PERIOD(moduleName, "playback/seekShortPeriod");
static const muse::Settings::Key SEEK_LONG_PERIOD(moduleName, "playback/seekLongPeriod");

// quality/dithering settings are stored as string in audacity.cfg
// we need to convert these when reading/writing
// it doesn't need translations
static const std::vector<std::string> playbackNames{
    "LowQuality", "MediumQuality", "HighQuality", "BestQuality"
};

static const std::vector<std::string> ditherNames{
    "None", "Rectangle", "Triangle", "Shaped"
};

draw::Color PlaybackConfiguration::playColor() const
{
    return "#18A999";
}

TimecodeFormatType PlaybackConfiguration::playbackTimeItemFormat() const
{
    TimecodeFormatType result = TimecodeFormatType::HHMMSSHundredths;

    QString formatStr = uiConfiguration()->uiItemState(PLAYBACK_TIME_ITEM_FORMAT);
    if (!formatStr.isEmpty()) {
        result = static_cast<TimecodeFormatType>(formatStr.toInt());
    }

    return result;
}

void PlaybackConfiguration::setPlaybackTimeItemFormat(TimecodeFormatType format)
{
    uiConfiguration()->setUiItemState(PLAYBACK_TIME_ITEM_FORMAT, QString::number(static_cast<int>(format)));
}

async::Notification PlaybackConfiguration::playbackTimeItemFormatChanged() const
{
    return uiConfiguration()->uiItemStateChanged(PLAYBACK_TIME_ITEM_FORMAT);
}

void PlaybackConfiguration::init()
{
    muse::settings()->setDefaultValue(PLAYBACK_QUALITY, muse::Val(playbackNames.at(3)));
    muse::settings()->valueChanged(PLAYBACK_QUALITY).onReceive(nullptr, [this](const muse::Val& val) {
        m_playbackQualityChanged.notify();
    });

    muse::settings()->setDefaultValue(DITHERING, muse::Val(ditherNames.at(0)));
    muse::settings()->valueChanged(DITHERING).onReceive(nullptr, [this](const muse::Val& val) {
        m_ditheringChanged.notify();
    });

    muse::settings()->setDefaultValue(SOLO_BEHAVIOR, muse::Val(TracksBehaviors::SoloBehavior::SoloBehaviorMulti));
    muse::settings()->valueChanged(SOLO_BEHAVIOR).onReceive(nullptr, [this](const muse::Val& val) {
        m_soloBehaviorChanged.notify();
    });

    muse::settings()->setDefaultValue(SEEK_SHORT_PERIOD, muse::Val(5.0));
    muse::settings()->valueChanged(SEEK_SHORT_PERIOD).onReceive(nullptr, [this](const muse::Val& val) {
        m_shortSkipChanged.notify();
    });

    muse::settings()->setDefaultValue(SEEK_LONG_PERIOD, muse::Val(15.0));
    muse::settings()->valueChanged(SEEK_LONG_PERIOD).onReceive(nullptr, [this](const muse::Val& val) {
        m_longSkipChanged.notify();
    });
}

std::vector<au::playback::PlaybackQualityPrefs::PlaybackQuality> PlaybackConfiguration::playbackQualityList() const
{
    return { PlaybackQualityPrefs::PlaybackQuality::LowQuality, PlaybackQualityPrefs::PlaybackQuality::MediumQuality,
             PlaybackQualityPrefs::PlaybackQuality::HighQuality, PlaybackQualityPrefs::PlaybackQuality::BestQuality };
}

au::playback::PlaybackQualityPrefs::PlaybackQuality PlaybackConfiguration::currentPlaybackQuality() const
{
    // best quality is default one
    int idx = 3;
    std::string rawSetting = muse::settings()->value(PLAYBACK_QUALITY).toString();
    auto it = std::find(playbackNames.begin(), playbackNames.end(), rawSetting);
    if (it != playbackNames.end()) {
        idx = static_cast<int>(std::distance(playbackNames.begin(), it));
    }
    return static_cast<PlaybackQualityPrefs::PlaybackQuality>(idx);
}

void PlaybackConfiguration::setPlaybackQuality(PlaybackQualityPrefs::PlaybackQuality quality)
{
    muse::settings()->setSharedValue(PLAYBACK_QUALITY, muse::Val(playbackNames.at(static_cast<int>(quality))));
}

std::vector<DitherTypePrefs::DitherType> PlaybackConfiguration::ditheringList() const
{
    return { DitherTypePrefs::DitherType::None, DitherTypePrefs::DitherType::Rectangle, DitherTypePrefs::DitherType::Triangle,
             DitherTypePrefs::DitherType::Shaped };
}

DitherTypePrefs::DitherType PlaybackConfiguration::currentDithering() const
{
    // dithering none is default one
    int idx = 0;
    std::string rawSetting = muse::settings()->value(DITHERING).toString();
    auto it = std::find(ditherNames.begin(), ditherNames.end(), rawSetting);
    if (it != ditherNames.end()) {
        idx = static_cast<int>(std::distance(ditherNames.begin(), it));
    }
    return static_cast<DitherTypePrefs::DitherType>(idx);
}

void PlaybackConfiguration::setDithering(DitherTypePrefs::DitherType dithering)
{
    muse::settings()->setSharedValue(DITHERING, muse::Val(ditherNames.at(static_cast<int>(dithering))));
}

async::Notification PlaybackConfiguration::ditheringChanged() const
{
    return m_ditheringChanged;
}

async::Notification PlaybackConfiguration::playbackQualityChanged() const
{
    return m_playbackQualityChanged;
}

TracksBehaviors::SoloBehavior PlaybackConfiguration::currentSoloBehavior() const
{
    return muse::settings()->value(SOLO_BEHAVIOR).toEnum<playback::TracksBehaviors::SoloBehavior>();
}

void PlaybackConfiguration::setSoloBehavior(playback::TracksBehaviors::SoloBehavior behavior)
{
    muse::settings()->setSharedValue(SOLO_BEHAVIOR, muse::Val(behavior));
}

muse::async::Notification PlaybackConfiguration::soloBehaviorChanged() const
{
    return m_soloBehaviorChanged;
}

au::trackedit::secs_t PlaybackConfiguration::shortSkip() const
{
    return muse::settings()->value(SEEK_SHORT_PERIOD).toDouble();
}

void PlaybackConfiguration::setShortSkip(trackedit::secs_t seconds)
{
    muse::settings()->setSharedValue(SEEK_SHORT_PERIOD, muse::Val(seconds));
}

async::Notification PlaybackConfiguration::shortSkipChanged() const
{
    return m_shortSkipChanged;
}

au::trackedit::secs_t PlaybackConfiguration::longSkip() const
{
    return muse::settings()->value(SEEK_LONG_PERIOD).toDouble();
}

void PlaybackConfiguration::setLongSkip(trackedit::secs_t seconds)
{
    muse::settings()->setSharedValue(SEEK_LONG_PERIOD, muse::Val(seconds));
}

async::Notification PlaybackConfiguration::longSkipChanged() const
{
    return m_longSkipChanged;
}
