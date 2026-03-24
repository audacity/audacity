/*
* Audacity: A Digital Audio Editor
*/

#include "framework/global/settings.h"

#include "playbackconfiguration.h"

using namespace muse;
using namespace au::playback;

static const std::string moduleName("playback");

static const muse::Settings::Key PLAYBACK_TIME_ITEM_FORMAT(moduleName, "playbackToolbar/playbackTimeItemFormat");
static const muse::Settings::Key PLAYBACK_METER_STYLE(moduleName, "playbackToolbar/playbackMeterStyle");
static const muse::Settings::Key PLAYBACK_METER_TYPE(moduleName, "playbackToolbar/playbackMeterType");
static const muse::Settings::Key PLAYBACK_METER_POSITION(moduleName, "playbackToolbar/playbackMeterPosition");
static const muse::Settings::Key PLAYBACK_METER_DB_RANGE(moduleName, "playbackToolbar/playbackMeterDbRange");
static const muse::Settings::Key PLAYBACK_HORIZONTAL_METER_SIZE(moduleName, "playbackToolbar/playbackHorizontalMeterSize");

static const muse::Settings::Key PLAYBACK_QUALITY("au3wrap", "/Quality/LibsoxrSampleRateConverterChoice");
static const muse::Settings::Key DITHERING("au3wrap", "Quality/DitherAlgorithmChoice");
static const muse::Settings::Key SOLO_BEHAVIOR(moduleName, "playback/soloBehavior");
static const muse::Settings::Key SEEK_SHORT_PERIOD(moduleName, "playback/seekShortPeriod");
static const muse::Settings::Key SEEK_LONG_PERIOD(moduleName, "playback/seekLongPeriod");
static const muse::Settings::Key SELECTION_FOLLOWS_LOOP_REGION(moduleName, "playback/selectionFollowsLoopRegion");

// quality/dithering settings are stored as string in audacity.cfg
// we need to convert these when reading/writing
// it doesn't need translations
static const std::vector<std::string> playbackNames{
    "LowQuality", "MediumQuality", "HighQuality", "BestQuality"
};

static const std::vector<std::string> ditherNames{
    "None", "Rectangle", "Triangle", "Shaped"
};

au::uicomponents::TimecodeFormatType PlaybackConfiguration::playbackTimeItemFormat() const
{
    return muse::settings()->value(PLAYBACK_TIME_ITEM_FORMAT)
           .toEnum<au::uicomponents::TimecodeFormatType>();
}

void PlaybackConfiguration::setPlaybackTimeItemFormat(au::uicomponents::TimecodeFormatType format)
{
    muse::settings()->setSharedValue(PLAYBACK_TIME_ITEM_FORMAT, muse::Val(static_cast<int>(format)));
}

async::Notification PlaybackConfiguration::playbackTimeItemFormatChanged() const
{
    return m_playbackTimeItemFormatChanged;
}

PlaybackMeterStyle::MeterStyle PlaybackConfiguration::playbackMeterStyle() const
{
    return muse::settings()->value(PLAYBACK_METER_STYLE)
           .toEnum<PlaybackMeterStyle::MeterStyle>();
}

void PlaybackConfiguration::setPlaybackMeterStyle(PlaybackMeterStyle::MeterStyle style)
{
    muse::settings()->setSharedValue(PLAYBACK_METER_STYLE, muse::Val(static_cast<int>(style)));
}

muse::async::Notification PlaybackConfiguration::playbackMeterStyleChanged() const
{
    return m_playbackMeterStyleChanged;
}

PlaybackMeterType::MeterType PlaybackConfiguration::playbackMeterType() const
{
    return muse::settings()->value(PLAYBACK_METER_TYPE)
           .toEnum<PlaybackMeterType::MeterType>();
}

void PlaybackConfiguration::setPlaybackMeterType(PlaybackMeterType::MeterType type)
{
    muse::settings()->setSharedValue(PLAYBACK_METER_TYPE, muse::Val(static_cast<int>(type)));
}

muse::async::Notification PlaybackConfiguration::playbackMeterTypeChanged() const
{
    return m_playbackMeterTypeChanged;
}

PlaybackMeterPosition::MeterPosition PlaybackConfiguration::playbackMeterPosition() const
{
    return muse::settings()->value(PLAYBACK_METER_POSITION)
           .toEnum<PlaybackMeterPosition::MeterPosition>();
}

void PlaybackConfiguration::setPlaybackMeterPosition(PlaybackMeterPosition::MeterPosition position)
{
    muse::settings()->setSharedValue(PLAYBACK_METER_POSITION, muse::Val(static_cast<int>(position)));
}

muse::async::Notification PlaybackConfiguration::playbackMeterPositionChanged() const
{
    return m_playbackMeterPositionChanged;
}

PlaybackMeterDbRange::DbRange PlaybackConfiguration::playbackMeterDbRange() const
{
    return muse::settings()->value(PLAYBACK_METER_DB_RANGE)
           .toEnum<PlaybackMeterDbRange::DbRange>();
}

void PlaybackConfiguration::setPlaybackMeterDbRange(PlaybackMeterDbRange::DbRange range)
{
    muse::settings()->setSharedValue(PLAYBACK_METER_DB_RANGE, muse::Val(static_cast<int>(range)));
}

muse::async::Notification PlaybackConfiguration::playbackMeterDbRangeChanged() const
{
    return m_playbackMeterDbRangeChanged;
}

int PlaybackConfiguration::playbackHorizontalMeterSize() const
{
    return muse::settings()->value(PLAYBACK_HORIZONTAL_METER_SIZE).toInt();
}

void PlaybackConfiguration::setPlaybackHorizontalMeterSize(int size)
{
    muse::settings()->setSharedValue(PLAYBACK_HORIZONTAL_METER_SIZE, muse::Val(size));
}

muse::async::Notification PlaybackConfiguration::playbackHorizontalMeterSizeChanged() const
{
    return m_playbackHorizontalMeterSizeChanged;
}

void PlaybackConfiguration::init()
{
    muse::settings()->setDefaultValue(PLAYBACK_TIME_ITEM_FORMAT,
                                      muse::Val(static_cast<int>(au::uicomponents::TimecodeFormatType::HHMMSSHundredths)));
    muse::settings()->valueChanged(PLAYBACK_TIME_ITEM_FORMAT).onReceive(nullptr, [this](const muse::Val&) {
        m_playbackTimeItemFormatChanged.notify();
    });

    muse::settings()->setDefaultValue(PLAYBACK_METER_STYLE,
                                      muse::Val(static_cast<int>(PlaybackMeterStyle::MeterStyle::Default)));
    muse::settings()->valueChanged(PLAYBACK_METER_STYLE).onReceive(nullptr, [this](const muse::Val&) {
        m_playbackMeterStyleChanged.notify();
    });

    muse::settings()->setDefaultValue(PLAYBACK_METER_TYPE,
                                      muse::Val(static_cast<int>(PlaybackMeterType::MeterType::DbLog)));
    muse::settings()->valueChanged(PLAYBACK_METER_TYPE).onReceive(nullptr, [this](const muse::Val&) {
        m_playbackMeterTypeChanged.notify();
    });

    muse::settings()->setDefaultValue(PLAYBACK_METER_POSITION,
                                      muse::Val(static_cast<int>(PlaybackMeterPosition::MeterPosition::TopBar)));
    muse::settings()->valueChanged(PLAYBACK_METER_POSITION).onReceive(nullptr, [this](const muse::Val&) {
        m_playbackMeterPositionChanged.notify();
    });

    muse::settings()->setDefaultValue(PLAYBACK_METER_DB_RANGE,
                                      muse::Val(static_cast<int>(PlaybackMeterDbRange::DbRange::Range60)));
    muse::settings()->valueChanged(PLAYBACK_METER_DB_RANGE).onReceive(nullptr, [this](const muse::Val&) {
        m_playbackMeterDbRangeChanged.notify();
    });

    muse::settings()->setDefaultValue(PLAYBACK_HORIZONTAL_METER_SIZE, muse::Val(0));
    muse::settings()->valueChanged(PLAYBACK_HORIZONTAL_METER_SIZE).onReceive(nullptr, [this](const muse::Val&) {
        m_playbackHorizontalMeterSizeChanged.notify();
    });

    muse::settings()->setDefaultValue(PLAYBACK_QUALITY, muse::Val(playbackNames.at(3)));
    muse::settings()->valueChanged(PLAYBACK_QUALITY).onReceive(nullptr, [this](const muse::Val&) {
        m_playbackQualityChanged.notify();
    });

    muse::settings()->setDefaultValue(DITHERING, muse::Val(ditherNames.at(0)));
    muse::settings()->valueChanged(DITHERING).onReceive(nullptr, [this](const muse::Val&) {
        m_ditheringChanged.notify();
    });

    muse::settings()->setDefaultValue(SOLO_BEHAVIOR, muse::Val(TracksBehaviors::SoloBehavior::SoloBehaviorMulti));
    muse::settings()->valueChanged(SOLO_BEHAVIOR).onReceive(nullptr, [this](const muse::Val&) {
        m_soloBehaviorChanged.notify();
    });

    muse::settings()->setDefaultValue(SEEK_SHORT_PERIOD, muse::Val(5.0));
    muse::settings()->valueChanged(SEEK_SHORT_PERIOD).onReceive(nullptr, [this](const muse::Val&) {
        m_shortSkipChanged.notify();
    });

    muse::settings()->setDefaultValue(SEEK_LONG_PERIOD, muse::Val(15.0));
    muse::settings()->valueChanged(SEEK_LONG_PERIOD).onReceive(nullptr, [this](const muse::Val&) {
        m_longSkipChanged.notify();
    });

    muse::settings()->setDefaultValue(SELECTION_FOLLOWS_LOOP_REGION, muse::Val(false));
    muse::settings()->valueChanged(SELECTION_FOLLOWS_LOOP_REGION).onReceive(nullptr, [this](const muse::Val&) {
        m_selectionFollowsLoopRegionChanged.notify();
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

bool PlaybackConfiguration::selectionFollowsLoopRegion() const
{
    return muse::settings()->value(SELECTION_FOLLOWS_LOOP_REGION).toBool();
}

void PlaybackConfiguration::setSelectionFollowsLoopRegion(bool follows)
{
    muse::settings()->setSharedValue(SELECTION_FOLLOWS_LOOP_REGION, muse::Val(follows));
}

async::Notification PlaybackConfiguration::selectionFollowsLoopRegionChanged() const
{
    return m_selectionFollowsLoopRegionChanged;
}
