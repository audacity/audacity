/*
* Audacity: A Digital Audio Editor
*/

#include "settings.h"

#include "Prefs.h"
#include "TranslatableString.h"
#include "Resample.h"
#include "Dither.h"

#include "playbackconfiguration.h"

using namespace muse;
using namespace au::playback;

static const std::string moduleName("playback");

static const muse::Settings::Key SOLO_BEHAVIOR(moduleName, "playback/soloBehavior");

draw::Color PlaybackConfiguration::playColor() const
{
    return "#18A999";
}

void PlaybackConfiguration::init()
{
    muse::settings()->setDefaultValue(SOLO_BEHAVIOR, muse::Val(TracksBehaviors::SoloBehavior::SoloBehaviorMulti));
    muse::settings()->valueChanged(SOLO_BEHAVIOR).onReceive(nullptr, [this](const muse::Val& val) {
        m_soloBehaviorChanged.notify();
    });
}

std::vector<std::string> PlaybackConfiguration::playbackQualityList() const
{
    std::vector<std::string> playbackQualityList;
    for (const auto& symbol : Resample::FastMethodSetting.GetSymbols()) {
        playbackQualityList.push_back(symbol.Translation().ToStdString());
    }

    return playbackQualityList;
}

std::string PlaybackConfiguration::currentPlaybackQuality() const
{
    auto quality = Resample::FastMethodSetting.Read();
    for (const auto& symbol : Resample::FastMethodSetting.GetSymbols()) {
        if (quality == symbol.Internal()) {
            return symbol.Msgid().Translation().ToStdString();
        }
    }
}

void PlaybackConfiguration::setPlaybackQuality(const std::string& quality)
{
    for (const auto& symbol : Resample::FastMethodSetting.GetSymbols()) {
        if (quality == symbol.Msgid().Translation()) {
            Resample::FastMethodSetting.Write(wxString(symbol.Internal()));
            m_playbackQualityChanged.notify();
        }
    }
}

std::vector<std::string> PlaybackConfiguration::ditheringList() const
{
    std::vector<std::string> ditheringList;
    for (const auto& symbol : Dither::FastSetting.GetSymbols()) {
        ditheringList.push_back(symbol.Translation().ToStdString());
    }

    return ditheringList;
}

std::string PlaybackConfiguration::currentDithering() const
{
    auto dithering = Dither::FastSetting.Read();
    for (const auto& symbol : Dither::FastSetting.GetSymbols()) {
        if (dithering == symbol.Internal()) {
            return symbol.Msgid().Translation().ToStdString();
        }
    }
}

void PlaybackConfiguration::setDithering(const std::string& dithering)
{
    for (const auto& symbol : Dither::FastSetting.GetSymbols()) {
        if (dithering == symbol.Msgid().Translation()) {
            Dither::FastSetting.Write(wxString(symbol.Internal()));
            m_ditheringChanged.notify();
        }
    }
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
    double shortSkip;
    gPrefs->Read(wxT("/AudioIO/SeekShortPeriod"), &shortSkip, 5.0);

    return shortSkip;
}

void PlaybackConfiguration::setShortSkip(trackedit::secs_t seconds)
{
    gPrefs->Write(wxT("/AudioIO/SeekShortPeriod"), static_cast<double>(seconds));
    m_shortSkipChanged.notify();
}

async::Notification PlaybackConfiguration::shortSkipChanged() const
{
    return m_shortSkipChanged;
}

au::trackedit::secs_t PlaybackConfiguration::longSkip() const
{
    double longSkip;
    gPrefs->Read(wxT("/AudioIO/SeekLongPeriod"), &longSkip, 15.0);

    return longSkip;
}

void PlaybackConfiguration::setLongSkip(trackedit::secs_t seconds)
{
    gPrefs->Write(wxT("/AudioIO/SeekLongPeriod"), static_cast<double>(seconds));
    m_longSkipChanged.notify();
}

async::Notification PlaybackConfiguration::longSkipChanged() const
{
    return m_longSkipChanged;
}
