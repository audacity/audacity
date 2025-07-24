/*
* Audacity: A Digital Audio Editor
*/

#include "playbackmeterdbrangemodel.h"

using namespace au::playback;

namespace {
constexpr const char* DB36_DESCRIPTION = "-36 dB (shallow range for high-amplitude editing)";
constexpr const char* DB48_DESCRIPTION = "-48 dB (PCM range of 8 bit samples)";
constexpr const char* DB60_DESCRIPTION = "-60 dB (PCM range of 10 bit samples)";
constexpr const char* DB72_DESCRIPTION = "-72 dB (PCM range of 12 bit samples)";
constexpr const char* DB84_DESCRIPTION = "-84 dB (PCM range of 14 bit samples)";
constexpr const char* DB96_DESCRIPTION = "-96 dB (PCM range of 16 bit samples)";
constexpr const char* DB120_DESCRIPTION = "-120 dB (approximate limit of human hearing)";
constexpr const char* DB144_DESCRIPTION = "-145 dB (PCM range of 24 bit samples)";

constexpr const char* DB36_ACTION = "meter-db-range-36";
constexpr const char* DB48_ACTION = "meter-db-range-48";
constexpr const char* DB60_ACTION = "meter-db-range-60";
constexpr const char* DB72_ACTION = "meter-db-range-72";
constexpr const char* DB84_ACTION = "meter-db-range-84";
constexpr const char* DB96_ACTION = "meter-db-range-96";
constexpr const char* DB120_ACTION = "meter-db-range-120";
constexpr const char* DB144_ACTION = "meter-db-range-145";
}

PlaybackMeterDbRangeModel:: PlaybackMeterDbRangeModel(QObject* parent)
    : muse::uicomponents::AbstractMenuModel(parent)
{
}

void PlaybackMeterDbRangeModel::load()
{
    muse::uicomponents::MenuItemList items;
    items << makeMenuItem(DB36_ACTION, muse::TranslatableString("dbRanges", description(PlaybackMeterDbRange::DbRange::Range36)));
    items << makeMenuItem(DB48_ACTION, muse::TranslatableString("dbRanges", description(PlaybackMeterDbRange::DbRange::Range48)));
    items << makeMenuItem(DB60_ACTION, muse::TranslatableString("dbRanges", description(PlaybackMeterDbRange::DbRange::Range60)));
    items << makeMenuItem(DB72_ACTION, muse::TranslatableString("dbRanges", description(PlaybackMeterDbRange::DbRange::Range72)));
    items << makeMenuItem(DB84_ACTION, muse::TranslatableString("dbRanges", description(PlaybackMeterDbRange::DbRange::Range84)));
    items << makeMenuItem(DB96_ACTION, muse::TranslatableString("dbRanges", description(PlaybackMeterDbRange::DbRange::Range96)));
    items << makeMenuItem(DB120_ACTION, muse::TranslatableString("dbRanges", description(PlaybackMeterDbRange::DbRange::Range120)));
    items << makeMenuItem(DB144_ACTION, muse::TranslatableString("dbRanges", description(PlaybackMeterDbRange::DbRange::Range144)));

    setItems(items);
}

const char* PlaybackMeterDbRangeModel::description(PlaybackMeterDbRange::DbRange range) const
{
    switch (range) {
    case PlaybackMeterDbRange::DbRange::Range36:
        return DB36_DESCRIPTION;
    case PlaybackMeterDbRange::DbRange::Range48:
        return DB48_DESCRIPTION;
    case PlaybackMeterDbRange::DbRange::Range60:
        return DB60_DESCRIPTION;
    case PlaybackMeterDbRange::DbRange::Range72:
        return DB72_DESCRIPTION;
    case PlaybackMeterDbRange::DbRange::Range84:
        return DB84_DESCRIPTION;
    case PlaybackMeterDbRange::DbRange::Range96:
        return DB96_DESCRIPTION;
    case PlaybackMeterDbRange::DbRange::Range120:
        return DB120_DESCRIPTION;
    case PlaybackMeterDbRange::DbRange::Range144:
        return DB144_DESCRIPTION;
    }

    return "";
}

const char* PlaybackMeterDbRangeModel::action(PlaybackMeterDbRange::DbRange range) const
{
    switch (range) {
    case PlaybackMeterDbRange::DbRange::Range36:
        return DB36_ACTION;
    case PlaybackMeterDbRange::DbRange::Range48:
        return DB48_ACTION;
    case PlaybackMeterDbRange::DbRange::Range60:
        return DB60_ACTION;
    case PlaybackMeterDbRange::DbRange::Range72:
        return DB72_ACTION;
    case PlaybackMeterDbRange::DbRange::Range84:
        return DB84_ACTION;
    case PlaybackMeterDbRange::DbRange::Range96:
        return DB96_ACTION;
    case PlaybackMeterDbRange::DbRange::Range120:
        return DB120_ACTION;
    case PlaybackMeterDbRange::DbRange::Range144:
        return DB144_ACTION;
    }

    return "";
}

std::optional<PlaybackMeterDbRange::DbRange> PlaybackMeterDbRangeModel::rangeFromAction(const std::string& action) const
{
    if (action == DB36_ACTION) {
        return PlaybackMeterDbRange::DbRange::Range36;
    } else if (action == DB48_ACTION) {
        return PlaybackMeterDbRange::DbRange::Range48;
    } else if (action == DB60_ACTION) {
        return PlaybackMeterDbRange::DbRange::Range60;
    } else if (action == DB72_ACTION) {
        return PlaybackMeterDbRange::DbRange::Range72;
    } else if (action == DB84_ACTION) {
        return PlaybackMeterDbRange::DbRange::Range84;
    } else if (action == DB96_ACTION) {
        return PlaybackMeterDbRange::DbRange::Range96;
    } else if (action == DB120_ACTION) {
        return PlaybackMeterDbRange::DbRange::Range120;
    } else if (action == DB144_ACTION) {
        return PlaybackMeterDbRange::DbRange::Range144;
    }
    return std::nullopt;
}
