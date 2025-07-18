/*
* Audacity: A Digital Audio Editor
*/

#include "playbackmeterdbrangemodel.h"

using namespace au::playback;

PlaybackMeterDbRangeModel:: PlaybackMeterDbRangeModel(QObject* parent)
    : muse::uicomponents::AbstractMenuModel(parent)
{
}

void PlaybackMeterDbRangeModel::load()
{
    muse::uicomponents::MenuItemList items;
    items << makeMenuItem("meter-db-range-36", muse::TranslatableString("dbRanges", "-36 dB (shallow range for high-amplitude editing)"));
    items << makeMenuItem("meter-db-range-48", muse::TranslatableString("dbRanges", "-48 dB (PCM range of 8 bit samples)"));
    items << makeMenuItem("meter-db-range-60", muse::TranslatableString("dbRanges", "-60 dB (PCM range of 10 bit samples)"));
    items << makeMenuItem("meter-db-range-72", muse::TranslatableString("dbRanges", "-72 dB (PCM range of 12 bit samples)"));
    items << makeMenuItem("meter-db-range-84", muse::TranslatableString("dbRanges", "-84 dB (PCM range of 14 bit samples)"));
    items << makeMenuItem("meter-db-range-96", muse::TranslatableString("dbRanges", "-96 dB (PCM range of 16 bit samples)"));
    items << makeMenuItem("meter-db-range-120", muse::TranslatableString("dbRanges", "-120 dB (approximate limit of human hearing)"));
    items << makeMenuItem("meter-db-range-145", muse::TranslatableString("dbRanges", "-145 dB (PCM range of 24 bit samples)"));

    setItems(items);
}
