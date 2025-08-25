/*
* Audacity: A Digital Audio Editor
*/
#include "recordconfiguration.h"

#include "global/settings.h"

using namespace muse;
using namespace au::record;

namespace {
const muse::Settings::Key MIC_METERING_KEY("record", "record/micMetering");
}

void RecordConfiguration::init()
{
    muse::settings()->setDefaultValue(MIC_METERING_KEY, muse::Val(true));
    muse::settings()->valueChanged(MIC_METERING_KEY).onReceive(nullptr, [this](const muse::Val&) {
        m_isMicMeteringOnChanged.notify();
    });
}

draw::Color RecordConfiguration::recordColor() const
{
    return "#EF476F";
}

bool RecordConfiguration::isMicMeteringOn() const
{
    return muse::settings()->value(MIC_METERING_KEY).toBool();
}

void RecordConfiguration::setIsMicMeteringOn(bool enable)
{
    muse::settings()->setSharedValue(MIC_METERING_KEY, muse::Val(enable));
}

muse::async::Notification RecordConfiguration::isMicMeteringOnChanged() const
{
    return m_isMicMeteringOnChanged;
}
