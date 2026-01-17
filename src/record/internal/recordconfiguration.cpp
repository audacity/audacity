/*
* Audacity: A Digital Audio Editor
*/
#include "recordconfiguration.h"

#include "global/settings.h"

using namespace muse;
using namespace au::record;

namespace {
const muse::Settings::Key MIC_METERING_KEY("record", "record/micMetering");
const muse::Settings::Key INPUT_MONITORING_KEY("record", "record/inputMonitoring");
}

void RecordConfiguration::init()
{
    muse::settings()->setDefaultValue(MIC_METERING_KEY, muse::Val(true));
    muse::settings()->valueChanged(MIC_METERING_KEY).onReceive(nullptr, [this](const muse::Val&) {
        m_isMicMeteringOnChanged.notify();
    });

    muse::settings()->setDefaultValue(INPUT_MONITORING_KEY, muse::Val(false));
    muse::settings()->valueChanged(INPUT_MONITORING_KEY).onReceive(nullptr, [this](const muse::Val&) {
        m_isInputMonitoringOnChanged.notify();
    });
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

bool RecordConfiguration::isInputMonitoringOn() const
{
    return muse::settings()->value(INPUT_MONITORING_KEY).toBool();
}

void RecordConfiguration::setIsInputMonitoringOn(bool enable)
{
    muse::settings()->setSharedValue(INPUT_MONITORING_KEY, muse::Val(enable));
}

muse::async::Notification RecordConfiguration::isInputMonitoringOnChanged() const
{
    return m_isInputMonitoringOnChanged;
}
