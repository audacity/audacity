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
const muse::Settings::Key LEAD_IN_TIME_DURATION_KEY("record", "record/leadInTimeDuration");
const muse::Settings::Key CROSSFADE_DURATION_KEY("record", "record/crossfadeDuration");
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

    muse::settings()->setDefaultValue(LEAD_IN_TIME_DURATION_KEY, muse::Val(2.0));
    muse::settings()->valueChanged(LEAD_IN_TIME_DURATION_KEY).onReceive(nullptr, [this](const muse::Val&) {
        m_leadInTimeDurationChanged.notify();
    });

    muse::settings()->setDefaultValue(CROSSFADE_DURATION_KEY, muse::Val(10.0));
    muse::settings()->valueChanged(CROSSFADE_DURATION_KEY).onReceive(nullptr, [this](const muse::Val&) {
        m_crossfadeDurationChanged.notify();
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

double RecordConfiguration::leadInTimeDuration() const
{
    return muse::settings()->value(LEAD_IN_TIME_DURATION_KEY).toDouble();
}

void RecordConfiguration::setLeadInTimeDuration(double seconds)
{
    muse::settings()->setSharedValue(LEAD_IN_TIME_DURATION_KEY, muse::Val(seconds));
}

muse::async::Notification RecordConfiguration::leadInTimeDurationChanged() const
{
    return m_leadInTimeDurationChanged;
}

double RecordConfiguration::crossfadeDuration() const
{
    return muse::settings()->value(CROSSFADE_DURATION_KEY).toDouble();
}

void RecordConfiguration::setCrossfadeDuration(double milliseconds)
{
    muse::settings()->setSharedValue(CROSSFADE_DURATION_KEY, muse::Val(milliseconds));
}

muse::async::Notification RecordConfiguration::crossfadeDurationChanged() const
{
    return m_crossfadeDurationChanged;
}
