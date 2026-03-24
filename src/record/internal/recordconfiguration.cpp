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
const muse::Settings::Key PRE_ROLL_DURATION_KEY("record", "record/preRollDuration");
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

    muse::settings()->setDefaultValue(PRE_ROLL_DURATION_KEY, muse::Val(5.0));
    muse::settings()->valueChanged(PRE_ROLL_DURATION_KEY).onReceive(nullptr, [this](const muse::Val&) {
        m_preRollDurationChanged.notify();
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

double RecordConfiguration::preRollDuration() const
{
    return muse::settings()->value(PRE_ROLL_DURATION_KEY).toDouble();
}

void RecordConfiguration::setPreRollDuration(double seconds)
{
    muse::settings()->setSharedValue(PRE_ROLL_DURATION_KEY, muse::Val(seconds));
}

muse::async::Notification RecordConfiguration::preRollDurationChanged() const
{
    return m_preRollDurationChanged;
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
