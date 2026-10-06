/*
* Audacity: A Digital Audio Editor
*/
#include "autoduckviewmodel.h"

#include "autoduckeffect.h"

#include "global/types/number.h"

#include <algorithm>

using namespace au::effects;

namespace {
// The parameter itself has no upper bound, but a knob needs one.
constexpr double holdKnobMax = 10.0;
}

AutoDuckViewModel::AutoDuckViewModel(QObject* parent, int instanceId)
    : BuiltinEffectModel(parent, instanceId)
{
}

void AutoDuckViewModel::doReload()
{
    emit paramsChanged();
    emit controlTrackChanged();
}

void AutoDuckViewModel::setParam(double AutoDuckEffect::* member, double value)
{
    auto& ade = effect<AutoDuckEffect>();
    if (muse::is_equal(ade.*member, value)) {
        return;
    }
    ade.*member = value;
    emit paramsChanged();
}

double AutoDuckViewModel::duckStart() const
{
    return -effect<AutoDuckEffect>().mOuterFadeDownLen;
}

void AutoDuckViewModel::setDuckStart(double value)
{
    setParam(&AutoDuckEffect::mOuterFadeDownLen,
             std::clamp(-value, AutoDuckEffect::OuterFadeDownLen.min, AutoDuckEffect::OuterFadeDownLen.max));
}

double AutoDuckViewModel::duckEnd() const
{
    return effect<AutoDuckEffect>().mInnerFadeDownLen;
}

void AutoDuckViewModel::setDuckEnd(double value)
{
    setParam(&AutoDuckEffect::mInnerFadeDownLen,
             std::clamp(value, AutoDuckEffect::InnerFadeDownLen.min, AutoDuckEffect::InnerFadeDownLen.max));
}

double AutoDuckViewModel::recoveryStart() const
{
    return -effect<AutoDuckEffect>().mInnerFadeUpLen;
}

void AutoDuckViewModel::setRecoveryStart(double value)
{
    setParam(&AutoDuckEffect::mInnerFadeUpLen,
             std::clamp(-value, AutoDuckEffect::InnerFadeUpLen.min, AutoDuckEffect::InnerFadeUpLen.max));
}

double AutoDuckViewModel::recoveryEnd() const
{
    return effect<AutoDuckEffect>().mOuterFadeUpLen;
}

void AutoDuckViewModel::setRecoveryEnd(double value)
{
    setParam(&AutoDuckEffect::mOuterFadeUpLen,
             std::clamp(value, AutoDuckEffect::OuterFadeUpLen.min, AutoDuckEffect::OuterFadeUpLen.max));
}

double AutoDuckViewModel::gainReduction() const
{
    return effect<AutoDuckEffect>().mDuckAmountDb;
}

void AutoDuckViewModel::setGainReduction(double value)
{
    setParam(&AutoDuckEffect::mDuckAmountDb,
             std::clamp(value, AutoDuckEffect::DuckAmountDb.min, AutoDuckEffect::DuckAmountDb.max));
}

double AutoDuckViewModel::threshold() const
{
    return effect<AutoDuckEffect>().mThresholdDb;
}

void AutoDuckViewModel::setThreshold(double value)
{
    setParam(&AutoDuckEffect::mThresholdDb,
             std::clamp(value, AutoDuckEffect::ThresholdDb.min, AutoDuckEffect::ThresholdDb.max));
}

double AutoDuckViewModel::hold() const
{
    return effect<AutoDuckEffect>().mMaximumPause;
}

void AutoDuckViewModel::setHold(double value)
{
    setParam(&AutoDuckEffect::mMaximumPause,
             std::clamp(value, AutoDuckEffect::MaximumPause.min, AutoDuckEffect::MaximumPause.max));
}

double AutoDuckViewModel::fadeLengthMax() const
{
    static_assert(AutoDuckEffect::OuterFadeDownLen.max == AutoDuckEffect::InnerFadeDownLen.max);
    static_assert(AutoDuckEffect::OuterFadeDownLen.max == AutoDuckEffect::InnerFadeUpLen.max);
    static_assert(AutoDuckEffect::OuterFadeDownLen.max == AutoDuckEffect::OuterFadeUpLen.max);
    return AutoDuckEffect::OuterFadeDownLen.max;
}

double AutoDuckViewModel::gainReductionMin() const
{
    return AutoDuckEffect::DuckAmountDb.min;
}

double AutoDuckViewModel::gainReductionMax() const
{
    return AutoDuckEffect::DuckAmountDb.max;
}

double AutoDuckViewModel::thresholdMin() const
{
    return AutoDuckEffect::ThresholdDb.min;
}

double AutoDuckViewModel::thresholdMax() const
{
    return AutoDuckEffect::ThresholdDb.max;
}

double AutoDuckViewModel::holdMax() const
{
    return holdKnobMax;
}

QVariantMap AutoDuckViewModel::defaults() const
{
    return {
        { "duckStart", -AutoDuckEffect::OuterFadeDownLen.def },
        { "duckEnd", AutoDuckEffect::InnerFadeDownLen.def },
        { "recoveryStart", -AutoDuckEffect::InnerFadeUpLen.def },
        { "recoveryEnd", AutoDuckEffect::OuterFadeUpLen.def },
        { "gainReduction", AutoDuckEffect::DuckAmountDb.def },
        { "threshold", AutoDuckEffect::ThresholdDb.def },
        { "hold", AutoDuckEffect::MaximumPause.def },
    };
}

QVariantList AutoDuckViewModel::controlTrackOptions() const
{
    QVariantList options;
    const auto& candidates = effect<AutoDuckEffect>().ControlTrackCandidates();
    for (size_t i = 0; i < candidates.size(); ++i) {
        QVariantMap option;
        option["text"] = QString::fromStdString(candidates[i].name);
        option["value"] = static_cast<int>(i);
        options << option;
    }
    return options;
}

int AutoDuckViewModel::controlTrackIndex() const
{
    const auto& ade = effect<AutoDuckEffect>();
    const auto id = ade.ControlTrackId();
    if (!id) {
        return -1;
    }
    const auto& candidates = ade.ControlTrackCandidates();
    const auto it = std::find_if(candidates.begin(), candidates.end(),
                                 [&](const AutoDuckEffect::ControlTrackCandidate& c) { return c.id == *id; });
    return it == candidates.end() ? -1 : static_cast<int>(std::distance(candidates.begin(), it));
}

void AutoDuckViewModel::setControlTrackIndex(int index)
{
    auto& ade = effect<AutoDuckEffect>();
    const auto& candidates = ade.ControlTrackCandidates();
    if (index < 0 || index >= static_cast<int>(candidates.size()) || index == controlTrackIndex()) {
        return;
    }
    ade.SetControlTrackId(candidates[index].id);
    emit controlTrackChanged();
}

bool AutoDuckViewModel::hasControlTrack() const
{
    return controlTrackIndex() >= 0;
}
