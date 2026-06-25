/*
 * Audacity: A Digital Audio Editor
 */
#include "filtercurveeqviewmodel.h"

#include "filtercurveeq.h"
#include "filtercurvemodel.h"

#include "au3-builtin-effects/EqualizationCurvesList.h"
#include "au3-builtin-effects/EqualizationFilter.h"

#include "../equalization_common/equalizationenvelopeutils.h"

#include "shared/axis/axislabel.h"
#include "shared/axis/axisscale.h"
#include "shared/axis/axisticks.h"
#include "types/number.h"

#include <algorithm>

namespace au::effects {
namespace {
// Tunable: each zoom step widens or narrows the visible dB range by this
// amount on each side (so the total range changes by 2×step).
constexpr float kZoomStepDb = 6.0f;
constexpr float kMinDbHalfRange = 6.0f;    // smallest half-range allowed when zooming in
constexpr float kDbHardMin = -120.0f;
constexpr float kDbHardMax = 60.0f;
}

FilterCurveEqViewModel::FilterCurveEqViewModel(QObject* parent, int instanceId)
    : BuiltinEffectModel{parent, instanceId},
    m_curveModel(new FilterCurveModel(this, effect<FilterCurveEq>()))
{
    connect(this, &FilterCurveEqViewModel::freqRangeChanged, this, &FilterCurveEqViewModel::xTicksChanged);
    connect(this, &FilterCurveEqViewModel::linFreqScaleChanged, this, &FilterCurveEqViewModel::xTicksChanged);
}

void FilterCurveEqViewModel::doReload()
{
    m_curveModel->reload();
    emit curveModelChanged();
    emit freqRangeChanged();
    emit gridlinesVisibleChanged();
    emit dbRangeChanged();
    emit linFreqScaleChanged();
}

FilterCurveModel* FilterCurveEqViewModel::curveModel() const
{
    return m_curveModel;
}

double FilterCurveEqViewModel::dbHardMin() const
{
    return kDbHardMin;
}

double FilterCurveEqViewModel::dbHardMax() const
{
    return kDbHardMax;
}

double FilterCurveEqViewModel::dbMin() const
{
    return effect<FilterCurveEq>().mCurvesList.mParameters.mdBMin;
}

double FilterCurveEqViewModel::dbMax() const
{
    return effect<FilterCurveEq>().mCurvesList.mParameters.mdBMax;
}

bool FilterCurveEqViewModel::canZoomIn() const
{
    const auto& parameters = effect<FilterCurveEq>().mCurvesList.mParameters;
    if (parameters.mdBMin < -kDbHardMax) {
        // Asymmetric phase: lower bound can always be raised toward -kDbHardMax.
        return true;
    }
    return parameters.mdBMax - kZoomStepDb >= kMinDbHalfRange
           && -parameters.mdBMin - kZoomStepDb >= kMinDbHalfRange;
}

bool FilterCurveEqViewModel::canZoomOut() const
{
    const auto& parameters = effect<FilterCurveEq>().mCurvesList.mParameters;
    if (parameters.mdBMax >= kDbHardMax) {
        // Upper bound capped; only the lower bound can still extend.
        return parameters.mdBMin > kDbHardMin;
    }
    return parameters.mdBMax + kZoomStepDb <= kDbHardMax
           && -parameters.mdBMin + kZoomStepDb <= kDbHardMax;
}

void FilterCurveEqViewModel::zoomIn()
{
    if (!canZoomIn()) {
        return;
    }
    auto& parameters = effect<FilterCurveEq>().mCurvesList.mParameters;
    if (parameters.mdBMin < -kDbHardMax) {
        parameters.mdBMin = std::min(parameters.mdBMin + kZoomStepDb, -kDbHardMax);
    } else {
        parameters.mdBMin += kZoomStepDb;
        parameters.mdBMax -= kZoomStepDb;
    }
    emit dbRangeChanged();
}

void FilterCurveEqViewModel::zoomOut()
{
    if (!canZoomOut()) {
        return;
    }
    auto& parameters = effect<FilterCurveEq>().mCurvesList.mParameters;
    if (parameters.mdBMax >= kDbHardMax) {
        parameters.mdBMin = std::max(parameters.mdBMin - kZoomStepDb, kDbHardMin);
    } else {
        parameters.mdBMin -= kZoomStepDb;
        parameters.mdBMax += kZoomStepDb;
    }
    emit dbRangeChanged();
}

double FilterCurveEqViewModel::loFreq() const
{
    return effect<FilterCurveEq>().mCurvesList.mParameters.mLoFreq;
}

double FilterCurveEqViewModel::hiFreq() const
{
    return effect<FilterCurveEq>().mCurvesList.mParameters.mHiFreq;
}

bool FilterCurveEqViewModel::gridlinesVisible() const
{
    return effect<FilterCurveEq>().mCurvesList.mParameters.mDrawGrid;
}

void FilterCurveEqViewModel::setGridlinesVisible(bool v)
{
    auto& parameters = effect<FilterCurveEq>().mCurvesList.mParameters;
    if (parameters.mDrawGrid == v) {
        return;
    }
    parameters.mDrawGrid = v;
    emit gridlinesVisibleChanged();
}

bool FilterCurveEqViewModel::linFreqScale() const
{
    return effect<FilterCurveEq>().mCurvesList.mParameters.mLin;
}

void FilterCurveEqViewModel::setLinFreqScale(bool v)
{
    auto& parameters = effect<FilterCurveEq>().mCurvesList.mParameters;
    if (parameters.mLin == v) {
        return;
    }
    if (v) {
        eq_common::envLogToLin(parameters);
    } else {
        eq_common::envLinToLog(parameters);
    }
    parameters.mLin = v;
    m_curveModel->reload();
    emit linFreqScaleChanged();
    emit curveModelChanged();
}

void FilterCurveEqViewModel::setLabelWidth(int v)
{
    if (m_labelWidth == v) {
        return;
    }
    m_labelWidth = v;
    emit labelWidthChanged();
    emit xTicksChanged();
}

void FilterCurveEqViewModel::setAxisWidth(double v)
{
    if (muse::is_equal(m_axisWidth, v)) {
        return;
    }
    m_axisWidth = v;
    emit axisWidthChanged();
    emit xTicksChanged();
}

QVariantList FilterCurveEqViewModel::xTicks() const
{
    if (m_labelWidth <= 0 || m_axisWidth <= 0) {
        return {};
    }
    std::vector<LabelAndPos> labelAndPos = linFreqScale() ? xTicksLin() : xTicksLog();

    std::sort(labelAndPos.begin(), labelAndPos.end(), [](const LabelAndPos& a, const LabelAndPos& b) {
        return a.position < b.position;
    });

    QVariantList list;
    list.reserve(labelAndPos.size());
    for (const auto& lp : labelAndPos) {
        list.append(QVariant::fromValue(QVariantMap {
                { "label", lp.label },
                { "position", lp.position },
            }));
    }

    return list;
}

std::vector<FilterCurveEqViewModel::LabelAndPos> FilterCurveEqViewModel::xTicksLin() const
{
    const shared::AxisTicks ticks = shared::axisTicks(loFreq(), hiFreq(), shared::AxisScale::Linear);
    const auto labels = shared::labelsForTicks(ticks.major, m_labelWidth, m_axisWidth);
    std::vector<LabelAndPos> labelAndPos;
    labelAndPos.reserve(ticks.major.size() + ticks.minor.size());
    for (size_t i = 0; i < ticks.major.size(); ++i) {
        labelAndPos.push_back({ QString::fromStdString(labels[i]), ticks.major[i].position });
    }
    for (const auto& tick : ticks.minor) {
        labelAndPos.push_back({ QString(), tick.position });
    }
    return labelAndPos;
}

std::vector<FilterCurveEqViewModel::LabelAndPos> FilterCurveEqViewModel::xTicksLog() const
{
    const auto left = std::log10(loFreq());
    const auto right = std::log10(hiFreq());
    const auto range = right - left;
    auto exponent = std::floor(left);
    auto increment = std::pow(10, exponent);
    int multiplier = std::ceil(loFreq() / increment);

    std::vector<shared::AxisTick> majors;
    std::vector<shared::AxisTick> minors;
    auto freq = 0.;
    while (true) {
        freq = std::round(multiplier * std::pow(10, exponent));
        if (freq > hiFreq()) {
            break;
        }
        const auto pos = (std::log10(freq) - left) / range;
        const auto isMajor = muse::is_equal(freq, loFreq()) || muse::is_equal(freq, hiFreq()) || multiplier == 1 || multiplier == 5;
        (isMajor ? majors : minors).push_back({ freq, pos });
        if (++multiplier % 10 == 0) {
            multiplier = 1;
            ++exponent;
        }
    }
    if (majors.empty() || !muse::is_equal(majors.front().val, loFreq())) {
        majors.insert(majors.begin(), { loFreq(), 0. });
    }
    if (!muse::is_equal(majors.back().val, hiFreq())) {
        majors.insert(majors.end(), { hiFreq(), 1. });
    }

    const auto majorLabels = shared::labelsForTicks(majors, m_labelWidth, m_axisWidth);

    std::vector<LabelAndPos> labelAndPos;
    labelAndPos.reserve(majors.size() + minors.size());

    for (size_t i = 0; i < majors.size(); ++i) {
        labelAndPos.push_back({ QString::fromStdString(majorLabels[i]), majors[i].position });
    }
    for (size_t i = 0; i < minors.size(); ++i) {
        labelAndPos.push_back({ QString(), minors[i].position });
    }

    return labelAndPos;
}
}
