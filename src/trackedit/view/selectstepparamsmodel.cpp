/*
* Audacity: A Digital Audio Editor
*/
#include "selectstepparamsmodel.h"

#include <ranges>

#include "framework/global/types/val.h"

#include "../internal/selectsteptype.h"

using namespace au::trackedit;
using namespace muse;
using namespace muse::rcommand;

namespace {
QString toQString(std::string_view text)
{
    return QString::fromUtf8(text.data(), static_cast<qsizetype>(text.size()));
}

QVariantMap option(const auto& entry)
{
    return { { "text", toQString(entry.description) }, { "value", toQString(entry.sval) } };
}

template<typename C>
QVariantList contractOptions()
{
    QVariantList list;
    for (const auto& entry : C::table) {
        list << option(entry);
    }
    return list;
}

template<typename E>
QString keyOf(E value)
{
    return QString::fromStdString(EnumContract<E>::str_val(value));
}

template<typename E>
std::optional<E> parsed(const QString& key)
{
    return EnumContract<E>::enum_val(key.toStdString());
}

Val valOf(const QVariantMap& map)
{
    ValMap values;
    for (auto it = map.cbegin(); it != map.cend(); ++it) {
        values[it.key().toStdString()] = Val::fromQVariant(it.value());
    }
    return Val(values);
}

QVariantMap toQVariantMap(const Val& val)
{
    QVariantMap map;
    for (const auto& [key, value] : val.toMap()) {
        map[QString::fromStdString(key)] = value.toQVariant();
    }
    return map;
}
}

SelectStepParamsModel::SelectStepParamsModel(QObject* parent)
    : QObject(parent)
{
}

QString SelectStepParamsModel::title() const
{
    return SELECT_ENHANCED_STEP_TITLE.qTranslated();
}

QVariantList SelectStepParamsModel::timeKindOptions() const
{
    return contractOptions<SelectTimeKindContract>();
}

QString SelectStepParamsModel::timeKind() const
{
    return keyOf(m_timeKind);
}

void SelectStepParamsModel::setTimeKind(const QString& kind)
{
    const std::optional<SelectTimeKind> value = parsed<SelectTimeKind>(kind);
    if (!value || *value == m_timeKind) {
        return;
    }

    m_timeKind = *value;
    emit stepChanged();
}

QVariantList SelectStepParamsModel::startAnchorOptions() const
{
    return contractOptions<SelectAnchorContract>();
}

QVariantList SelectStepParamsModel::endAnchorOptions() const
{
    const SelectAnchor start = startAnchorOf(m_timeRange.relativeTo);
    QVariantList list;
    for (const auto& entry : SelectAnchorContract::table | std::views::filter([start](const auto& entry) {
        return relativeToOf(start, entry.eval).has_value();
    })) {
        list << option(entry);
    }
    return list;
}

QString SelectStepParamsModel::startAnchor() const
{
    return keyOf(startAnchorOf(m_timeRange.relativeTo));
}

void SelectStepParamsModel::setStartAnchor(const QString& anchor)
{
    const std::optional<SelectAnchor> start = parsed<SelectAnchor>(anchor);
    if (!start || *start == startAnchorOf(m_timeRange.relativeTo)) {
        return;
    }

    const std::optional<SelectTimeRelativeTo> kept = relativeToOf(*start, endAnchorOf(m_timeRange.relativeTo));
    const std::optional<SelectTimeRelativeTo> relativeTo = kept ? kept : relativeToOf(*start, *start);
    if (!relativeTo) {
        return;
    }

    m_timeRange.relativeTo = *relativeTo;
    emit stepChanged();
}

QString SelectStepParamsModel::endAnchor() const
{
    return keyOf(endAnchorOf(m_timeRange.relativeTo));
}

void SelectStepParamsModel::setEndAnchor(const QString& anchor)
{
    const std::optional<SelectAnchor> end = parsed<SelectAnchor>(anchor);
    if (!end) {
        return;
    }

    const std::optional<SelectTimeRelativeTo> relativeTo = relativeToOf(startAnchorOf(m_timeRange.relativeTo), *end);
    if (!relativeTo || *relativeTo == m_timeRange.relativeTo) {
        return;
    }

    m_timeRange.relativeTo = *relativeTo;
    emit stepChanged();
}

double SelectStepParamsModel::startOffset() const
{
    return m_timeRange.startOffset;
}

void SelectStepParamsModel::setStartOffset(double offset)
{
    if (m_timeRange.startOffset == offset) {
        return;
    }

    m_timeRange.startOffset = offset;
    emit stepChanged();
}

double SelectStepParamsModel::endOffset() const
{
    return m_timeRange.endOffset;
}

void SelectStepParamsModel::setEndOffset(double offset)
{
    if (m_timeRange.endOffset == offset) {
        return;
    }

    m_timeRange.endOffset = offset;
    emit stepChanged();
}

QVariantList SelectStepParamsModel::tracksKindOptions() const
{
    return contractOptions<SelectTracksKindContract>();
}

QString SelectStepParamsModel::tracksKind() const
{
    return keyOf(m_tracksKind);
}

void SelectStepParamsModel::setTracksKind(const QString& kind)
{
    const std::optional<SelectTracksKind> value = parsed<SelectTracksKind>(kind);
    if (!value || *value == m_tracksKind) {
        return;
    }

    m_tracksKind = *value;
    emit stepChanged();
}

int SelectStepParamsModel::firstTrack() const
{
    return m_trackRange.firstTrack;
}

void SelectStepParamsModel::setFirstTrack(int track)
{
    if (track < 1 || track == m_trackRange.firstTrack) {
        return;
    }

    m_trackRange.firstTrack = track;
    emit stepChanged();
}

int SelectStepParamsModel::trackCount() const
{
    return m_trackRange.trackCount;
}

void SelectStepParamsModel::setTrackCount(int count)
{
    if (count < 1 || count == m_trackRange.trackCount) {
        return;
    }

    m_trackRange.trackCount = count;
    emit stepChanged();
}

QVariantList SelectStepParamsModel::modeOptions() const
{
    return contractOptions<SelectTracksModeContract>();
}

QString SelectStepParamsModel::mode() const
{
    return keyOf(m_trackRange.mode);
}

void SelectStepParamsModel::setMode(const QString& mode)
{
    const std::optional<SelectTracksMode> value = parsed<SelectTracksMode>(mode);
    if (!value || *value == m_trackRange.mode) {
        return;
    }

    m_trackRange.mode = *value;
    emit stepChanged();
}

QString SelectStepParamsModel::summary() const
{
    return selectStepSummary(step()).toQString();
}

void SelectStepParamsModel::load(const QVariantMap& data)
{
    const RetVal<SelectEnhancedStep> loaded = selectStepFromVal(valOf(data));
    const SelectEnhancedStep stored = loaded.ret ? loaded.val : SelectEnhancedStep {};

    m_timeKind = kindOf(stored.time);
    m_tracksKind = kindOf(stored.tracks);

    if (const auto* range = std::get_if<SelectTimeRange>(&stored.time)) {
        m_timeRange = *range;
    }

    if (const auto* range = std::get_if<SelectTrackRange>(&stored.tracks)) {
        m_trackRange = *range;
    }

    emit stepChanged();
}

QVariantMap SelectStepParamsModel::stepData() const
{
    return toQVariantMap(toVal(step()));
}

SelectEnhancedStep SelectStepParamsModel::step() const
{
    const SelectTimeSpec time = m_timeKind == SelectTimeKind::Range ? SelectTimeSpec(m_timeRange) : WholeProjectTime {};
    const SelectTrackSpec tracks = m_tracksKind == SelectTracksKind::Range ? SelectTrackSpec(m_trackRange) : AllTracks {};

    return SelectEnhancedStep { time, tracks };
}
