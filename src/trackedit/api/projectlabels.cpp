/*
 * Audacity: A Digital Audio Editor
 */
#include "projectlabels.h"

#include <algorithm>
#include <utility>

#include "internal/projecteditstate.h"

namespace au::trackedit::api::detail {
LabelObject::LabelObject(std::shared_ptr<EditState> state, size_t trackIndex, size_t labelIndex, QObject* parent)
    : QObject(parent), m_state(std::move(state)), m_trackIndex(trackIndex), m_labelIndex(labelIndex)
{
}

double LabelObject::start() const
{
    return source().start;
}

double LabelObject::end() const
{
    return source().end;
}

QString LabelObject::text() const
{
    return QString::fromStdString(source().text);
}

bool LabelObject::update(double start, double end, const QString& text)
{
    if (!m_state->writable() || m_removed || !validTimes(start, end)) {
        throwProjectError(this, QStringLiteral("Invalid label update"));
        return false;
    }
    const auto matches = [&](const LabelChange& change) {
        return change.trackIndex == m_trackIndex && change.nativeId == source().nativeId;
    };
    auto found = std::find_if(m_state->labelChanges.begin(), m_state->labelChanges.end(), matches);
    if (found == m_state->labelChanges.end()) {
        m_state->labelChanges.push_back({
                m_trackIndex,
                source().nativeId,
                false,
                start,
                end,
                text.toStdString(),
            });
    } else {
        if (found->remove) {
            throwProjectError(this, QStringLiteral("The label was removed"));
            return false;
        }
        found->start = start;
        found->end = end;
        found->text = text.toStdString();
    }
    return true;
}

bool LabelObject::remove()
{
    if (!m_state->writable() || m_removed) {
        throwProjectError(this, QStringLiteral("The label cannot be removed"));
        return false;
    }
    const auto matches = [&](const LabelChange& change) {
        return change.trackIndex == m_trackIndex && change.nativeId == source().nativeId;
    };
    m_state->labelChanges.erase(std::remove_if(m_state->labelChanges.begin(),
                                               m_state->labelChanges.end(), matches), m_state->labelChanges.end());
    m_state->labelChanges.push_back({
            m_trackIndex,
            source().nativeId,
            true,
            0.0,
            0.0,
            {},
        });
    m_removed = true;
    return true;
}

const ProjectLabel& LabelObject::source() const
{
    return m_state->labelTracks.at(m_trackIndex).labels.at(m_labelIndex);
}

LabelTrackObject::LabelTrackObject(std::shared_ptr<EditState> state, size_t index, bool intersectingLabelsOnly, QObject* parent)
    : QObject(parent), m_state(std::move(state)), m_index(index), m_intersectingOnly(intersectingLabelsOnly)
{
}

QString LabelTrackObject::name() const
{
    return QString::fromStdString(m_state->labelTracks.at(m_index).name);
}

QVariantList LabelTrackObject::labels() const
{
    QVariantList result;
    const auto& input = m_state->labelTracks.at(m_index);
    for (size_t label = 0; label < input.labels.size(); ++label) {
        if (m_intersectingOnly) {
            if (m_state->hasSelectedLabels()) {
                if (!input.labels[label].selected) {
                    continue;
                }
            } else if (input.labels[label].end < m_state->selectionStart || input.labels[label].start > m_state->selectionEnd) {
                continue;
            }
        }
        QObject*& wrapper = m_labelWrappers[label];
        if (!wrapper) {
            wrapper = new LabelObject(m_state, m_index, label, const_cast<LabelTrackObject*>(this));
        }
        result.push_back(QVariant::fromValue<QObject*>(wrapper));
    }
    return result;
}

size_t LabelTrackObject::index() const
{
    return m_index;
}

const std::shared_ptr<EditState>& LabelTrackObject::state() const
{
    return m_state;
}

bool LabelTrackObject::addLabel(double start, double end, const QString& text)
{
    if (!m_state->writable() || !validTimes(start, end)) {
        throwProjectError(this, QStringLiteral("Invalid label"));
        return false;
    }
    m_state->labelChanges.push_back({
            m_index,
            std::nullopt,
            false,
            start,
            end,
            text.toStdString(),
        });
    return true;
}

AddedLabelTrackObject::AddedLabelTrackObject(std::shared_ptr<EditState> state, size_t index, QObject* parent)
    : QObject(parent), m_state(std::move(state)), m_index(index)
{
}

bool AddedLabelTrackObject::addLabel(double start, double end, const QString& text)
{
    if (!m_state->writable() || !validTimes(start, end)) {
        throwProjectError(this, QStringLiteral("Invalid label"));
        return false;
    }
    m_state->addedLabelTracks.at(m_index).labels.push_back({
            0,
            start,
            end,
            text.toStdString(),
        });
    return true;
}

QObject* makeLabelTrack(std::shared_ptr<EditState> state, size_t index, bool intersectingLabelsOnly, QObject* parent)
{
    return new LabelTrackObject(std::move(state), index, intersectingLabelsOnly, parent);
}

std::optional<size_t> labelTrackIndex(const std::shared_ptr<EditState>& state, QObject* object)
{
    auto* track = qobject_cast<LabelTrackObject*>(object);
    return track && track->state() == state ? std::optional<size_t> { track->index() } : std::nullopt;
}

QObject* makeAddedLabelTrack(std::shared_ptr<EditState> state, size_t index, QObject* parent)
{
    return new AddedLabelTrackObject(std::move(state), index, parent);
}
} // namespace au::trackedit::api::detail
