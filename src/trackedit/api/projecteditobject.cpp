/*
 * Audacity: A Digital Audio Editor
 */
#include "internal/projecteditstate.h"
#include "projecteditobject.h"

#include <algorithm>
#include <utility>

#include <QVariantList>

#include "au3-math/SampleFormat.h"
#include "au3-transactions/TransactionScope.h"
#include "au3-wave-track/WaveTrack.h"
#include "log.h"

namespace au::trackedit::api::detail {
ProjectSelectionObject::ProjectSelectionObject(std::shared_ptr<EditState> state, QObject* parent)
    : QObject(parent), m_state(std::move(state))
{
    refresh();
}

void ProjectSelectionObject::refresh()
{
    m_audioTracks.clear();
    m_labelTracks.clear();
    for (size_t index = 0; index < m_state->audioTracks.size(); ++index) {
        const auto& track = m_state->audioTracks[index];
        if (!track.selected
            || (m_state->hasSelectedClips()
                && std::none_of(track.clips.begin(), track.clips.end(), [](const ProjectAudioClip& clip) {
            return clip.selected;
        }))) {
            continue;
        }
        QObject*& wrapper = m_audioWrappers[index];
        if (!wrapper) {
            wrapper = makeAudioTrack(m_state, index, true, this);
        }
        m_audioTracks.push_back(QVariant::fromValue<QObject*>(wrapper));
    }
    for (size_t index = 0; index < m_state->labelTracks.size(); ++index) {
        const auto& track = m_state->labelTracks[index];
        if (!track.selected
            || (m_state->hasSelectedLabels()
                && std::none_of(track.labels.begin(), track.labels.end(), [](const ProjectLabel& label) {
            return label.selected;
        }))) {
            continue;
        }
        QObject*& wrapper = m_labelWrappers[index];
        if (!wrapper) {
            wrapper = makeLabelTrack(m_state, index, true, this);
        }
        m_labelTracks.push_back(QVariant::fromValue<QObject*>(wrapper));
    }
}

double ProjectSelectionObject::start() const
{
    return m_state->selectionStart;
}

double ProjectSelectionObject::end() const
{
    return m_state->selectionEnd;
}

double ProjectSelectionObject::duration() const
{
    return m_state->selectionEnd - m_state->selectionStart;
}

QVariantList ProjectSelectionObject::audioTracks() const
{
    return m_audioTracks;
}

QVariantList ProjectSelectionObject::labelTracks() const
{
    return m_labelTracks;
}

ProjectEditObject::ProjectEditObject(std::shared_ptr<EditState> state, QObject* parent)
    : QObject(parent), m_state(std::move(state)), m_selection(new ProjectSelectionObject(m_state, this))
{
    for (size_t index = 0; index < m_state->audioTracks.size(); ++index) {
        m_audioTracks.push_back(QVariant::fromValue<QObject*>(makeAudioTrack(m_state, index, false, this)));
    }
    for (size_t index = 0; index < m_state->labelTracks.size(); ++index) {
        m_labelTracks.push_back(QVariant::fromValue<QObject*>(makeLabelTrack(m_state, index, false, this)));
    }
}

ProjectEditObject::~ProjectEditObject()
{
    if (m_state->active && !m_state->committed) {
        abandon();
    }
}

QObject* ProjectEditObject::selection() const
{
    return m_selection;
}

QVariantList ProjectEditObject::audioTracks() const
{
    return m_audioTracks;
}

QVariantList ProjectEditObject::labelTracks() const
{
    return m_labelTracks;
}

bool ProjectEditObject::setSelection(const QVariantMap& value)
{
    const double start = value.value(QStringLiteral("start")).toDouble();
    const double end = value.value(QStringLiteral("end")).toDouble();
    if (!m_state->writable() || !m_state->setSelection || !value.contains(QStringLiteral("start"))
        || !value.contains(QStringLiteral("end")) || !value.contains(QStringLiteral("tracks")) || !validTimes(start, end)) {
        throwProjectError(this, QStringLiteral("Invalid project selection"));
        return false;
    }

    std::vector<size_t> audioTracks;
    std::vector<size_t> labelTracks;
    const QVariantList tracks = value.value(QStringLiteral("tracks")).toList();
    for (const QVariant& value : tracks) {
        QObject* object = value.value<QObject*>();
        if (const auto index = audioTrackIndex(m_state, object)) {
            audioTracks.push_back(*index);
        } else if (const auto index = labelTrackIndex(m_state, object)) {
            labelTracks.push_back(*index);
        } else {
            throwProjectError(this, QStringLiteral("Invalid project selection track"));
            return false;
        }
    }

    for (auto& track : m_state->audioTracks) {
        track.selected = false;
    }
    for (auto& track : m_state->labelTracks) {
        track.selected = false;
    }
    for (size_t index : audioTracks) {
        m_state->audioTracks[index].selected = true;
    }
    for (size_t index : labelTracks) {
        m_state->labelTracks[index].selected = true;
    }
    m_state->selectionStart = start;
    m_state->selectionEnd = end;
    m_state->selectionChanged = true;
    m_selection->refresh();
    return true;
}

QObject* ProjectEditObject::createAudioWriter(const QVariantMap& value)
{
    const auto format = audioFormat(value);
    if (!m_state->writable() || !format) {
        throwProjectError(this, QStringLiteral("Invalid audio writer format"));
        return nullptr;
    }
    try {
        auto audio = m_state->factory->Create(format->channelCount, floatSample, format->sampleRate);
        ++m_state->writers;
        return makeAudioWriter(m_state, *format, std::move(audio), this);
    } catch (...) {
        throwProjectError(this, QStringLiteral("Could not create the audio writer"));
        return nullptr;
    }
}

bool ProjectEditObject::transformAudio(QObject* track, const QVariantMap& options, QObject* processor, QObject* task)
{
    return detail::transformAudio(m_state, track, options, processor, task, this);
}

QObject* ProjectEditObject::createLabelTrack(const QString& name)
{
    if (!m_state->writable()) {
        throwProjectError(this, QStringLiteral("The project edit is no longer active"));
        return nullptr;
    }
    m_state->addedLabelTracks.push_back({ name.toStdString(), {} });
    return makeAddedLabelTrack(m_state, m_state->addedLabelTracks.size() - 1, this);
}

void ProjectEditObject::commit()
{
    if (!m_state->writable()) {
        throwProjectError(this, QStringLiteral("The project edit is no longer active"));
        return;
    }
    if (m_state->writers != m_state->routedWriters) {
        throwProjectError(this, QStringLiteral("An audio writer was not routed"));
        return;
    }
    std::string error;
    AppliedChanges applied;
    bool storageCommitted = false;
    const auto rollback = [&] {
        std::string rollbackError;
        if (!rollbackChanges(*m_state, applied, rollbackError)) {
            LOGE() << "failed to restore project edit: " << rollbackError;
        }
    };
    try {
        m_state->prepared = std::make_unique<PreparedChanges>();
        if (!prepareChanges(*m_state, *m_state->prepared, error)) {
            throwProjectError(this, QString::fromStdString(error));
            return;
        }
        m_state->committed = true;
        if (m_state->deferredCommit) {
            return;
        }
        if (!applyChanges(*m_state, error, &applied)) {
            throwProjectError(this, QString::fromStdString(error));
            rollback();
            abandon();
            return;
        }
        if (m_state->transaction && !m_state->transaction->Commit()) {
            throwProjectError(this, QStringLiteral("Could not commit project storage"));
            rollback();
            abandon();
            return;
        }
        storageCommitted = true;
        m_state->active = false;
        m_state->transaction.reset();
        m_state->release();
    } catch (const std::exception& exception) {
        throwProjectError(this, QString::fromUtf8(exception.what()));
        if (!storageCommitted) {
            rollback();
        }
        abandon();
        return;
    } catch (...) {
        throwProjectError(this, QStringLiteral("Could not commit project changes"));
        if (!storageCommitted) {
            rollback();
        }
        abandon();
        return;
    }

    try {
        if (m_state->selectionChanged && m_state->setSelection) {
            m_state->setSelection(m_state->selectionStart, m_state->selectionEnd, m_state->selectedTrackIds);
        }
        m_state->history->pushHistoryState(m_state->undoName, m_state->undoName);
    } catch (const std::exception& exception) {
        LOGE() << "project edit was committed but finalization failed: " << exception.what();
    } catch (...) {
        LOGE() << "project edit was committed but finalization failed";
    }
}

void ProjectEditObject::abort()
{
    if (!m_state->active || m_state->committed) {
        throwProjectError(this, QStringLiteral("The project edit is no longer active"));
        return;
    }
    abandon();
}

void ProjectEditObject::abandon()
{
    m_state->active = false;
    m_state->transaction.reset();
    m_state->release();
}

QObject* makeProjectSelection(std::shared_ptr<EditState> state, QObject* parent)
{
    return new ProjectSelectionObject(std::move(state), parent);
}

QObject* makeProjectEdit(std::shared_ptr<EditState> state, QObject* parent)
{
    return new ProjectEditObject(std::move(state), parent);
}
} // namespace au::trackedit::api::detail
