/*
 * Audacity: A Digital Audio Editor
 */
#include "tracksendsmodel.h"

#include <QVariantMap>

#include "framework/global/containers.h"

using namespace au::projectscene;
using namespace au::trackedit;

TrackSendsModel::TrackSendsModel(QObject* parent)
    : QAbstractListModel(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

QVariant TrackSendsModel::trackId() const
{
    return QVariant::fromValue(m_trackId);
}

void TrackSendsModel::setTrackId(const QVariant& trackId)
{
    const TrackId id = trackId.isValid() ? trackId.toLongLong() : INVALID_TRACK;
    if (m_trackId == id) {
        return;
    }
    m_trackId = id;
    emit trackIdChanged();
}

void TrackSendsModel::load()
{
    beginResetModel();
    m_sends = trackeditInteraction()->trackSends(m_trackId);
    endResetModel();

    // Busses are renamed and go away, and undo changes the sends
    if (const auto project = globalContext()->currentTrackeditProject()) {
        const auto reload = [this](const auto&...) {
            beginResetModel();
            m_sends = trackeditInteraction()->trackSends(m_trackId);
            endResetModel();
        };
        project->trackChanged().onReceive(this, reload, muse::async::Asyncable::Mode::SetReplace);
        project->trackRemoved().onReceive(this, reload, muse::async::Asyncable::Mode::SetReplace);
        project->tracksChanged().onReceive(this, reload, muse::async::Asyncable::Mode::SetReplace);
    }
}

QVariantList TrackSendsModel::addMenu() const
{
    QVariantList result;
    const auto project = globalContext()->currentTrackeditProject();
    if (!project) {
        return result;
    }
    for (const Track& track : project->trackList()) {
        if (track.type != TrackType::Bus || track.id == m_trackId) {
            continue;
        }
        const bool sent = std::any_of(m_sends.begin(), m_sends.end(), [&](const TrackSend& send) { return send.bus == track.id; });
        QVariantMap item;
        item["id"] = QString::number(track.id);
        item["title"] = track.title.toQString();
        item["enabled"] = !sent && trackeditInteraction()->canOutputTo(m_trackId, track.id);
        result << item;
    }
    return result;
}

void TrackSendsModel::addSend(const QString& busId)
{
    trackeditInteraction()->addTrackSend(m_trackId, busId.toLongLong());
}

void TrackSendsModel::removeSend(int row)
{
    if (row >= 0 && row < rowCount()) {
        trackeditInteraction()->removeTrackSend(m_trackId, m_sends.at(row).bus);
    }
}

void TrackSendsModel::setLevel(int row, float level, bool completed)
{
    if (row < 0 || row >= rowCount()) {
        return;
    }
    // The slider shows it already
    m_sends[row].level = level;
    trackeditInteraction()->setTrackSendLevel(m_trackId, m_sends.at(row).bus, level, completed);
}

void TrackSendsModel::setPreFader(int row, bool preFader)
{
    if (row >= 0 && row < rowCount()) {
        trackeditInteraction()->setTrackSendPreFader(m_trackId, m_sends.at(row).bus, preFader);
    }
}

int TrackSendsModel::rowCount(const QModelIndex&) const
{
    return static_cast<int>(m_sends.size());
}

QVariant TrackSendsModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid() || index.row() >= rowCount()) {
        return {};
    }
    const TrackSend& send = m_sends.at(index.row());
    switch (role) {
    case TitleRole:
        return busTitle(send.bus);
    case LevelRole:
        return send.level;
    case PreFaderRole:
        return send.preFader;
    default:
        return {};
    }
}

QHash<int, QByteArray> TrackSendsModel::roleNames() const
{
    return {
        { TitleRole, "title" },
        { LevelRole, "level" },
        { PreFaderRole, "preFader" },
    };
}

QString TrackSendsModel::busTitle(TrackId busId) const
{
    const auto project = globalContext()->currentTrackeditProject();
    const auto bus = project ? project->track(busId) : std::nullopt;
    return bus ? bus->title.toQString() : QString {};
}
