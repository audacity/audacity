/*
 * Audacity: A Digital Audio Editor
 */
#include "clipenvelopemodel.h"

#include "log.h"

using namespace au::projectscene;

ClipEnvelopeModel::ClipEnvelopeModel(QObject* parent)
    : QAbstractListModel(parent)
    , muse::Injectable(muse::modularity::ContextPtr())
{
}

void ClipEnvelopeModel::init()
{
    clipsInteraction()->clipStartTimeChanged().onReceive(
        this,
        [this](const ClipKey& key, muse::secs_t /*newStart*/, bool /*completed*/) {
        if (m_clipKey.isValid() && key == m_clipKey) {
            reload();
        }
    });

    clipsInteraction()->clipEnvelopeChanged().onReceive(
        this,
        [this](const ClipKey& key, bool /*completed*/) {
        if (m_clipKey.isValid() && key == m_clipKey) {
            reload();
        }
    });

    reload();
}

QVariant ClipEnvelopeModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid()) {
        return {};
    }

    const int row = index.row();
    if (row < 0 || row >= int(m_points.size())) {
        return {};
    }

    const auto& p = m_points[size_t(row)];

    switch (role) {
    case TimeRole:
        return p.time;
    case ValueRole:
        return p.value;
    default:
        return {};
    }
}

QHash<int, QByteArray> ClipEnvelopeModel::roleNames() const
{
    return {
        { TimeRole, "time" },
        { ValueRole, "value" },
    };
}

QVector<QPointF> ClipEnvelopeModel::points() const
{
    QVector<QPointF> out;
    out.reserve(int(m_points.size()));
    for (const auto& point : m_points) {
        out.push_back(QPointF(point.time, point.value));
    }
    return out;
}

double ClipEnvelopeModel::minValue() const
{
    return m_info.minValue;
}

double ClipEnvelopeModel::maxValue() const
{
    return m_info.maxValue;
}

double ClipEnvelopeModel::defaultValue() const
{
    return m_info.defaultValue;
}

bool ClipEnvelopeModel::exponential() const
{
    return m_info.exponential;
}

double ClipEnvelopeModel::clipStartTime() const
{
    return m_clipStartTime;
}

double ClipEnvelopeModel::clipEndTime() const
{
    return m_clipEndTime;
}

int ClipEnvelopeModel::rowCount(const QModelIndex& parent) const
{
    return parent.isValid() ? 0 : int(m_points.size());
}

au::projectscene::ClipKey ClipEnvelopeModel::clipKey() const
{
    return m_clipKey;
}

void ClipEnvelopeModel::setClipKey(const ClipKey& key)
{
    if (m_clipKey == key) {
        return;
    }

    m_clipKey = key;
    reload();
}

void ClipEnvelopeModel::reload()
{
    if (!m_clipKey.isValid()) {
        clear();
        return;
    }

    m_clipStartTime = clipsInteraction()->clipStartTime(m_clipKey.key);
    m_clipEndTime   = clipsInteraction()->clipEndTime(m_clipKey.key);
    emit clipTimeChanged();

    auto points = clipsInteraction()->clipEnvelopePoints(m_clipKey.key);
    auto infoOpt = clipsInteraction()->clipEnvelopeInfo(m_clipKey.key);
    if (infoOpt) {
        m_info = *infoOpt;
    } else {
        m_info = {};
    }

    beginResetModel();
    m_points = std::move(points);
    endResetModel();

    emit pointsChanged();
    emit envelopeInfoChanged();
}

void ClipEnvelopeModel::clear()
{
    if (m_points.empty()) {
        return;
    }
    beginResetModel();
    m_points.clear();
    endResetModel();

    emit pointsChanged();
}

void ClipEnvelopeModel::setPoint(int index, double tAbs, double value, bool completed)
{
    if (!m_clipKey.isValid()) {
        return;
    }
    if (index < 0 || index >= int(m_points.size())) {
        return;
    }

    if (!m_dragActive || m_dragIndex != index) {
        // if we somehow had a different drag active, end it
        if (m_dragActive) {
            clipsInteraction()->endClipEnvelopePointDrag(m_clipKey.key, /*commit*/ true);
        }

        if (!clipsInteraction()->beginClipEnvelopePointDrag(m_clipKey.key, index)) {
            m_dragActive = false;
            m_dragIndex = -1;
            return;
        }

        m_dragActive = true;
        m_dragIndex = index;
    }

    if (!clipsInteraction()->updateClipEnvelopePointDrag(m_clipKey.key, tAbs, value)) {
        return;
    }

    auto& p = m_points[size_t(index)];
    p.time = tAbs;
    p.value = value;
    const QModelIndex idx = this->index(index, 0);
    emit dataChanged(idx, idx, { TimeRole, ValueRole });

    if (completed) {
        clipsInteraction()->endClipEnvelopePointDrag(m_clipKey.key, /*commit*/ true);

        m_dragActive = false;
        m_dragIndex = -1;

        reload();
    }
}

void ClipEnvelopeModel::addPoint(double tAbs, double value, bool completed)
{
    if (!m_clipKey.isValid()) {
        return;
    }

    if (!clipsInteraction()->setClipEnvelopePoint(m_clipKey.key, tAbs, value, completed)) {
        return;
    }

    reload();
}

void ClipEnvelopeModel::removePoint(int index, bool completed)
{
    if (!m_clipKey.isValid()) {
        return;
    }
    if (index < 0 || index >= int(m_points.size())) {
        return;
    }

    if (!clipsInteraction()->removeClipEnvelopePoint(m_clipKey.key, index, completed)) {
        return;
    }

    reload();
}

void ClipEnvelopeModel::flatten(double value, bool completed)
{
    if (!m_clipKey.isValid()) {
        return;
    }

    if (!clipsInteraction()->flattenClipEnvelope(m_clipKey.key, value, completed)) {
        return;
    }

    reload();
}

void ClipEnvelopeModel::cancelDrag()
{
    if (!m_clipKey.isValid()) {
        return;
    }

    clipsInteraction()->endClipEnvelopePointDrag(m_clipKey.key, /*commit*/ false);

    m_dragActive = false;
    m_dragIndex = -1;
}
