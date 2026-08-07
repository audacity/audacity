/*
 * Audacity: A Digital Audio Editor
 */
#include "clipgainmodel.h"

#include "log.h"

using namespace au::automation;
using namespace au::projectscene;

ClipGainModel::ClipGainModel(QObject* parent)
    : QAbstractListModel(parent)
    , muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void ClipGainModel::init()
{
    projectHistory()->historyChanged().onReceive(this, [this](auto) {
        if (m_clipKey.isValid()) {
            reload();
        }
    });

    clipsInteraction()->clipStartTimeChanged().onReceive(
        this,
        [this](const ClipKey& key, muse::secs_t /*newStart*/, bool /*completed*/) {
        if (m_clipKey.isValid() && key == m_clipKey) {
            reload();
        }
    });

    clipGainInteraction()->clipGainChanged().onReceive(
        this,
        [this](const ClipKey& key, bool /*completed*/) {
        if (m_clipKey.isValid() && key == m_clipKey) {
            reload();
        }
    });

    reload();
}

QVariant ClipGainModel::data(const QModelIndex& index, int role) const
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
        return p.xValue;
    case ValueRole:
        return p.yValue;
    default:
        return {};
    }
}

QHash<int, QByteArray> ClipGainModel::roleNames() const
{
    return {
        { TimeRole, "time" },
        { ValueRole, "value" },
    };
}

QVector<QPointF> ClipGainModel::points() const
{
    QVector<QPointF> out;
    out.reserve(int(m_points.size()));
    for (const auto& point : m_points) {
        out.push_back(QPointF(point.xValue, point.yValue));
    }
    return out;
}

double ClipGainModel::minValue() const
{
    return m_info.minValue;
}

double ClipGainModel::maxValue() const
{
    return m_info.maxValue;
}

double ClipGainModel::defaultValue() const
{
    return m_info.defaultValue;
}

bool ClipGainModel::exponential() const
{
    return m_info.exponential;
}

double ClipGainModel::ySplitNormalized() const
{
    return 0.7;
}

double ClipGainModel::ySplitValue() const
{
    return 1.0;
}

double ClipGainModel::clipStartTime() const
{
    return m_clipStartTime;
}

double ClipGainModel::clipEndTime() const
{
    return m_clipEndTime;
}

int ClipGainModel::rowCount(const QModelIndex& parent) const
{
    return parent.isValid() ? 0 : int(m_points.size());
}

au::projectscene::ClipKey ClipGainModel::clipKey() const
{
    return m_clipKey;
}

void ClipGainModel::setClipKey(const ClipKey& key)
{
    if (m_clipKey == key) {
        return;
    }

    m_clipKey = key;

    if (m_clipKey.isValid()) {
        if (auto prj = globalContext()->currentTrackeditProject()) {
            prj->clipList(m_clipKey.key.trackId).onItemRemoved(this, [this](const trackedit::Clip& clip) {
                if (clip.key == m_clipKey.key) {
                    clear();
                    m_clipKey = ClipKey {};
                }
            }, muse::async::Asyncable::Mode::SetReplace);
        }
    }

    reload();
}

void ClipGainModel::reload()
{
    if (!m_clipKey.isValid()) {
        clear();
        return;
    }

    m_clipStartTime = clipsInteraction()->clipStartTime(m_clipKey.key);
    m_clipEndTime   = clipsInteraction()->clipEndTime(m_clipKey.key);

    // Track/clip may have been deleted (e.g. after track removal + history notify)
    if (m_clipStartTime < 0 || m_clipEndTime < 0) {
        clear();
        return;
    }

    emit clipTimeChanged();

    auto points = clipGainInteraction()->clipGainPoints(m_clipKey.key);
    auto infoOpt = clipGainInteraction()->clipGainInfo(m_clipKey.key);
    if (infoOpt) {
        m_info = *infoOpt;
    } else {
        m_info = {};
    }

    beginResetModel();
    m_points = std::move(points);
    endResetModel();

    emit pointsChanged();
    emit clipGainAutomationInfoChanged();
}

void ClipGainModel::clear()
{
    if (m_points.empty()) {
        return;
    }
    beginResetModel();
    m_points.clear();
    endResetModel();

    emit pointsChanged();
}

void ClipGainModel::setPoint(int index, double tAbs, double value, bool completed)
{
    if (!m_clipKey.isValid()) {
        return;
    }
    if (index < 0) {
        return;
    }
    if (!m_dragActive && index >= int(m_points.size())) {
        return;
    }

    const double clampedTime = std::clamp(tAbs, m_clipStartTime, m_clipEndTime);

    if (!m_dragActive || m_dragIndex != index) {
        // if we somehow had a different drag active, end it
        if (m_dragActive) {
            clipGainInteraction()->endClipGainPointDrag(m_clipKey.key, /*commit*/ true);
        }

        if (!clipGainInteraction()->beginClipGainPointDrag(m_clipKey.key, index)) {
            m_dragActive = false;
            m_dragIndex = -1;
            return;
        }

        m_dragActive = true;
        m_dragIndex = index;
    }

    if (!clipGainInteraction()->updateClipGainPointDrag(m_clipKey.key, clampedTime, value)) {
        return;
    }

    if (index < int(m_points.size())) {
        auto& p = m_points[size_t(index)];
        p.xValue = clampedTime;
        p.yValue = value;
        const QModelIndex idx = this->index(index, 0);
        emit dataChanged(idx, idx, { TimeRole, ValueRole });
    }

    if (completed) {
        clipGainInteraction()->endClipGainPointDrag(m_clipKey.key, /*commit*/ true);

        m_dragActive = false;
        m_dragIndex = -1;

        reload();
    }
}

void ClipGainModel::addPoint(double tAbs, double value, bool completed)
{
    if (!m_clipKey.isValid()) {
        return;
    }

    const double clampedTime = std::clamp(tAbs, m_clipStartTime, m_clipEndTime);

    if (!clipGainInteraction()->setClipGainPoint(m_clipKey.key, clampedTime, value, completed)) {
        return;
    }

    reload();
}

void ClipGainModel::removePoint(int index, bool completed)
{
    if (!m_clipKey.isValid()) {
        return;
    }
    if (index < 0 || index >= int(m_points.size())) {
        return;
    }

    if (!clipGainInteraction()->removeClipGainPoint(m_clipKey.key, index, completed)) {
        return;
    }

    reload();
}

void ClipGainModel::cancelDrag()
{
    if (!m_clipKey.isValid()) {
        return;
    }

    clipGainInteraction()->endClipGainPointDrag(m_clipKey.key, /*commit*/ false);

    m_dragActive = false;
    m_dragIndex = -1;
}
