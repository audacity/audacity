/*
* Audacity: A Digital Audio Editor
*/
#include "tracklabelslayoutmanager.h"

#include <algorithm>
#include <cmath>

#include <QAbstractListModel>
#include <QModelIndex>
#include <QVariant>

#include "tracklabelslistmodel.h"
#include "tracklabelitem.h"
#include "viewtrackitem.h"

#include "defer.h"

using namespace au::projectscene;
using namespace au::trackedit;

static const double COMPARE_EPS = 0.001;

TrackLabelsLayoutManager::TrackLabelsLayoutManager(QObject* parent)
    : QObject(parent)
{
}

TrackLabelsLayoutManager::~TrackLabelsLayoutManager()
{
}

void TrackLabelsLayoutManager::init()
{
    m_isInited = true;

    relayout();
    relink();
}

au::projectscene::LabelKey TrackLabelsLayoutManager::leftLinkedLabel(const LabelKey& key) const
{
    if (!m_leftLinkedLabels.contains(key)) {
        return LabelKey();
    }

    return m_leftLinkedLabels[key];
}

void TrackLabelsLayoutManager::activateLeftLinkedLabel(const LabelKey& key)
{
    if (!m_leftLinkedLabels.contains(key)) {
        return;
    }

    QList<LabelInfo> labels = collectLabelsInfo();
    const LabelKey& otherKey = m_leftLinkedLabels[key];
    for (const LabelInfo& label : labels) {
        if (label.key == otherKey) {
            label.item->setIsLinkedActive(true);
            break;
        }
    }
}

au::projectscene::LabelKey TrackLabelsLayoutManager::rightLinkedLabel(const LabelKey& key) const
{
    if (!m_rightLinkedLabels.contains(key)) {
        return LabelKey();
    }

    return m_rightLinkedLabels[key];
}

void TrackLabelsLayoutManager::activateRightLinkedLabel(const LabelKey& key)
{
    if (!m_rightLinkedLabels.contains(key)) {
        return;
    }

    QList<LabelInfo> labels = collectLabelsInfo();
    const LabelKey& otherKey = m_rightLinkedLabels[key];
    for (const LabelInfo& label : labels) {
        if (label.key == otherKey) {
            label.item->setIsLinkedActive(true);
            break;
        }
    }
}

void TrackLabelsLayoutManager::deactivateLinkedLabel(const LabelKey& key)
{
    QList<LabelInfo> labels = collectLabelsInfo();
    const LabelKey& otherLeftLinkedLabelKey = m_leftLinkedLabels[key];
    const LabelKey& otherRightLinkedLabelKey = m_rightLinkedLabels[key];
    for (const LabelInfo& label : labels) {
        if (label.key == otherLeftLinkedLabelKey) {
            label.item->setIsLinkedActive(false);
        } else if (label.key == otherRightLinkedLabelKey) {
            label.item->setIsLinkedActive(false);
        }
    }
}

QObject* TrackLabelsLayoutManager::labelsModel() const
{
    return m_labelsModel;
}

void TrackLabelsLayoutManager::setLabelsModel(QObject* model)
{
    TrackLabelsListModel* labelsListModel = qobject_cast<TrackLabelsListModel*>(model);
    if (m_labelsModel == labelsListModel) {
        return;
    }

    unsubscribeFromLabelsChanges();

    m_labelsModel = labelsListModel;
    emit labelsModelChanged();

    subscribeToLabelsChanges();
}

void TrackLabelsLayoutManager::subscribeToLabelsChanges()
{
    if (!m_labelsModel) {
        return;
    }

    unsubscribeFromLabelsChanges();

    QList<LabelInfo> labels = collectLabelsInfo();

    for (const LabelInfo& label : labels) {
        connect(label.item, &TrackLabelItem::startTimeChanged, this, &TrackLabelsLayoutManager::relayout);
        connect(label.item, &TrackLabelItem::endTimeChanged, this, &TrackLabelsLayoutManager::relayout);
        connect(label.item, &TrackLabelItem::visualWidthChanged, [this, labelKey = label.key]() {
            QList<LabelInfo> labels = collectLabelsInfo();
            for (const LabelInfo& label : labels) {
                if (label.key == labelKey && !label.isEditing) {
                    relayout();
                    break;
                }
            }
        });

        connect(label.item, &TrackLabelItem::isEditingChanged, this, &TrackLabelsLayoutManager::relink);
    }

    connect(m_labelsModel, &QAbstractListModel::rowsInserted, this, &TrackLabelsLayoutManager::subscribeToLabelsChanges);

    relayout();
}

void TrackLabelsLayoutManager::unsubscribeFromLabelsChanges()
{
    if (!m_labelsModel) {
        return;
    }

    QList<LabelInfo> labels = collectLabelsInfo();
    for (const LabelInfo& label : labels) {
        label.item->disconnect(this);
    }

    disconnect(m_labelsModel, &QAbstractListModel::rowsInserted, this, &TrackLabelsLayoutManager::subscribeToLabelsChanges);
}

QList<TrackLabelsLayoutManager::LabelInfo> TrackLabelsLayoutManager::collectLabelsInfo() const
{
    QList<LabelInfo> labels;

    if (!m_labelsModel) {
        return labels;
    }

    for (int i = 0; i < m_labelsModel->rowCount(QModelIndex()); ++i) {
        QModelIndex index = m_labelsModel->index(i);
        QVariant itemVariant = m_labelsModel->data(index, Qt::UserRole + 1);
        ViewTrackItem* viewItem = qvariant_cast<ViewTrackItem*>(itemVariant);
        TrackLabelItem* labelItem = qobject_cast<TrackLabelItem*>(viewItem);

        if (labelItem) {
            LabelInfo info;
            info.item = labelItem;
            info.key = labelItem->key();

            TrackItemTime time = labelItem->time();
            info.startTime = time.startTime;
            info.width = time.endTime - time.startTime;
            info.isPoint = std::abs(time.endTime - time.startTime) < COMPARE_EPS;

            info.x = labelItem->x();
            info.visualWidth = labelItem->visualWidth();
            info.level = labelItem->level();
            info.isEditing = labelItem->isEditing();

            labels << info;
        }
    }

    return labels;
}

void TrackLabelsLayoutManager::relayout()
{
    if (m_isBusy || !m_isInited) {
        return;
    }

    m_isBusy = true;
    DEFER {
        m_isBusy = false;
    };

    if (!m_labelsModel) {
        return;
    }

    QList<LabelInfo> labels = collectLabelsInfo();

    std::sort(labels.begin(), labels.end(), [](const LabelInfo& a, const LabelInfo& b) {
        if (a.x != b.x) {
            return a.x < b.x;
        }
        return a.visualWidth > b.visualWidth;
    });

    for (int i = 0; i < labels.size(); ++i) {
        LabelInfo& current = labels[i];

        current.level = 0;
        bool foundLevel = false;

        while (!foundLevel) {
            bool hasOverlap = false;
            int conflictingIndex = -1;

            for (int j = 0; j < i; ++j) {
                LabelInfo& other = labels[j];

                if (other.level == current.level) {
                    double currentRight = current.x + current.visualWidth;
                    double otherRight = other.x + other.visualWidth;
                    if (current.x < otherRight && other.x < currentRight) {
                        hasOverlap = true;
                        conflictingIndex = j;
                        break;
                    }
                }
            }

            if (!hasOverlap) {
                break;
            }

            if (conflictingIndex >= 0) {
                auto& conflictingLabel = labels[conflictingIndex];

                // If one is a point, the point should be on a higher level
                if (current.isPoint && !conflictingLabel.isPoint) {
                    current.level++;
                } else if (!current.isPoint && conflictingLabel.isPoint) {
                    conflictingLabel.level++;
                    conflictingLabel.item->setLevel(conflictingLabel.level);
                    continue;
                } else {
                    // Both are points or both are not points - compare by x
                    double conflictingLabelRight = conflictingLabel.startTime + conflictingLabel.width;
                    if (current.x < conflictingLabel.x) {
                        // current has smaller x, push conflicting to higher level
                        conflictingLabel.level++;
                        conflictingLabel.item->setLevel(conflictingLabel.level);
                        continue;
                    } else if (current.startTime == conflictingLabelRight) {
                        foundLevel = true;
                        continue;
                    } else {
                        current.level++;
                    }
                }
            } else {
                current.level++;
            }
        }

        current.item->setLevel(current.level);
    }
}

void TrackLabelsLayoutManager::relink()
{
    if (m_isBusy || !m_isInited) {
        return;
    }

    m_isBusy = true;
    DEFER {
        m_isBusy = false;
    };

    if (!m_labelsModel) {
        return;
    }

    m_leftLinkedLabels.clear();
    m_rightLinkedLabels.clear();

    QList<LabelInfo> labels = collectLabelsInfo();
    for (const LabelInfo& label : labels) {
        label.item->setIsLeftLinked(false);
        label.item->setIsRightLinked(false);
    }

    // Update linked labels
    // Label is linked if its right edge matches another label's left edge (or vice versa)
    for (int i = 0; i < labels.size(); ++i) {
        const LabelInfo& current = labels[i];

        if (current.isPoint) {
            continue;
        }

        double currentRightEdge = current.startTime + current.width;
        double currentLeftEdge = current.startTime;

        for (int j = 0; j < labels.size(); ++j) {
            if (i == j) {
                continue;
            }

            const LabelInfo& other = labels[j];

            double otherRightEdge = other.startTime + other.width;
            double otherLeftEdge = other.startTime;

            if (std::abs(currentRightEdge - otherLeftEdge) < COMPARE_EPS) {
                m_rightLinkedLabels[current.key] = other.key;
                current.item->setIsRightLinked(true);

                m_leftLinkedLabels[other.key] = current.key;
                other.item->setIsLeftLinked(true);
            }

            if (std::abs(currentLeftEdge - otherRightEdge) < COMPARE_EPS) {
                m_leftLinkedLabels[current.key] = other.key;
                current.item->setIsLeftLinked(true);

                m_rightLinkedLabels[other.key] = current.key;
                other.item->setIsRightLinked(true);
            }
        }
    }
}
