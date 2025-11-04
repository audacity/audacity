/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>
#include <QMap>
#include <QList>

#include "projectscene/types/projectscenetypes.h"

namespace au::projectscene {
class TrackLabelsListModel;
class TrackLabelItem;

class TrackLabelsLayoutManager : public QObject
{
    Q_OBJECT

    Q_PROPERTY(QObject * labelsModel READ labelsModel WRITE setLabelsModel NOTIFY labelsModelChanged)

public:
    explicit TrackLabelsLayoutManager(QObject* parent = nullptr);
    ~TrackLabelsLayoutManager();

    Q_INVOKABLE void init();

    Q_INVOKABLE LabelKey leftLinkedLabel(const LabelKey& key) const;
    Q_INVOKABLE void activateLeftLinkedLabel(const LabelKey& key);

    Q_INVOKABLE LabelKey rightLinkedLabel(const LabelKey& key) const;
    Q_INVOKABLE void activateRightLinkedLabel(const LabelKey& key);

    Q_INVOKABLE void deactivateLinkedLabel(const LabelKey& key);

    QObject* labelsModel() const;
    void setLabelsModel(QObject* model);

signals:
    void labelsModelChanged();

private:
    friend class TrackLabelsLayoutManagerTests;

    struct LabelInfo {
        TrackLabelItem* item = nullptr;
        LabelKey key;
        double startTime = 0;
        double width = 0;
        double x = 0;
        double visualWidth = 0;
        int level = 0;
        bool isEditing = false;
        bool isPoint = false;
    };

    void subscribeToLabelsChanges();
    void unsubscribeFromLabelsChanges();

    void relayout();
    void relink();

    QList<LabelInfo> collectLabelsInfo() const;

    TrackLabelsListModel* m_labelsModel = nullptr;

    bool m_isInited = false;
    bool m_isBusy = false;

    QMap<LabelKey, LabelKey> m_leftLinkedLabels;
    QMap<LabelKey, LabelKey> m_rightLinkedLabels;
};
}
