/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "trackitemslistmodel.h"
#include "tracklabelitem.h"

namespace au::projectscene {
class TrackLabelsListModel : public TrackItemsListModel
{
    Q_OBJECT

public:
    explicit TrackLabelsListModel(QObject* parent = nullptr);

    Q_INVOKABLE void selectLabel(const LabelKey& key);
    Q_INVOKABLE void resetSelectedLabels();
    Q_INVOKABLE bool changeLabelTitle(const LabelKey& key, const QString& newTitle);

    Q_INVOKABLE bool moveSelectedLabels(const LabelKey& key, bool completed);
    Q_INVOKABLE bool stretchLabelLeft(const LabelKey& key, bool completed);
    Q_INVOKABLE bool stretchLabelRight(const LabelKey& key, bool completed);

private:

    void onInit() override;
    void onReload() override;

    void update();
    void updateItemMetrics(ViewTrackItem* item) override;
    trackedit::TrackItemKeyList getSelectedItemKeys() const override;
    TrackLabelItem* labelItemByKey(const trackedit::LabelKey& k) const;

    muse::async::NotifyList<au::trackedit::Label> m_allLabelList;
};
}
