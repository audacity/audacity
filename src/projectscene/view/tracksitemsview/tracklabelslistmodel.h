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

    Q_INVOKABLE void toggleTracksDataSelectionByLabel(const LabelKey& key);

    Q_INVOKABLE bool moveSelectedLabels(const LabelKey& key, bool completed);
    Q_INVOKABLE bool stretchLabelLeft(const LabelKey& key, const LabelKey& leftLinkedLabel, bool unlink, bool completed);
    Q_INVOKABLE bool stretchLabelRight(const LabelKey& key, const LabelKey& rightLinkedLabel, bool unlink, bool completed);

    void startEditItem(const TrackItemKey& key) override;
    void endEditItem(const TrackItemKey& key) override;

private:
    friend class TrackLabelsLayoutManagerTests;

    void onInit() override;
    void onReload() override;

    void update();
    void updateItemMetrics(ViewTrackItem* item) override;
    trackedit::TrackItemKeyList getSelectedItemKeys() const override;

    TrackLabelItem* labelItemByKey(const trackedit::LabelKey& k) const;

    void selectTracksDataFromLabelRange(const LabelKey& key);
    void doSelectTracksData(const LabelKey& key);
    bool isTrackDataSelected() const;

    void resetSelectedTracksData();

    muse::async::NotifyList<au::trackedit::Label> m_allLabelList;
    bool m_needToSelectTracksData = false;

    //! Shift+press on an already selected label must not deselect it right away: the press
    //! may be the start of a group drag, and the move offset is anchored to the pressed
    //! label, so dropping it from the selection makes the rest of the group run away.
    //! The deselection is deferred here and applied only when the gesture turns out to be
    //! a click (release without movement).
    trackedit::TrackItemKey m_pendingShiftDeselect;
};
}
