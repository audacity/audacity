/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "uicomponents/view/abstractmenumodel.h"
#include "trackedit/trackedittypes.h"
#include "trackedit/iselectioncontroller.h"
#include "effects/effects_base/ieffectsprovider.h"
#include <QObject>

namespace au::projectscene {
class RealtimeEffectMenuModelBase : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT
    muse::Inject<trackedit::ISelectionController> selectionController;

public:
    explicit RealtimeEffectMenuModelBase(QObject* parent = nullptr);

    Q_INVOKABLE void load() final override;

protected:
    const std::optional<au::trackedit::TrackId>& trackId() const;
    void resetList();
    void removeTrack(const au::trackedit::TrackId& trackId);

    muse::Inject<effects::IEffectsProvider> effectsProvider;

private:
    void beginResetModel();
    void endResetModel();
    void setTrackId(std::optional<au::trackedit::TrackId>);

    virtual void doLoad() = 0;
    virtual void populateMenu() = 0;
    virtual void doResetList() {}
    virtual void doRemoveTrack(const au::trackedit::TrackId&) {}
    virtual void onTrackIdChanged() {}

    std::optional<au::trackedit::TrackId> m_trackId;
};
}
