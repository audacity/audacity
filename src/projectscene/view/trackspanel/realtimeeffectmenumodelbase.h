/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "uicomponents/view/abstractmenumodel.h"
#include "trackedit/trackedittypes.h"
#include "effects/effects_base/ieffectsprovider.h"
#include <QObject>

namespace au::projectscene {
class RealtimeEffectMenuModelBase : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT
    Q_PROPERTY(au::trackedit::TrackId trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged)

public:
    explicit RealtimeEffectMenuModelBase(QObject* parent = nullptr);

    au::trackedit::TrackId trackId() const;
    void setTrackId(au::trackedit::TrackId trackId);

    Q_INVOKABLE void load() final override;

signals:
    void trackIdChanged();

protected:
    muse::Inject<effects::IEffectsProvider> effectsProvider;
    std::optional<au::trackedit::TrackId> m_trackId;

private:
    virtual void doLoad() = 0;
    virtual void populateMenu() = 0;
};
}
