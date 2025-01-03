/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>
#include "effects/effects_base/ieffectsprovider.h"
#include "trackedit/trackedittypes.h"
#include "uicomponents/view/abstractmenumodel.h"

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
    au::trackedit::TrackId m_trackId = -1;

private:
    virtual void doLoad() = 0;
    virtual void populateMenu() = 0;
};
}  // namespace au::projectscene
