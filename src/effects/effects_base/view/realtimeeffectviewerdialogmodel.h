/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "modularity/ioc.h"
#include "ieffectinstancesregister.h"
#include "ieffectsprovider.h"
#include "effectstypes.h"

#include <QObject>

namespace au::effects {
class RealtimeEffectViewerDialogModel : public QObject, public muse::Injectable
{
    Q_OBJECT
    Q_PROPERTY(QString effectState READ prop_effectState WRITE prop_setEffectState FINAL)

    muse::Inject<IEffectInstancesRegister> instancesRegister;
    muse::Inject<IEffectsProvider> effectsProvider;

public:
    RealtimeEffectViewerDialogModel(QObject* parent = nullptr);
    ~RealtimeEffectViewerDialogModel() override;

    QString prop_effectState() const;
    void prop_setEffectState(const QString& effectState);

private:
    void unregisterState();

    RealtimeEffectStatePtr m_effectState;
};
}
