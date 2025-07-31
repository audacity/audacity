/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/abstracteffectmodel.h"
#include "../common/params.h"

#include "effects/effects_base/ieffectsprovider.h"

namespace au::effects {
class GraphicEq;
class GraphicEqBandsModel;
class GraphicEqViewModel : public AbstractEffectModel
{
    Q_OBJECT
    Q_PROPERTY(GraphicEqBandsModel * bandsModel READ bandsModel NOTIFY bandsModelChanged FINAL)
    Q_PROPERTY(double minDbGain READ minDbGain CONSTANT FINAL)
    Q_PROPERTY(double maxDbGain READ maxDbGain CONSTANT FINAL)

    muse::Inject<IEffectsProvider> effectsProvider;

public:
    GraphicEqViewModel();

    GraphicEqBandsModel* bandsModel() const;
    double minDbGain() const;
    double maxDbGain() const;

signals:
    void bandsModelChanged();

private:
    GraphicEq* effect() const;
    void doReload() override;

    GraphicEqBandsModel* const mBandsModel;
};
}
