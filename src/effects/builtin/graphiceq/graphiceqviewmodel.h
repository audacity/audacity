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

    muse::Inject<IEffectsProvider> effectsProvider;

public:
    GraphicEqViewModel();

    GraphicEqBandsModel* bandsModel() const;

signals:
    void bandsModelChanged();

private:
    GraphicEq* effect() const;
    void doReload() override;

    GraphicEqBandsModel* const mBandsModel;
};
}
