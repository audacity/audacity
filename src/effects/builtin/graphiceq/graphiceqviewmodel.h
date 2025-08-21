/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/builtineffectmodel.h"
#include "../common/params.h"

namespace au::effects {
class GraphicEq;
class GraphicEqBandsModel;
class GraphicEqViewModel : public BuiltinEffectModel
{
    Q_OBJECT
    Q_PROPERTY(GraphicEqBandsModel * bandsModel READ bandsModel NOTIFY bandsModelChanged FINAL)
    Q_PROPERTY(double minDbGain READ minDbGain CONSTANT FINAL)
    Q_PROPERTY(double maxDbGain READ maxDbGain CONSTANT FINAL)

public:
    GraphicEqViewModel();

    GraphicEqBandsModel* bandsModel() const;
    double minDbGain() const;
    double maxDbGain() const;

signals:
    void bandsModelChanged();

private:
    void doReload() override;

    GraphicEqBandsModel* const mBandsModel;
};
}
