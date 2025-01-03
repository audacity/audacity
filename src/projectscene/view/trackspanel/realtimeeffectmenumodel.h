/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "modularity/ioc.h"
#include "realtimeeffectmenumodelbase.h"

namespace au::projectscene {
class RealtimeEffectMenuModel : public RealtimeEffectMenuModelBase
{
    Q_OBJECT

public:
    explicit RealtimeEffectMenuModel(QObject* parent = nullptr);

private:
    void doLoad() override;
    void populateMenu() override;
};
}  // namespace au::projectscene
