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
    void handleMenuItem(const QString& itemId) override;

    void doLoad() override;
    void populateMenu() override;
};
}
