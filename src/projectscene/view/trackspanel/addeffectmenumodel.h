/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "modularity/ioc.h"
#include "realtimeeffectmenumodelbase.h"

namespace au::projectscene {
class AddEffectMenuModel : public RealtimeEffectMenuModelBase
{
    Q_OBJECT

public:
    explicit AddEffectMenuModel(QObject* parent = nullptr);

private:
    void handleMenuItem(const QString& itemId) override;

    void doLoad() override;
    void doPopulateMenu() override;
};
}
