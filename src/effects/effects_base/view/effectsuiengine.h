/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "../ieffectsuiengine.h"

#include "modularity/ioc.h"
#include "ui/iuiengine.h"

namespace au::effects {
class QmlApiEngine;
class EffectsUiEngine : public QObject, public IEffectsUiEngine, public muse::Injectable
{
    Q_OBJECT

    muse::GlobalInject<muse::ui::IUiEngine> uiEngine;

public:
    EffectsUiEngine(const muse::modularity::ContextPtr& ctx)
        : muse::Injectable(ctx) {}
    ~EffectsUiEngine() override;

    QQmlEngine* qmlEngine() const override;

private:
    QQmlEngine* engine();
    void setup();

    QQmlEngine* m_engine = nullptr;
};
}
