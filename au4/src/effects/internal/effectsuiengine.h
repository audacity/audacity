/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "effects/ieffectsuiengine.h"

#include "modularity/ioc.h"
#include "ui/iuiengine.h"

#include "ui/view/qmltranslation.h"

namespace au::effects {
class QmlApiEngine;
class EffectsUiEngine : public QObject, public IEffectsUiEngine
{
    Q_OBJECT

    muse::Inject<muse::ui::IUiEngine> uiEngine;

public:
    ~EffectsUiEngine() override;

    QQmlEngine* qmlEngine() const override;

private:
    QQmlEngine* engine();
    void setup();

    QQmlEngine* m_engine = nullptr;
    muse::ui::QmlTranslation* m_translation = nullptr;
};
}
