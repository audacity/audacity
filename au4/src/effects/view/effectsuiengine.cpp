/*
* Audacity: A Digital Audio Editor
*/
#include "effectsuiengine.h"

#include <QQmlEngine>
#include <QQmlContext>

using namespace au::effects;

EffectsUiEngine::~EffectsUiEngine()
{
    delete m_engine;
}

void EffectsUiEngine::setup()
{
    //! NOTE Needed for UI components, should not be used directly in effects
    QObject* ui = dynamic_cast<QObject*>(uiEngine.get().get());
    m_engine->rootContext()->setContextProperty("ui", ui);

    m_engine->addImportPath(":/qml");
}

QQmlEngine* EffectsUiEngine::engine()
{
    if (!m_engine) {
        m_engine = new QQmlEngine(this);
        setup();
    }

    return m_engine;
}

QQmlEngine* EffectsUiEngine::qmlEngine() const
{
    return const_cast<EffectsUiEngine*>(this)->engine();
}
