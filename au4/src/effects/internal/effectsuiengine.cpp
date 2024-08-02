/*
* Audacity: A Digital Audio Editor
*/
#include "effectsuiengine.h"

#include <QQmlEngine>
#include <QQmlContext>

using namespace au::effects;

EffectsUiEngine::~EffectsUiEngine()
{
    delete m_translation;
    delete m_engine;
}

void EffectsUiEngine::setup()
{
    //! NOTE Needed for UI components, should not be used directly in effects
    QObject* ui = dynamic_cast<QObject*>(uiEngine.get().get());
    m_engine->rootContext()->setContextProperty("ui", ui);

    QJSValue translator = m_engine->newQObject(m_translation);
    QJSValue translateFn = translator.property("translate");
    QJSValue globalObj = m_engine->globalObject();
    m_engine->globalObject().setProperty("qsTrc", translateFn);

    m_engine->addImportPath(":/qml");
}

QQmlEngine* EffectsUiEngine::engine()
{
    if (!m_engine) {
        m_engine = new QQmlEngine(this);
        m_translation = new muse::ui::QmlTranslation(this);
        setup();
    }

    return m_engine;
}

QQmlEngine* EffectsUiEngine::qmlEngine() const
{
    return const_cast<EffectsUiEngine*>(this)->engine();
}
