/*
 * Audacity: A Digital Audio Editor
 */
#include "guifocusstate.h"

#include <QApplication>
#include <QObject>

namespace au::appshell {
void GuiFocusState::init()
{
    QObject::connect(qApp, &QApplication::applicationStateChanged, [this](Qt::ApplicationState state){
        m_isFocusedChanged.send(state == Qt::ApplicationActive);
    });
}

bool GuiFocusState::isFocused() const
{
    return qApp->applicationState() == Qt::ApplicationActive;
}

muse::async::Channel<bool> GuiFocusState::isFocusedChanged() const
{
    return m_isFocusedChanged;
}
}
