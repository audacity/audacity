/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <QObject>
#include <QKeyEvent>
#include <QTimer>

#include "modularity/ioc.h"
#include "async/asyncable.h"
#include "context/iuicontextresolver.h"
#include "shortcuts/ishortcutsregister.h"

namespace au::projectscene {
class TapHoldShortcut : public QObject, public muse::Injectable, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<muse::shortcuts::IShortcutsRegister> shortcutsRegister{ this };
    muse::Inject<context::IUiContextResolver> uicontextResolver{ this };

public:
    explicit TapHoldShortcut(muse::modularity::ContextPtr ctx, const std::string& action, QObject* target = nullptr);

    void setAction(const std::string& action);

    void setHoldDelay(int ms);

    muse::async::Notification pressed();
    muse::async::Notification released();
    muse::async::Notification tapped();
    muse::async::Notification holdStarted();
    muse::async::Notification holdEnded();

private:
    bool eventFilter(QObject* obj, QEvent* event) override;
    void onHoldTimeout();
    void updateShortcut(const std::string& action);

    bool matchKeyEvent(QKeyEvent* ke) const;

    QTimer m_holdTimer;
    std::string m_action;
    muse::shortcuts::Shortcut m_shortcut;

    int m_holdDelay = 250;
    bool m_holding = false;
    bool m_holdActive = false;

    muse::async::Notification m_pressed;
    muse::async::Notification m_released;
    muse::async::Notification m_holdStarted;
    muse::async::Notification m_holdEnded;
    muse::async::Notification m_tapped;
};
}
