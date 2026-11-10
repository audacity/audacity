/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <QObject>
#include <QKeyEvent>
#include <QTimer>

#include "modularity/ioc.h"
#include "async/asyncable.h"
#include "rcommand/commandtypes.h"
#include "shortcuts_v2/icommandshortcutsregister.h"
#include "shortcuts_v2/ishortcutsresolver.h"

namespace au::projectscene {
class TapHoldShortcut : public QObject, public muse::Contextable, public muse::async::Asyncable
{
    Q_OBJECT

    muse::GlobalInject<muse::shortcuts::ICommandShortcutsRegister> commandShortcutsRegister;
    muse::ContextInject<muse::shortcuts::IShortcutsResolver> shortcutsResolver{ this };

public:
    explicit TapHoldShortcut(muse::modularity::ContextPtr ctx, const muse::rcommand::Command& command, QObject* target = nullptr);

    void setCommand(const muse::rcommand::Command& command);

    void setHoldDelay(int ms);

    muse::async::Notification pressed();
    muse::async::Notification released();
    muse::async::Notification tapped();
    muse::async::Notification holdStarted();
    muse::async::Notification holdEnded();

private:
    bool eventFilter(QObject* obj, QEvent* event) override;
    void onHoldTimeout();
    void updateShortcut();

    bool matchKeyEvent(QKeyEvent* ke) const;

    QTimer m_holdTimer;
    muse::rcommand::Command m_command;
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
