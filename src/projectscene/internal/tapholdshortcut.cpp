/*
* Audacity: A Digital Audio Editor
*/

#include "tapholdshortcut.h"

#include <QKeyEvent>

namespace au::projectscene {
TapHoldShortcut::TapHoldShortcut(muse::modularity::ContextPtr ctx, const muse::rcommand::Command& command, QObject* target)
    : QObject(nullptr), muse::Contextable(ctx), m_command(command)
{
    connect(&m_holdTimer, &QTimer::timeout, this, &TapHoldShortcut::onHoldTimeout);
    m_holdTimer.setSingleShot(true);

    commandShortcutsRegister()->shortcutsChanged().onNotify(this, [this]() {
        updateShortcut();
    });

    updateShortcut();

    if (target) {
        target->installEventFilter(this);
    } else {
        qApp->installEventFilter(this);
    }
}

void TapHoldShortcut::setCommand(const muse::rcommand::Command& command)
{
    m_command = command;
    updateShortcut();
}

void TapHoldShortcut::setHoldDelay(int ms) { m_holdDelay = ms; }

muse::async::Notification TapHoldShortcut::pressed()
{
    return m_pressed;
}

muse::async::Notification TapHoldShortcut::released()
{
    return m_released;
}

muse::async::Notification TapHoldShortcut::tapped()
{
    return m_tapped;
}

muse::async::Notification TapHoldShortcut::holdStarted()
{
    return m_holdStarted;
}

muse::async::Notification TapHoldShortcut::holdEnded()
{
    return m_holdEnded;
}

bool TapHoldShortcut::eventFilter(QObject* obj, QEvent* event)
{
    if (event->type() == QEvent::ShortcutOverride) {
        auto* ke = static_cast<QKeyEvent*>(event);
        if (matchKeyEvent(ke)) {
            ke->accept();
            return true;
        }
    }

    if (event->type() == QEvent::KeyPress) {
        auto* ke = static_cast<QKeyEvent*>(event);
        if (matchKeyEvent(ke) && !ke->isAutoRepeat()) {
            m_holding = true;
            m_holdActive = false;

            m_pressed.notify();
            m_holdTimer.start(m_holdDelay);
            return true;
        }
    }

    if (event->type() == QEvent::KeyRelease) {
        auto* ke = static_cast<QKeyEvent*>(event);
        if (matchKeyEvent(ke) && !ke->isAutoRepeat()) {
            m_holdTimer.stop();
            if (m_holding && !m_holdActive) {
                m_tapped.notify();
            } else if (m_holdActive) {
                m_holdEnded.notify();
            }
            m_released.notify();
            m_holding = false;
            m_holdActive = false;
            return true;
        }
    }

    return QObject::eventFilter(obj, event);
}

void TapHoldShortcut::onHoldTimeout()
{
    if (m_holding && !m_holdActive) {
        m_holdActive = true;
        m_holdStarted.notify();
    }
}

void TapHoldShortcut::updateShortcut()
{
    m_shortcut = commandShortcutsRegister()->shortcut(m_command);
}

bool TapHoldShortcut::matchKeyEvent(QKeyEvent* ke) const
{
    if (!m_shortcut.isValid() || m_shortcut.sequences.empty()
        || !shortcutsResolver()->selectActive({ m_shortcut }).isValid()) {
        return false;
    }

    const QKeySequence ks(QString::fromStdString(m_shortcut.sequences.front()));
    const QKeyCombination evComb(ke->modifiers(), Qt::Key(ke->key()));
    const QKeyCombination targetComb(ks[0]);

    return evComb == targetComb;
}
}
