/*
* Audacity: A Digital Audio Editor
*/

#include "tapholdshortcut.h"

#include <QKeyEvent>

namespace au::projectscene {
TapHoldShortcut::TapHoldShortcut(const std::string& action, QObject* target, QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this)), m_action(action)
{
    connect(&m_holdTimer, &QTimer::timeout, this, &TapHoldShortcut::onHoldTimeout);
    m_holdTimer.setSingleShot(true);

    shortcutsRegister()->shortcutsChanged().onNotify(this, [this]() {
        updateShortcut(m_action);
    });

    updateShortcut(m_action);

    if (target) {
        target->installEventFilter(this);
    } else {
        qApp->installEventFilter(this);
    }
}

void TapHoldShortcut::setAction(const std::string& action)
{
    m_action = action;
    updateShortcut(m_action);
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

void TapHoldShortcut::updateShortcut(const std::string& action)
{
    m_shortcut = shortcutsRegister()->shortcut(action);
}

bool TapHoldShortcut::matchKeyEvent(QKeyEvent* ke) const
{
    if (!uicontextResolver()->isShortcutContextAllowed(m_shortcut.context) || !m_shortcut.isValid()) {
        return false;
    }

    const QKeySequence ks(QString::fromStdString(m_shortcut.sequences.front()));
    const QKeyCombination evComb(ke->modifiers(), Qt::Key(ke->key()));
    const QKeyCombination targetComb(ks[0]);

    return evComb == targetComb;
}
}
