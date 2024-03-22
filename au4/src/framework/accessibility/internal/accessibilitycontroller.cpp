/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include "accessibilitycontroller.h"

#include <QGuiApplication>
#include <QAccessible>
#include <QWindow>
#include <QTimer>

#ifdef Q_OS_LINUX
#include <QKeyEvent>
#include <private/qcoreapplication_p.h>
#endif

#include "accessibleobject.h"
#include "accessiblestub.h"
#include "accessibleiteminterface.h"

#include "log.h"

#ifdef MUE_ENABLE_ACCESSIBILITY_TRACE
#define MYLOG() LOGI()
#else
#define MYLOG() LOGN()
#endif

using namespace mu::accessibility;

AccessibleObject* s_rootObject = nullptr;
std::shared_ptr<IQAccessibleInterfaceRegister> accessibleInterfaceRegister = nullptr;

static void updateHandlerNoop(QAccessibleEvent*)
{
}

AccessibilityController::~AccessibilityController()
{
    unreg(this);
}

QAccessibleInterface* AccessibilityController::accessibleInterface(QObject*)
{
    return static_cast<QAccessibleInterface*>(new AccessibleItemInterface(s_rootObject));
}

static QAccessibleInterface* muAccessibleFactory(const QString& classname, QObject* object)
{
    if (!accessibleInterfaceRegister) {
        accessibleInterfaceRegister = mu::modularity::ioc()->resolve<IQAccessibleInterfaceRegister>("accessibility");
    }

    auto interfaceGetter = accessibleInterfaceRegister->interfaceGetter(classname);
    if (interfaceGetter) {
        return interfaceGetter(object);
    }

    return AccessibleStub::accessibleInterface(object);
}

void AccessibilityController::init()
{
    QAccessible::installFactory(muAccessibleFactory);

    reg(this);
    const Item& self = findItem(this);
    s_rootObject = self.object;

    QAccessible::installRootObjectHandler(nullptr);
    QAccessible::setRootObject(s_rootObject);
}

void AccessibilityController::reg(IAccessible* item)
{
    if (!m_inited) {
        m_inited = true;
        init();
    }

    if (findItem(item).isValid()) {
        LOGW() << "Already registered";
        return;
    }

    MYLOG() << "item: " << item->accessibleName();

    Item it;
    it.item = item;
    it.object = new AccessibleObject(item);
    it.object->setController(weak_from_this());
    it.iface = QAccessible::queryAccessibleInterface(it.object);

    m_allItems.insert(item, it);

    if (item->accessibleParent() == this) {
        m_children.append(item);
    }

    item->accessiblePropertyChanged().onReceive(this, [this, item](const IAccessible::Property& p, const Val value) {
        propertyChanged(item, p, value);
    });

    item->accessibleStateChanged().onReceive(this, [this, item](const State& state, bool arg) {
        stateChanged(item, state, arg);
    });

    QAccessibleEvent ev(it.object, QAccessible::ObjectCreated);
    sendEvent(&ev);
}

void AccessibilityController::unreg(IAccessible* aitem)
{
    MYLOG() << aitem->accessibleName();

    Item item = m_allItems.take(aitem);
    if (!item.isValid()) {
        return;
    }

    if (m_lastFocused == item.item) {
        m_lastFocused = nullptr;
    }

    if (m_itemForRestoreFocus == item.item) {
        m_itemForRestoreFocus = nullptr;
    }

    if (m_children.contains(aitem)) {
        m_children.removeOne(aitem);
    }

    QAccessibleEvent ev(item.object, QAccessible::ObjectDestroyed);
    sendEvent(&ev);

    delete item.object;
}

const IAccessible* AccessibilityController::accessibleRoot() const
{
    return this;
}

const IAccessible* AccessibilityController::lastFocused() const
{
    return m_lastFocused;
}

bool AccessibilityController::needToVoicePanelInfo() const
{
    return m_needToVoicePanelInfo;
}

QString AccessibilityController::currentPanelAccessibleName() const
{
    const IAccessible* focusedItemPanel = panel(m_lastFocused);
    return focusedItemPanel ? focusedItemPanel->accessibleName() : "";
}

void AccessibilityController::setIgnoreQtAccessibilityEvents(bool ignore)
{
    if (ignore) {
        QAccessible::installUpdateHandler(updateHandlerNoop);
    } else {
        QAccessible::installUpdateHandler(nullptr);
    }
}

void AccessibilityController::propertyChanged(IAccessible* item, IAccessible::Property property, const Val& value)
{
    const Item& it = findItem(item);
    if (!it.isValid()) {
        return;
    }

    QAccessible::Event etype = QAccessible::InvalidEvent;
    switch (property) {
    case IAccessible::Property::Undefined:
        return;
    case IAccessible::Property::Parent: etype = QAccessible::ParentChanged;
        break;
    case IAccessible::Property::Name: {
#ifdef Q_OS_MAC
        m_needToVoicePanelInfo = false;
        etype = QAccessible::NameChanged;
        break;
#else
        triggerRevoicingOfChangedName(item);
        return;
#endif
    }
    case IAccessible::Property::Description: etype = QAccessible::DescriptionChanged;
        break;
    case IAccessible::Property::Value: {
        QAccessibleValueChangeEvent ev(it.object, it.item->accessibleValue());
        sendEvent(&ev);
        return;
    }
    case IAccessible::Property::TextCursor: {
        QAccessibleTextCursorEvent ev(it.object, it.item->accessibleCursorPosition());
        sendEvent(&ev);
        return;
    }
    case IAccessible::Property::TextInsert: {
        IAccessible::TextRange range(value.toQVariant().toMap());
        QAccessibleTextInsertEvent ev(it.object, range.startPosition,
                                      it.item->accessibleText(range.startPosition, range.endPosition));
        sendEvent(&ev);
        return;
    }
    case IAccessible::Property::TextRemove: {
        IAccessible::TextRange range(value.toQVariant().toMap());
        QAccessibleTextRemoveEvent ev(it.object, range.startPosition,
                                      it.item->accessibleText(range.startPosition, range.endPosition));
        sendEvent(&ev);
        return;
    }
    }

    QAccessibleEvent ev(it.object, etype);
    sendEvent(&ev);
}

void AccessibilityController::stateChanged(IAccessible* aitem, State state, bool arg)
{
    if (!configuration()->enabled()) {
        return;
    }

    MYLOG() << aitem->accessibleName() << ", state: " << int(state) << ", arg: " << arg;
    const Item& item = findItem(aitem);
    IF_ASSERT_FAILED(item.isValid()) {
        return;
    }

    if (!item.item->accessibleParent()) {
        LOGE() << "for item: " << aitem->accessibleName() << " parent is null";
        return;
    }

    QAccessible::State qstate;
    switch (state) {
    case State::Enabled: {
        qstate.disabled = true;
    } break;
    case State::Active: {
        qstate.active = true;
    } break;
    case State::Focused: {
        qstate.focused = true;
    } break;
    case State::Selected: {
        qstate.selected = true;
    } break;
    case State::Checked: {
        qstate.checked = true;
    } break;
    default: {
        LOGE() << "not handled state: " << int(state);
        return;
    }
    }

    QAccessibleStateChangeEvent ev(item.object, qstate);
    sendEvent(&ev);

    if (state == State::Focused) {
        if (arg) {
            cancelPreviousReading();
            savePanelAccessibleName(m_lastFocused, item.item);

            QAccessibleEvent ev2(item.object, QAccessible::Focus);
            sendEvent(&ev2);
            m_lastFocused = item.item;
        }
    }
}

void AccessibilityController::sendEvent(QAccessibleEvent* ev)
{
#ifdef MUE_ENABLE_ACCESSIBILITY_TRACE
    AccessibleObject* obj = qobject_cast<AccessibleObject*>(ev->object());
    MYLOG() << "object: " << obj->item()->accessibleName() << ", event: " << int(ev->type());
#endif

    QAccessible::updateAccessibility(ev);

    m_eventSent.send(ev);
}

void AccessibilityController::cancelPreviousReading()
{
    if (!configuration()->active()) {
        return;
    }

#ifdef Q_OS_LINUX
    //! HACK: it needs for canceling reading the name of previous control on accessibility
    QKeyEvent* keyEvent = new QKeyEvent(QEvent::KeyPress, Qt::Key_Cancel, Qt::KeyboardModifier::NoModifier, 0, 1, 0);
    QCoreApplicationPrivate::setEventSpontaneous(keyEvent, true);
    application()->notify(mainWindow()->qWindow(), keyEvent);
#endif
}

void AccessibilityController::savePanelAccessibleName(const IAccessible* oldItem, const IAccessible* newItem)
{
    if (m_ignorePanelChangingVoice) {
        m_needToVoicePanelInfo = false;
        return;
    }

    const IAccessible* oldItemPanel = panel(oldItem);
    QString oldItemPanelName = oldItemPanel ? oldItemPanel->accessibleName() : "";

    const IAccessible* newItemPanel = panel(newItem);
    QString newItemPanelName = newItemPanel ? newItemPanel->accessibleName() : "";

    m_needToVoicePanelInfo = oldItemPanelName != newItemPanelName;
}

#ifndef Q_OS_MAC

void AccessibilityController::triggerRevoicingOfChangedName(IAccessible* item)
{
    if (!configuration()->active()) {
        return;
    }

    if (m_lastFocused != item) {
        return;
    }

    const IAccessible* itemPanel = panel(item);
    if (!itemPanel) {
        return;
    }

    m_ignorePanelChangingVoice = true;

    item->setState(State::Focused, false);

    IAccessible* tmpFocusedItem = findSiblingItem(itemPanel, item);
    if (!tmpFocusedItem) {
        tmpFocusedItem = const_cast<IAccessible*>(itemPanel);
    }

    tmpFocusedItem->setState(State::Focused, true);
    m_itemForRestoreFocus = item;

    //! NOTE: Restore the focused element after some delay(this value was found experimentally)
    QTimer::singleShot(100, [=]() {
        if (m_lastFocused) {
            m_lastFocused->setState(State::Focused, false);
        }

        if (m_itemForRestoreFocus) {
            m_itemForRestoreFocus->setState(State::Focused, true);
        }

        m_ignorePanelChangingVoice = false;
    });
}

#endif

const IAccessible* AccessibilityController::panel(const IAccessible* item) const
{
    if (!item) {
        return nullptr;
    }

    const IAccessible* parent = item->accessibleParent();
    while (parent) {
        if (parent->accessibleRole() == IAccessible::Panel) {
            return parent;
        }

        parent = parent->accessibleParent();
    }

    return nullptr;
}

IAccessible* AccessibilityController::findSiblingItem(const IAccessible* item, const IAccessible* currentItem) const
{
    TRACEFUNC;
    size_t count = item->accessibleChildCount();
    for (size_t i = 0; i < count; ++i) {
        const IAccessible* ch = item->accessibleChild(i);
        const Item& chIt = findItem(ch);
        if (!chIt.isValid() || !chIt.iface || chIt.item->accessibleIgnored() || ch == currentItem) {
            continue;
        }

        if (!chIt.iface->state().invisible && ch->accessibleRole() != IAccessible::Group
            && ch->accessibleRole() != IAccessible::Panel) {
            return chIt.item;
        }

        if (chIt.item->accessibleChildCount() > 0) {
            IAccessible* subItem = findSiblingItem(chIt.item, currentItem);
            if (subItem) {
                return subItem;
            }
        }
    }

    return nullptr;
}

mu::async::Channel<QAccessibleEvent*> AccessibilityController::eventSent() const
{
    return m_eventSent;
}

const AccessibilityController::Item& AccessibilityController::findItem(const IAccessible* aitem) const
{
    auto it = m_allItems.find(aitem);
    if (it != m_allItems.end()) {
        return it.value();
    }

    static AccessibilityController::Item null;
    return null;
}

QAccessibleInterface* AccessibilityController::parentIface(const IAccessible* item) const
{
    IF_ASSERT_FAILED(item) {
        return nullptr;
    }

    const IAccessible* parent = item->accessibleParent();
    if (!parent) {
        return nullptr;
    }

    const Item& it = findItem(parent);
    if (!it.isValid()) {
        return nullptr;
    }

    if (it.item->accessibleRole() == IAccessible::Role::Application) {
        if (!qApp->isQuitLockEnabled()) {
            return QAccessible::queryAccessibleInterface(interactiveProvider()->topWindow());
        } else {
            return QAccessible::queryAccessibleInterface(qApp->focusWindow());
        }
    }

    return it.iface;
}

int AccessibilityController::childCount(const IAccessible* item) const
{
    IF_ASSERT_FAILED(item) {
        return 0;
    }

    const Item& it = findItem(item);
    IF_ASSERT_FAILED(it.isValid()) {
        return 0;
    }
    return static_cast<int>(it.item->accessibleChildCount());
}

QAccessibleInterface* AccessibilityController::child(const IAccessible* item, int i) const
{
    IF_ASSERT_FAILED(item) {
        return nullptr;
    }

    const IAccessible* chld = item->accessibleChild(static_cast<size_t>(i));
    IF_ASSERT_FAILED(chld) {
        return nullptr;
    }

    const Item& chldIt = findItem(chld);
    IF_ASSERT_FAILED(chldIt.isValid()) {
        return nullptr;
    }

    return chldIt.iface;
}

int AccessibilityController::indexOfChild(const IAccessible* item, const QAccessibleInterface* iface) const
{
    TRACEFUNC;
    size_t count = item->accessibleChildCount();
    for (size_t i = 0; i < count; ++i) {
        const IAccessible* ch = item->accessibleChild(i);
        const Item& chIt = findItem(ch);
        IF_ASSERT_FAILED(chIt.isValid()) {
            continue;
        }

        if (chIt.iface == iface) {
            return static_cast<int>(i);
        }
    }

    return -1;
}

QAccessibleInterface* AccessibilityController::focusedChild(const IAccessible* item) const
{
    TRACEFUNC;
    size_t count = item->accessibleChildCount();
    for (size_t i = 0; i < count; ++i) {
        const IAccessible* ch = item->accessibleChild(i);
        const Item& chIt = findItem(ch);
        if (!chIt.isValid() || !chIt.iface || chIt.item->accessibleIgnored()) {
            continue;
        }

        if (chIt.iface->state().focused) {
            return chIt.iface;
        }

        if (chIt.item->accessibleChildCount() > 0) {
            QAccessibleInterface* subItem = focusedChild(chIt.item);
            if (subItem) {
                return subItem;
            }
        }
    }

    return nullptr;
}

IAccessible* AccessibilityController::accessibleParent() const
{
    return nullptr;
}

size_t AccessibilityController::accessibleChildCount() const
{
    return static_cast<size_t>(m_children.size());
}

IAccessible* AccessibilityController::accessibleChild(size_t i) const
{
    return m_children.at(static_cast<int>(i));
}

QWindow* AccessibilityController::accessibleWindow() const
{
    return mainWindow()->qWindow();
}

IAccessible::Role AccessibilityController::accessibleRole() const
{
    return IAccessible::Role::Application;
}

QString AccessibilityController::accessibleName() const
{
    return QString("AccessibilityController");
}

QString AccessibilityController::accessibleDescription() const
{
    return QString();
}

bool AccessibilityController::accessibleState(State st) const
{
    switch (st) {
    case State::Undefined: return false;
    case State::Enabled: return true;
    case State::Active: return true;
    default: {
        LOGW() << "not handled state: " << static_cast<int>(st);
    }
    }

    return false;
}

QRect AccessibilityController::accessibleRect() const
{
    return mainWindow()->qWindow()->geometry();
}

bool AccessibilityController::accessibleIgnored() const
{
    return false;
}

QVariant AccessibilityController::accessibleValue() const
{
    return QVariant();
}

QVariant AccessibilityController::accessibleMaximumValue() const
{
    return QVariant();
}

QVariant AccessibilityController::accessibleMinimumValue() const
{
    return QVariant();
}

QVariant AccessibilityController::accessibleValueStepSize() const
{
    return QVariant();
}

void AccessibilityController::accessibleSelection(int, int*, int*) const
{
}

int AccessibilityController::accessibleSelectionCount() const
{
    return 0;
}

int AccessibilityController::accessibleCursorPosition() const
{
    return 0;
}

QString AccessibilityController::accessibleText(int, int) const
{
    return QString();
}

QString AccessibilityController::accessibleTextBeforeOffset(int, TextBoundaryType, int*, int*) const
{
    return QString();
}

QString AccessibilityController::accessibleTextAfterOffset(int, TextBoundaryType, int*, int*) const
{
    return QString();
}

QString AccessibilityController::accessibleTextAtOffset(int, TextBoundaryType, int*, int*) const
{
    return QString();
}

int AccessibilityController::accessibleCharacterCount() const
{
    return 0;
}

int AccessibilityController::accessibleRowIndex() const
{
    return 0;
}

mu::async::Channel<IAccessible::Property, mu::Val> AccessibilityController::accessiblePropertyChanged() const
{
    static async::Channel<IAccessible::Property, Val> ch;
    return ch;
}

mu::async::Channel<IAccessible::State, bool> AccessibilityController::accessibleStateChanged() const
{
    static async::Channel<IAccessible::State, bool> ch;
    return ch;
}

void AccessibilityController::setState(State, bool)
{
}
