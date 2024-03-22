/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "DockWidgetInstantiator_p.h"
#include "DockWidgetQuick.h"
#include "../DockRegistry_p.h"

using namespace KDDockWidgets;

QString DockWidgetInstantiator::uniqueName() const
{
    return m_uniqueName;
}

void DockWidgetInstantiator::setUniqueName(const QString &name)
{
    m_uniqueName = name;
    Q_EMIT uniqueNameChanged();
}

QString DockWidgetInstantiator::source() const
{
    return m_sourceFilename;
}

void DockWidgetInstantiator::setSource(const QString &source)
{
    m_sourceFilename = source;
    Q_EMIT sourceChanged();
}

DockWidgetQuick *DockWidgetInstantiator::dockWidget() const
{
    return m_dockWidget;
}

TitleBar *DockWidgetInstantiator::actualTitleBar() const
{
    return m_dockWidget ? m_dockWidget->actualTitleBar() : nullptr;
}

QString DockWidgetInstantiator::title() const
{
    return m_dockWidget ? m_dockWidget->title() : QString();
}

void DockWidgetInstantiator::setTitle(const QString &title)
{
    if (m_dockWidget)
        m_dockWidget->setTitle(title);
    m_title = title;
}

bool DockWidgetInstantiator::isFocused() const
{
    return m_dockWidget && m_dockWidget->isFocused();
}

bool DockWidgetInstantiator::isFloating() const
{
    return m_dockWidget && m_dockWidget->isFloating();
}

void DockWidgetInstantiator::setFloating(bool is)
{
    if (m_dockWidget)
        m_dockWidget->setFloating(is);
    m_isFloating = is;
}

void DockWidgetInstantiator::addDockWidgetAsTab(DockWidgetInstantiator *other,
                                                InitialVisibilityOption option)
{
    if (m_dockWidget)
        m_dockWidget->addDockWidgetAsTab(other ? other->dockWidget() : nullptr, option);
}

void DockWidgetInstantiator::addDockWidgetAsTab(DockWidgetBase *other,
                                                InitialVisibilityOption option)
{
    if (m_dockWidget)
        m_dockWidget->addDockWidgetAsTab(other, option);
}

void DockWidgetInstantiator::addDockWidgetToContainingWindow(DockWidgetBase *other,
                                                             Location location,
                                                             DockWidgetBase *relativeTo,
                                                             QSize initialSize,
                                                             InitialVisibilityOption option)
{
    if (m_dockWidget)
        m_dockWidget->addDockWidgetToContainingWindow(other, location, relativeTo,
                                                      InitialOption(option, initialSize));
}

void DockWidgetInstantiator::addDockWidgetToContainingWindow(DockWidgetInstantiator *other,
                                                             Location location,
                                                             DockWidgetInstantiator *relativeTo,
                                                             QSize initialSize,
                                                             InitialVisibilityOption option)
{
    if (m_dockWidget)
        m_dockWidget->addDockWidgetToContainingWindow(
            other ? other->dockWidget() : nullptr, location,
            relativeTo ? relativeTo->dockWidget() : nullptr, InitialOption(option, initialSize));
}

void DockWidgetInstantiator::setAsCurrentTab()
{
    if (m_dockWidget)
        m_dockWidget->setAsCurrentTab();
}

void DockWidgetInstantiator::forceClose()
{
    if (m_dockWidget)
        m_dockWidget->forceClose();
}

Q_INVOKABLE bool DockWidgetInstantiator::close()
{
    if (m_dockWidget)
        return m_dockWidget->close();

    return false;
}

void DockWidgetInstantiator::show()
{
    if (m_dockWidget)
        m_dockWidget->show();
}

void DockWidgetInstantiator::raise()
{
    if (m_dockWidget)
        m_dockWidget->raise();
}

void DockWidgetInstantiator::moveToSideBar()
{
    if (m_dockWidget)
        m_dockWidget->moveToSideBar();
}

void DockWidgetInstantiator::classBegin()
{
    // Nothing interesting to do here.
}

void DockWidgetInstantiator::componentComplete()
{
    if (m_uniqueName.isEmpty()) {
        qWarning() << Q_FUNC_INFO << "Each DockWidget need an unique name. Set the uniqueName property.";
        return;
    }

    if (DockRegistry::self()->containsDockWidget(m_uniqueName)) {
        // Dock widget already exists. all good.
        return;
    }

    if (m_dockWidget) {
        qWarning() << Q_FUNC_INFO << "Unexpected bug.";
        return;
    }
    const auto childItems = this->childItems();
    if (m_sourceFilename.isEmpty() && childItems.size() != 1) {
        qWarning() << Q_FUNC_INFO << "Either 'source' property must be set or add exactly one child"
                   << "; source=" << m_sourceFilename << "; num children=" << childItems.size();
        return;
    }

    m_dockWidget = new DockWidgetQuick(m_uniqueName, {}, {}, qmlEngine(this));

    connect(m_dockWidget, &DockWidgetQuick::titleChanged, this,
            &DockWidgetInstantiator::titleChanged);
    connect(m_dockWidget, &DockWidgetQuick::actualTitleBarChanged, this,
            &DockWidgetInstantiator::actualTitleBarChanged);
    connect(m_dockWidget, &DockWidgetQuick::optionsChanged, this,
            &DockWidgetInstantiator::optionsChanged);
    connect(m_dockWidget, &DockWidgetQuick::shown, this, &DockWidgetInstantiator::shown);
    connect(m_dockWidget, &DockWidgetQuick::hidden, this, &DockWidgetInstantiator::hidden);
    connect(m_dockWidget, &DockWidgetQuick::iconChanged, this,
            &DockWidgetInstantiator::iconChanged);
    connect(m_dockWidget, &DockWidgetQuick::widgetChanged, this,
            &DockWidgetInstantiator::widgetChanged);
    connect(m_dockWidget, &DockWidgetQuick::isFocusedChanged, this,
            &DockWidgetInstantiator::isFocusedChanged);
    connect(m_dockWidget, &DockWidgetQuick::isFocusedChanged, this,
            &DockWidgetInstantiator::isFocusedChanged);
    connect(m_dockWidget, &DockWidgetQuick::isOverlayedChanged, this,
            &DockWidgetInstantiator::isOverlayedChanged);
    connect(m_dockWidget, &DockWidgetQuick::isFloatingChanged, this,
            &DockWidgetInstantiator::isFloatingChanged);
    connect(m_dockWidget, &DockWidgetQuick::removedFromSideBar, this,
            &DockWidgetInstantiator::removedFromSideBar);
    connect(m_dockWidget, &DockWidgetQuick::windowActiveAboutToChange, this,
            &DockWidgetInstantiator::windowActiveAboutToChange);


    if (m_sourceFilename.isEmpty()) {
        m_dockWidget->setWidget(childItems.constFirst());
    } else {
        m_dockWidget->setWidget(m_sourceFilename);
    }

    if (!m_title.isEmpty())
        m_dockWidget->setTitle(m_title);

    if (m_isFloating.has_value())
        m_dockWidget->setFloating(m_isFloating.value());

    Q_EMIT dockWidgetChanged();
}
