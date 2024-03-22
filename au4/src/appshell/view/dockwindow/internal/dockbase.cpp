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

#include "dockbase.h"

#include <QRect>
#include <QTimer>
#include <QAction>

#include "log.h"

#include "thirdparty/KDDockWidgets/src/DockWidgetQuick.h"
#include "thirdparty/KDDockWidgets/src/private/quick/FrameQuick_p.h"
#include "thirdparty/KDDockWidgets/src/private/FloatingWindow_p.h"

namespace mu::dock {
static QSize adjustSizeByConstraints(const QSize& size, const QSize& min, const QSize& max)
{
    return size.expandedTo(min).boundedTo(max);
}

static bool sizeInRange(const QSize& size, const QSize& min, const QSize& max)
{
    bool widthInRange = size.width() >= min.width() && size.width() <= max.width();
    bool heightInRange = size.height() >= min.height() && size.height() <= max.height();

    return widthInRange && heightInRange;
}

class DockWidgetImpl : public KDDockWidgets::DockWidgetQuick
{
public:
    DockWidgetImpl(const QString& uniqueName)
        : KDDockWidgets::DockWidgetQuick(uniqueName)
    {
        setObjectName(uniqueName);
    }

    QSize minimumSize() const override
    {
        return DockWidgetBase::minimumSize();
    }

    QSize maximumSize() const override
    {
        return DockWidgetBase::maximumSize();
    }
};
}

using namespace mu::dock;

DockBase::DockBase(DockType type, QQuickItem* parent)
    : QQuickItem(parent)
{
    Q_ASSERT(type != DockType::Undefined);

    m_properties.type = type;
    m_properties.floatable = true;
    m_properties.closable = true;
    m_properties.resizable = true;
    m_properties.separatorsVisible = true;
}

QString DockBase::title() const
{
    return m_title;
}

int DockBase::minimumWidth() const
{
    return m_minimumWidth;
}

int DockBase::minimumHeight() const
{
    return m_minimumHeight;
}

int DockBase::maximumWidth() const
{
    return m_maximumWidth;
}

int DockBase::maximumHeight() const
{
    return m_maximumHeight;
}

int DockBase::contentWidth() const
{
    return m_contentWidth;
}

int DockBase::contentHeight() const
{
    return m_contentHeight;
}

QSize DockBase::preferredSize() const
{
    return QSize(width(), height());
}

int DockBase::locationProperty() const
{
    return static_cast<int>(m_properties.location);
}

Location DockBase::location() const
{
    return m_properties.location;
}

QPoint DockBase::globalPosition() const
{
    if (!m_dockWidget) {
        return QPoint();
    }

    auto frame = static_cast<const KDDockWidgets::FrameQuick*>(m_dockWidget->frame());
    if (!frame) {
        return QPoint();
    }

    return frame->mapToGlobal(QPoint(0, 0));
}

QVariantList DockBase::dropDestinationsProperty() const
{
    return m_dropDestinations;
}

QList<DropDestination> DockBase::dropDestinations() const
{
    QList<DropDestination> result;

    for (const QVariant& obj : m_dropDestinations) {
        QVariantMap map = obj.toMap();

        DropDestination destination;
        destination.dock = map["dock"].value<DockBase*>();

        if (map.contains("dropLocation")) {
            destination.dropLocation = static_cast<Location>(map["dropLocation"].toInt());
        } else {
            destination.dropLocation = Location::Left;
        }

        if (map.contains("dropDistance")) {
            destination.dropDistance = map["dropDistance"].toInt();
        }

        result << destination;
    }

    return result;
}

bool DockBase::closable() const
{
    return m_properties.closable;
}

bool DockBase::resizable() const
{
    return m_properties.resizable;
}

bool DockBase::separatorsVisible() const
{
    return m_properties.separatorsVisible;
}

bool DockBase::floatable() const
{
    return m_properties.floatable;
}

bool DockBase::floating() const
{
    return m_floating;
}

bool DockBase::inited() const
{
    return m_inited;
}

DockType DockBase::type() const
{
    return m_properties.type;
}

KDDockWidgets::DockWidgetQuick* DockBase::dockWidget() const
{
    return m_dockWidget;
}

void DockBase::setTitle(const QString& title)
{
    if (title == m_title) {
        return;
    }

    m_title = title;
    emit titleChanged();
}

void DockBase::setMinimumWidth(int width)
{
    if (width == minimumWidth()) {
        return;
    }

    m_minimumWidth = width;
    emit minimumSizeChanged();
}

void DockBase::setMinimumHeight(int height)
{
    if (height == minimumHeight()) {
        return;
    }

    m_minimumHeight = height;
    emit minimumSizeChanged();
}

void DockBase::setMaximumWidth(int width)
{
    if (width == maximumWidth()) {
        return;
    }

    m_maximumWidth = width;
    emit maximumSizeChanged();
}

void DockBase::setMaximumHeight(int height)
{
    if (height == maximumHeight()) {
        return;
    }

    m_maximumHeight = height;
    emit maximumSizeChanged();
}

void DockBase::setContentWidth(int width)
{
    if (m_contentWidth == width) {
        return;
    }

    m_contentWidth = width;
    emit contentSizeChanged();
}

void DockBase::setContentHeight(int height)
{
    if (m_contentHeight == height) {
        return;
    }

    m_contentHeight = height;
    emit contentSizeChanged();
}

void DockBase::setLocation(int location)
{
    if (location == m_properties.location) {
        return;
    }

    m_properties.location = static_cast<Location>(location);
    emit locationChanged();
}

void DockBase::setDropDestinations(const QVariantList& destinations)
{
    if (m_dropDestinations == destinations) {
        return;
    }

    m_dropDestinations = destinations;
    emit dropDestinationsChanged();
}

void DockBase::setFloatable(bool floatable)
{
    if (floatable == m_properties.floatable) {
        return;
    }

    m_properties.floatable = floatable;
    emit floatableChanged();
}

void DockBase::setClosable(bool closable)
{
    if (closable == m_properties.closable) {
        return;
    }

    m_properties.closable = closable;
    emit closableChanged();
}

void DockBase::setResizable(bool resizable)
{
    if (resizable == m_properties.resizable) {
        return;
    }

    m_properties.resizable = resizable;
    emit resizableChanged();
}

void DockBase::setSeparatorsVisible(bool visible)
{
    if (visible == m_properties.separatorsVisible) {
        return;
    }

    m_properties.separatorsVisible = visible;
    emit separatorsVisibleChanged();
}

void DockBase::setFloating(bool floating)
{
    IF_ASSERT_FAILED(m_dockWidget) {
        return;
    }

    m_dockWidget->setFloating(floating);
}

void DockBase::setContentNavigationPanel(mu::ui::NavigationPanel* panel)
{
    if (m_contentNavigationPanel == panel) {
        return;
    }

    m_contentNavigationPanel = panel;
    emit contentNavigationPanelChanged();
}

void DockBase::init()
{
    IF_ASSERT_FAILED(m_dockWidget) {
        return;
    }

    setVisible(m_dockWidget->isOpen());
    setInited(true);

    applySizeConstraints();
}

void DockBase::deinit()
{
    setInited(false);
}

bool DockBase::isOpen() const
{
    IF_ASSERT_FAILED(m_dockWidget) {
        return false;
    }

    return m_dockWidget->isOpen();
}

void DockBase::open()
{
    TRACEFUNC;

    IF_ASSERT_FAILED(m_dockWidget) {
        return;
    }

    m_dockWidget->show();
    setVisible(true);

    applySizeConstraints();
}

void DockBase::close()
{
    TRACEFUNC;

    IF_ASSERT_FAILED(m_dockWidget) {
        return;
    }

    m_dockWidget->forceClose();
    setVisible(false);
}

void DockBase::showHighlighting(const QRect& highlightingRect)
{
    if (highlightingRect == m_properties.highlightingRect) {
        return;
    }

    m_properties.highlightingRect = highlightingRect;
    writeProperties();
}

void DockBase::hideHighlighting()
{
    showHighlighting(QRect());
}

QRect DockBase::frameGeometry() const
{
    if (m_dockWidget && m_dockWidget->isVisible()) {
        return m_dockWidget->frameGeometry();
    }

    return QRect();
}

bool DockBase::isInSameFrame(const DockBase* other) const
{
    IF_ASSERT_FAILED(other) {
        return false;
    }

    auto frame = static_cast<const KDDockWidgets::FrameQuick*>(m_dockWidget->frame());
    auto otherFrame = static_cast<const KDDockWidgets::FrameQuick*>(other->dockWidget()->frame());

    if (!frame || !otherFrame) {
        return false;
    }

    return frame && otherFrame && frame == otherFrame;
}

void DockBase::setFramePanelOrder(int order)
{
    if (!m_dockWidget) {
        return;
    }

    auto frame = static_cast<const KDDockWidgets::FrameQuick*>(m_dockWidget->frame());
    if (!frame) {
        return;
    }

    if (frame->beingDeletedLater()) {
        return;
    }

    QQuickItem* frameVisualItem = frame->visualItem();
    if (!frameVisualItem) {
        return;
    }

    frameVisualItem->setProperty("titleBarNavigationPanelOrder", order);
}

void DockBase::resetToDefault()
{
    setVisible(m_defaultVisibility);
}

void DockBase::resize(int width, int height)
{
    TRACEFUNC;

    if (width == this->width() && height == this->height()) {
        return;
    }

    if (!m_dockWidget) {
        return;
    }

    auto frame = static_cast<const KDDockWidgets::FrameQuick*>(m_dockWidget->frame());
    if (!frame) {
        return;
    }

    const Layouting::Item* item = frame->layoutItem();
    if (!item) {
        return;
    }

    Layouting::ItemBoxContainer* parentContainer = item->parentBoxContainer();
    if (!parentContainer) {
        return;
    }

    width = qBound(m_minimumWidth, width, m_maximumWidth);
    height = qBound(m_minimumHeight, height, m_maximumHeight);

    QSize minSizeBackup = QSize(m_minimumWidth, m_minimumHeight);
    QSize maxSizeBackup = QSize(m_maximumWidth, m_maximumHeight);

    m_minimumWidth = width;
    m_maximumWidth = width;

    const QQuickItem* visualItem = frame->visualItem();
    int extraHeight = visualItem ? visualItem->property("nonContentsHeight").toInt() : 0;
    height += extraHeight;

    m_minimumHeight = height;
    m_maximumHeight = height;

    applySizeConstraints();
    parentContainer->layoutEqually();

    m_minimumWidth = minSizeBackup.width();
    m_maximumWidth = maxSizeBackup.width();
    m_minimumHeight = minSizeBackup.height();
    m_maximumHeight = maxSizeBackup.height();

    applySizeConstraints();
}

mu::ui::NavigationPanel* DockBase::contentNavigationPanel() const
{
    return m_contentNavigationPanel;
}

void DockBase::componentComplete()
{
    TRACEFUNC;

    QQuickItem::componentComplete();

    auto children = childItems();
    IF_ASSERT_FAILED_X(children.size() == 1, "Dock must have only one child as its content!") {
        return;
    }

    QQuickItem* content = children.first();
    IF_ASSERT_FAILED(content) {
        return;
    }

    if (content->objectName().isEmpty()) {
        content->setObjectName(objectName() + "_content");
    }

    m_dockWidget = new DockWidgetImpl(objectName());
    m_dockWidget->setWidget(content);
    m_dockWidget->setTitle(m_title);

    writeProperties();
    listenFloatingChanges();

    connect(m_dockWidget, &KDDockWidgets::DockWidgetQuick::widthChanged, this, [this]() {
        if (m_dockWidget) {
            setWidth(m_dockWidget->width());
        }
    });

    connect(m_dockWidget, &KDDockWidgets::DockWidgetQuick::heightChanged, this, [this]() {
        if (m_dockWidget) {
            setHeight(m_dockWidget->height());
        }
    });

    connect(this, &DockBase::minimumSizeChanged, this, &DockBase::applySizeConstraints);
    connect(this, &DockBase::maximumSizeChanged, this, &DockBase::applySizeConstraints);

    connect(this, &DockBase::visibleChanged, [this](){
        emit reorderNavigationRequested();
    });

    m_defaultVisibility = isVisible();
}

void DockBase::applySizeConstraints()
{
    if (!m_dockWidget) {
        return;
    }

    TRACEFUNC;

    int minWidth = m_minimumWidth > 0 ? m_minimumWidth : m_dockWidget->minimumWidth();
    int minHeight = m_minimumHeight > 0 ? m_minimumHeight : m_dockWidget->minimumHeight();
    int maxWidth = m_maximumWidth > 0 ? m_maximumWidth : m_dockWidget->maximumWidth();
    int maxHeight = m_maximumHeight > 0 ? m_maximumHeight : m_dockWidget->maximumHeight();

    minWidth = std::min(minWidth, maxWidth);
    minHeight = std::min(minHeight, maxHeight);
    maxWidth = std::max(maxWidth, minWidth);
    maxHeight = std::max(maxHeight, minHeight);

    QSize minimumSize(minWidth, minHeight);
    QSize maximumSize(maxWidth, maxHeight);

    if (!m_properties.resizable) {
        maximumSize = minimumSize;
    }

    KDDockWidgets::Frame* frame = m_dockWidget->frame();

    if (frame) {
        frame->setMinimumSize(minimumSize);
        frame->setMaximumSize(maximumSize);
    }

    m_dockWidget->setMinimumSize(minimumSize);
    m_dockWidget->setMaximumSize(maximumSize);

    if (KDDockWidgets::FloatingWindow* window = m_dockWidget->floatingWindow()) {
        window->setMinimumSize(minimumSize);
        window->setMaximumSize(maximumSize);

        QSize winSize = adjustSizeByConstraints(window->frameGeometry().size(), minimumSize, maximumSize);
        QRect winRect(window->dragRect().topLeft(), winSize);

        window->setGeometry(winRect);

        if (KDDockWidgets::LayoutWidget* layout = window->layoutWidget()) {
            layout->setLayoutSize(winSize);
        }
    }

    if (!frame || !m_inited) {
        return;
    }

    QSize currentSize = m_dockWidget->size();

    //! NOTE: Initial size for all dock-widgets
    //! See QWidgetAdapter_quick.cpp, QWidgetAdapter::QWidgetAdapter
    static constexpr QSize INITIAL_DOCK_SIZE(800, 800);
    if (currentSize == INITIAL_DOCK_SIZE) {
        return;
    }

    if (sizeInRange(currentSize, minimumSize, maximumSize)) {
        return;
    }

    if (const Layouting::Item* layout = frame->layoutItem()) {
        if (Layouting::ItemBoxContainer* container = layout->parentBoxContainer()) {
            container->layoutEqually_recursive();
        }
    }
}

void DockBase::listenFloatingChanges()
{
    IF_ASSERT_FAILED(m_dockWidget) {
        return;
    }

    auto frameConn = std::make_shared<QMetaObject::Connection>();

    connect(m_dockWidget, &KDDockWidgets::DockWidgetQuick::parentChanged, this, [this, frameConn]() {
        if (frameConn) {
            disconnect(*frameConn);
            doSetFloating(false);
        }

        if (!m_dockWidget || !m_dockWidget->parentItem()) {
            return;
        }

        const KDDockWidgets::Frame* frame = m_dockWidget->frame();
        if (!frame) {
            return;
        }

        //! NOTE: window will be available later
        //! So it is important to apply size constraints
        //! and emit floatingChanged() after that
        QTimer::singleShot(0, this, [this]() {
            updateFloatingStatus();

            if (m_floating) {
                applySizeConstraints();
            }
        });

        *frameConn = connect(frame, &KDDockWidgets::Frame::isInMainWindowChanged,
                             this, &DockBase::onIsInMainWindowChanged, Qt::UniqueConnection);
    });

    connect(m_dockWidget->toggleAction(), &QAction::toggled, this, [this]() {
        if (!isOpen()) {
            doSetFloating(false);
        }
    });
}

void DockBase::updateFloatingStatus()
{
    bool floating = m_dockWidget && m_dockWidget->floatingWindow();

    doSetFloating(floating);
}

void DockBase::onIsInMainWindowChanged()
{
    applySizeConstraints();
    updateFloatingStatus();
}

void DockBase::doSetFloating(bool floating)
{
    if (m_floating == floating) {
        return;
    }

    m_floating = floating;
    emit floatingChanged();
}

void DockBase::writeProperties()
{
    if (m_dockWidget) {
        writePropertiesToObject(m_properties, *m_dockWidget);
    }
}

void DockBase::setInited(bool inited)
{
    if (m_inited == inited) {
        return;
    }

    m_inited = inited;
    emit initedChanged();
}

bool DropDestination::operator==(const DropDestination& dest) const
{
    return dock == dest.dock && dropLocation == dest.dropLocation && dropDistance == dest.dropDistance;
}

bool DropDestination::isValid() const
{
    return dock != nullptr;
}

void DropDestination::clear()
{
    dock = nullptr;
    dropLocation = Location::Undefined;
    dropDistance = 0;
}
