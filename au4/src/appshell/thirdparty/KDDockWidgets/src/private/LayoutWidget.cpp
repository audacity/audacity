/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2020-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "LayoutSaver_p.h"
#include "Config.h"
#include "DockWidgetBase_p.h"
#include "FloatingWindow_p.h"
#include "Frame_p.h"
#include "FrameworkWidgetFactory.h"
#include "MainWindowBase.h"
#include "Position_p.h"

using namespace KDDockWidgets;


LayoutWidget::LayoutWidget(QWidgetOrQuick *parent)
    : LayoutGuestWidget(parent)
{
}

LayoutWidget::~LayoutWidget()
{
    if (m_rootItem->hostWidget()->asQObject() == this)
        delete m_rootItem;
    DockRegistry::self()->unregisterLayout(this);
}

bool LayoutWidget::isInMainWindow() const
{
    return mainWindow() != nullptr;
}

MainWindowBase *LayoutWidget::mainWindow() const
{
    if (auto pw = QWidgetAdapter::parentWidget()) {
        // Note that if pw is a FloatingWindow then pw->parentWidget() can be a MainWindow too, as
        // it's parented
        if (pw->objectName() == QLatin1String("MyCentralWidget"))
            return qobject_cast<MainWindowBase *>(pw->parentWidget());

        if (auto mw = qobject_cast<MainWindowBase *>(pw))
            return mw;
    }

    return nullptr;
}

FloatingWindow *LayoutWidget::floatingWindow() const
{
    return qobject_cast<FloatingWindow *>(QWidgetAdapter::parentWidget());
}

void LayoutWidget::setRootItem(Layouting::ItemContainer *root)
{
    delete m_rootItem;
    m_rootItem = root;
    connect(m_rootItem, &Layouting::ItemContainer::numVisibleItemsChanged, this,
            &MultiSplitter::visibleWidgetCountChanged);
    connect(m_rootItem, &Layouting::ItemContainer::minSizeChanged, this,
            [this] { setMinimumSize(layoutMinimumSize()); });
}

QSize LayoutWidget::layoutMinimumSize() const
{
    return m_rootItem->minSize();
}

QSize LayoutWidget::layoutMaximumSizeHint() const
{
    return m_rootItem->maxSizeHint();
}

void LayoutWidget::setLayoutMinimumSize(QSize sz)
{
    if (sz != m_rootItem->minSize()) {
        setLayoutSize(size().expandedTo(m_rootItem->minSize())); // Increase size in case we need to
        m_rootItem->setMinSize(sz);
    }
}

QSize LayoutWidget::size() const
{
    return m_rootItem->size();
}

void LayoutWidget::clearLayout()
{
    m_rootItem->clear();
}

bool LayoutWidget::checkSanity() const
{
    return m_rootItem->checkSanity();
}

void LayoutWidget::dumpLayout() const
{
    m_rootItem->dumpLayout();
}

void LayoutWidget::restorePlaceholder(DockWidgetBase *dw, Layouting::Item *item, int tabIndex)
{
    if (item->isPlaceholder()) {
        Frame *newFrame = Config::self().frameworkWidgetFactory()->createFrame(this);
        item->restore(newFrame);
    }

    auto frame = qobject_cast<Frame *>(item->guestAsQObject());
    Q_ASSERT(frame);

    if (tabIndex != -1 && frame->dockWidgetCount() >= tabIndex) {
        frame->insertWidget(dw, tabIndex);
    } else {
        frame->addWidget(dw);
    }

    frame->QWidgetAdapter::setVisible(true);
}

void LayoutWidget::unrefOldPlaceholders(const Frame::List &framesBeingAdded) const
{
    for (Frame *frame : framesBeingAdded) {
        for (DockWidgetBase *dw : frame->dockWidgets()) {
            dw->d->lastPositions().removePlaceholders(this);
        }
    }
}

void LayoutWidget::setLayoutSize(QSize size)
{
    if (size != this->size()) {
        m_rootItem->setSize_recursive(size);
        if (!m_inResizeEvent && !LayoutSaver::restoreInProgress())
            resize(size);
    }
}

const Layouting::Item::List LayoutWidget::items() const
{
    return m_rootItem->items_recursive();
}

bool LayoutWidget::containsItem(const Layouting::Item *item) const
{
    return m_rootItem->contains_recursive(item);
}

bool LayoutWidget::containsFrame(const Frame *frame) const
{
    return itemForFrame(frame) != nullptr;
}

int LayoutWidget::count() const
{
    return m_rootItem->count_recursive();
}

int LayoutWidget::visibleCount() const
{
    return m_rootItem->visibleCount_recursive();
}

int LayoutWidget::placeholderCount() const
{
    return count() - visibleCount();
}

Layouting::Item *LayoutWidget::itemForFrame(const Frame *frame) const
{
    if (!frame)
        return nullptr;

    return m_rootItem->itemForWidget(frame);
}

DockWidgetBase::List LayoutWidget::dockWidgets() const
{
    DockWidgetBase::List dockWidgets;
    const Frame::List frames = this->frames();
    for (Frame *frame : frames)
        dockWidgets << frame->dockWidgets();

    return dockWidgets;
}

Frame::List LayoutWidget::framesFrom(QWidgetOrQuick *frameOrMultiSplitter) const
{
    if (auto frame = qobject_cast<Frame *>(frameOrMultiSplitter))
        return { frame };

    if (auto msw = qobject_cast<MultiSplitter *>(frameOrMultiSplitter))
        return msw->frames();

    return {};
}

Frame::List LayoutWidget::frames() const
{
    const Layouting::Item::List items = m_rootItem->items_recursive();

    Frame::List result;
    result.reserve(items.size());

    for (Layouting::Item *item : items) {
        if (auto f = static_cast<Frame *>(item->guestAsQObject()))
            result.push_back(f);
    }

    return result;
}

void LayoutWidget::removeItem(Layouting::Item *item)
{
    if (!item) {
        qWarning() << Q_FUNC_INFO << "nullptr item";
        return;
    }

    item->parentContainer()->removeItem(item);
}

void LayoutWidget::updateSizeConstraints()
{
    const QSize newMinSize = m_rootItem->minSize();
    qCDebug(sizing) << Q_FUNC_INFO << "Updating size constraints from" << minimumSize() << "to"
                    << newMinSize;

    setLayoutMinimumSize(newMinSize);
}

bool LayoutWidget::deserialize(const LayoutSaver::MultiSplitter &l)
{
    QHash<QString, Layouting::Widget *> frames;
    for (const LayoutSaver::Frame &frame : std::as_const(l.frames)) {
        Frame *f = Frame::deserialize(frame);
        Q_ASSERT(!frame.id.isEmpty());
        frames.insert(frame.id, f);
    }

    m_rootItem->fillFromVariantMap(l.layout, frames);

    updateSizeConstraints();

    // This qMin() isn't needed for QtWidgets (but harmless), but it's required for QtQuick
    // as some sizing is async
    const QSize newLayoutSize = QWidgetAdapter::size().expandedTo(m_rootItem->minSize());

    m_rootItem->setSize_recursive(newLayoutSize);

    return true;
}

void LayoutWidget::onLayoutRequest()
{
    updateSizeConstraints();
}

bool LayoutWidget::onResize(QSize newSize)
{
    QScopedValueRollback<bool> resizeGuard(m_inResizeEvent, true); // to avoid re-entrancy

    if (!LayoutSaver::restoreInProgress()) {
        // don't resize anything while we're restoring the layout
        setLayoutSize(newSize);
    }

    return false; // So QWidget::resizeEvent is called
}

LayoutSaver::MultiSplitter LayoutWidget::serialize() const
{
    LayoutSaver::MultiSplitter l;
    l.layout = m_rootItem->toVariantMap();
    const Layouting::Item::List items = m_rootItem->items_recursive();
    l.frames.reserve(items.size());
    for (Layouting::Item *item : items) {
        if (!item->isContainer()) {
            if (auto frame = qobject_cast<Frame *>(item->guestAsQObject()))
                l.frames.insert(frame->id(), frame->serialize());
        }
    }

    return l;
}
