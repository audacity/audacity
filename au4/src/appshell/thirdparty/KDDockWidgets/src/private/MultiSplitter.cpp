/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2020-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

/**
 * @file
 * @brief A widget that supports an arbitrary number of splitters (called Separators) in any
 * combination of vertical/horizontal.
 *
 * @author Sérgio Martins \<sergio.martins@kdab.com\>
 */

#include "MultiSplitter_p.h"
#include "LayoutSaver_p.h"
#include "Config.h"
#include "DockRegistry_p.h"
#include "DockWidgetBase.h"
#include "DockWidgetBase_p.h"
#include "FloatingWindow_p.h"
#include "Frame_p.h"
#include "FrameworkWidgetFactory.h"
#include "LayoutSaver.h"
#include "Logging_p.h"
#include "MainWindowBase.h"
#include "Position_p.h"
#include "WindowBeingDragged_p.h"
#include "multisplitter/Widget.h"

#include <QScopedValueRollback>

using namespace KDDockWidgets;

MultiSplitter::MultiSplitter(QWidgetOrQuick *parent)
    : LayoutWidget(parent)
{
    Q_ASSERT(parent);
    setRootItem(new Layouting::ItemBoxContainer(this));
    DockRegistry::self()->registerLayout(this);

    setLayoutSize(parent->size());

    // Initialize min size
    updateSizeConstraints();

    setMinimumSize(minimumSize());
}

MultiSplitter::~MultiSplitter()
{
}

bool MultiSplitter::validateInputs(QWidgetOrQuick *widget, Location location,
                                   const Frame *relativeToFrame, InitialOption option) const
{
    if (!widget) {
        qWarning() << Q_FUNC_INFO << "Widget is null";
        return false;
    }

    const bool isDockWidget = qobject_cast<DockWidgetBase *>(widget);
    const bool isStartHidden = option.startsHidden();

    if (!qobject_cast<Frame *>(widget) && !qobject_cast<MultiSplitter *>(widget) && !isDockWidget) {
        qWarning() << "Unknown widget type" << widget;
        return false;
    }

    if (isDockWidget != isStartHidden) {
        qWarning() << "Wrong parameters" << isDockWidget << isStartHidden;
        return false;
    }

    if (relativeToFrame && relativeToFrame == widget) {
        qWarning() << "widget can't be relative to itself";
        return false;
    }

    Layouting::Item *item = itemForFrame(qobject_cast<Frame *>(widget));

    if (containsItem(item)) {
        qWarning() << "MultiSplitter::addWidget: Already contains" << widget;
        return false;
    }

    if (location == Location_None) {
        qWarning() << "MultiSplitter::addWidget: not adding to location None";
        return false;
    }

    const bool relativeToThis = relativeToFrame == nullptr;

    Layouting::Item *relativeToItem = itemForFrame(relativeToFrame);
    if (!relativeToThis && !containsItem(relativeToItem)) {
        qWarning() << "MultiSplitter::addWidget: Doesn't contain relativeTo:"
                   << "; relativeToFrame=" << relativeToFrame
                   << "; relativeToItem=" << relativeToItem
                   << "; options=" << option;
        return false;
    }

    return true;
}

void MultiSplitter::addWidget(QWidgetOrQuick *w, Location location,
                              Frame *relativeToWidget,
                              InitialOption option)
{
    auto frame = qobject_cast<Frame *>(w);
    if (itemForFrame(frame) != nullptr) {
        // Item already exists, remove it.
        // Changing the frame parent will make the item clean itself up. It turns into a placeholder and is removed by unrefOldPlaceholders
        frame->QWidgetAdapter::setParent(nullptr); // so ~Item doesn't delete it
        frame->setLayoutItem(nullptr); // so Item is destroyed, as there's no refs to it
    }

    // Make some sanity checks:
    if (!validateInputs(w, location, relativeToWidget, option))
        return;

    Layouting::Item *relativeTo = itemForFrame(relativeToWidget);
    if (!relativeTo)
        relativeTo = m_rootItem;

    Layouting::Item *newItem = nullptr;

    Frame::List frames = framesFrom(w);
    unrefOldPlaceholders(frames);
    auto dw = qobject_cast<DockWidgetBase *>(w);

    if (frame) {
        newItem = new Layouting::Item(this);
        newItem->setGuestWidget(frame);
    } else if (dw) {
        newItem = new Layouting::Item(this);
        frame = Config::self().frameworkWidgetFactory()->createFrame();
        newItem->setGuestWidget(frame);
        frame->addWidget(dw, option);
    } else if (auto ms = qobject_cast<MultiSplitter *>(w)) {
        newItem = ms->m_rootItem;
        newItem->setHostWidget(this);

        if (FloatingWindow *fw = ms->floatingWindow()) {
            newItem->setSize_recursive(fw->size());
        }

        delete ms;
    } else {
        // This doesn't happen but let's make coverity happy.
        // Tests will fail if this is ever printed.
        qWarning() << Q_FUNC_INFO << "Unknown widget added" << w;
        return;
    }

    Q_ASSERT(!newItem->geometry().isEmpty());
    Layouting::ItemBoxContainer::insertItemRelativeTo(newItem, relativeTo, location, option);

    if (dw && option.startsHidden())
        delete frame;
}

void MultiSplitter::addMultiSplitter(MultiSplitter *sourceMultiSplitter, Location location,
                                     Frame *relativeTo,
                                     InitialOption option)
{
    qCDebug(addwidget) << Q_FUNC_INFO << sourceMultiSplitter << location << relativeTo;
    addWidget(sourceMultiSplitter, location, relativeTo, option);
}

QVector<Layouting::Separator *> MultiSplitter::separators() const
{
    return m_rootItem->separators_recursive();
}

int MultiSplitter::availableLengthForOrientation(Qt::Orientation orientation) const
{
    if (orientation == Qt::Vertical)
        return availableSize().height();
    else
        return availableSize().width();
}

QSize MultiSplitter::availableSize() const
{
    return m_rootItem->availableSize();
}

void MultiSplitter::layoutEqually()
{
    if (!checkSanity())
        return;

    layoutEqually(m_rootItem);
}

void MultiSplitter::layoutEqually(Layouting::ItemBoxContainer *container)
{
    if (container) {
        container->layoutEqually_recursive();
    } else {
        qWarning() << Q_FUNC_INFO << "null container";
    }
}

void MultiSplitter::setRootItem(Layouting::ItemBoxContainer *root)
{
    LayoutWidget::setRootItem(root);
    m_rootItem = root;
}

Layouting::ItemBoxContainer *MultiSplitter::rootItem() const
{
    return m_rootItem;
}

QRect MultiSplitter::rectForDrop(const WindowBeingDragged *wbd, Location location,
                                 const Layouting::Item *relativeTo) const
{
    Layouting::Item item(nullptr);
    if (!wbd)
        return {};

    item.setSize(wbd->size().boundedTo(wbd->maxSize()));
    item.setMinSize(wbd->minSize());
    item.setMaxSizeHint(wbd->maxSize());

    Layouting::ItemBoxContainer *container = relativeTo ? relativeTo->parentBoxContainer()
                                                        : m_rootItem;

    return container->suggestedDropRect(&item, relativeTo, location);
}

bool MultiSplitter::deserialize(const LayoutSaver::MultiSplitter &l)
{
    setRootItem(new Layouting::ItemBoxContainer(this));
    return LayoutWidget::deserialize(l);
}
