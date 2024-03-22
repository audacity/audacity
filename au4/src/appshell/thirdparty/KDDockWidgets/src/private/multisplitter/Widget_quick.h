/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2020-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#pragma once

#include "Widget.h"

#include <QQuickItem>

QT_BEGIN_NAMESPACE
class QQuickItem;
QT_END_NAMESPACE

///@file
///@brief A Layouting::Widget that's deals in QQuickItem

namespace Layouting {

///@brief A Layouting::Widget that's deals in QQuickItem
/// Allows to host a QQuickItem in the layout
class DOCKS_EXPORT Widget_quick : public Widget
{
public:
    explicit Widget_quick(QQuickItem *thisWidget)
        : Widget(thisWidget)
        , m_thisWidget(thisWidget)
    {
    }

    ~Widget_quick() override;

    QSize minSize() const override;
    QSize maxSizeHint() const override;
    QRect geometry() const override;
    void setGeometry(QRect) override;
    void setParent(Widget *) override;
    QDebug &dumpDebug(QDebug &) const override;
    bool isVisible() const override;
    void setVisible(bool) const override;
    std::unique_ptr<Widget> parentWidget() const override;
    void setLayoutItem(Item *) override
    {
    }
    void show() override;
    void hide() override;
    void move(int x, int y) override;
    void setSize(int width, int height) override;
    void setWidth(int width) override;
    void setHeight(int height) override;
    void update() override;

    static QSize widgetMinSize(const QWidget *w);

protected:
    QQuickItem *createQQuickItem(const QString &filename, QQuickItem *parent) const;

private:
    QQuickItem *const m_thisWidget;
    Q_DISABLE_COPY(Widget_quick)
};

}
