/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2020-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#pragma once

#include "Widget.h"

QT_BEGIN_NAMESPACE
class QWidget;
QT_END_NAMESPACE

///@file
///@brief A Layouting::Widget that's deals in QWidget

namespace Layouting {

///@brief A Layouting::Widget that's deals in QWidget
/// Allows to host a QWidget in the layout
class DOCKS_EXPORT Widget_qwidget : public Widget
{
public:
    explicit Widget_qwidget(QWidget *thisWidget);
    ~Widget_qwidget() override;

    QWidget *asQWidget() const override
    {
        return m_thisWidget;
    }

    QSize sizeHint() const override;
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

private:
    QWidget *const m_thisWidget;
    Q_DISABLE_COPY(Widget_qwidget)
};

}
