/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#ifndef KD_TITLEBARWIDGET_P_H
#define KD_TITLEBARWIDGET_P_H

#include "kddockwidgets/docks_export.h"
#include "kddockwidgets/private/TitleBar_p.h"

#include <QPainter>
#include <QToolButton>
#include <QStyle>
#include <QWidget>
#include <QVector>
#include <QStyleOptionToolButton>

QT_BEGIN_NAMESPACE
class QHBoxLayout;
class QLabel;
QT_END_NAMESPACE

namespace KDDockWidgets {

class DockWidget;
class Frame;

class DOCKS_EXPORT TitleBarWidget : public TitleBar
{
    Q_OBJECT
public:
    explicit TitleBarWidget(Frame *parent);
    explicit TitleBarWidget(FloatingWindow *parent);
    ~TitleBarWidget() override;

    ///@brief getter for the close button
    QWidget *closeButton() const;

protected:
    void paintEvent(QPaintEvent *) override;
    void mouseDoubleClickEvent(QMouseEvent *) override;
    void updateMaximizeButton() override;
    void updateMinimizeButton() override;
    void updateAutoHideButton() override;
    QSize sizeHint() const override;

#ifdef DOCKS_DEVELOPER_MODE
    // The following are needed for the unit-tests
    bool isCloseButtonVisible() const override;
    bool isCloseButtonEnabled() const override;
    bool isFloatButtonVisible() const override;
    bool isFloatButtonEnabled() const override;
#endif

private:
    void init();
    int buttonAreaWidth() const;
    void updateMargins();
    QRect iconRect() const;

    QHBoxLayout *const m_layout;
    QAbstractButton *m_closeButton = nullptr;
    QAbstractButton *m_floatButton = nullptr;
    QAbstractButton *m_maximizeButton = nullptr;
    QAbstractButton *m_minimizeButton = nullptr;
    QAbstractButton *m_autoHideButton = nullptr;
    QLabel *m_dockWidgetIcon = nullptr;
};

/// @brief Button widget to be used in the TitleBar.
/// These are the KDDockWidget default buttons. Users can replace with their own and are not
/// forced to use these.
class Button : public QToolButton
{
    Q_OBJECT
public:
    explicit Button(QWidget *parent)
        : QToolButton(parent)
    {
        setSizePolicy(QSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed));
    }

    ~Button() override;
    QSize sizeHint() const override;

    void paintEvent(QPaintEvent *) override;
};

}

#endif
