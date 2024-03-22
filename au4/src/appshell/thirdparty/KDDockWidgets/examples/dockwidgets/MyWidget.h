/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#ifndef EXAMPLEDOCKABLEWIDGET_H
#define EXAMPLEDOCKABLEWIDGET_H

#pragma once

#include <QWidget>

QT_BEGIN_NAMESPACE
class QPainter;
QT_END_NAMESPACE

class MyWidget : public QWidget
{
    Q_OBJECT
public:
    explicit MyWidget(const QString &backgroundFile, const QString &logoFile, QWidget *parent = nullptr);
    ~MyWidget();
protected:
    void drawLogo(QPainter &);
    QImage m_background;
    QImage m_logo;
};

class MyWidget1 : public MyWidget
{
    Q_OBJECT
public:
    explicit MyWidget1(QWidget *parent = nullptr);
protected:
    void paintEvent(QPaintEvent*) override;
};

class MyWidget2 : public MyWidget
{
    Q_OBJECT
public:
    explicit MyWidget2(QWidget *parent = nullptr);
protected:
    void paintEvent(QPaintEvent*) override;
};

class MyWidget3 : public MyWidget
{
    Q_OBJECT
public:
    explicit MyWidget3(QWidget *parent = nullptr);
protected:
    void paintEvent(QPaintEvent*) override;
    QImage m_triangle;
};


#endif
