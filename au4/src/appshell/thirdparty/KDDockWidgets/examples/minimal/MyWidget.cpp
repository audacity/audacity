/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "MyWidget.h"

#include <QPainter>
#include <QDebug>
#include <QFile>
#include <QLineEdit>

static QHash<QString, QImage> s_images; /// clazy:exclude=non-pod-global-static

MyWidget::MyWidget(const QString &backgroundFile, const QString &logoFile, QWidget *parent)
    : QWidget(parent)
{
    if (!backgroundFile.isEmpty()) {
        auto it = s_images.find(backgroundFile);
        if (it == s_images.end())
            it = s_images.insert(backgroundFile, QImage(backgroundFile));
        m_background = it.value();
    }

    if (!logoFile.isEmpty()) {
        auto it = s_images.find(logoFile);
        if (it == s_images.end())
            it = s_images.insert(logoFile, QImage(logoFile));
        m_logo = it.value();
    }

    setFocusPolicy(Qt::StrongFocus);
#if 0
    // Uncomment to show focus propagation working
    new QLineEdit(this);
    auto l2 = new QLineEdit(this);
    l2->move(0, 100);
    setFocusProxy(l2);
#endif
}

MyWidget::~MyWidget()
{
}

void MyWidget::drawLogo(QPainter &p)
{
    if (m_logo.isNull())
        return;

    const qreal ratio = m_logo.height() / (m_logo.width() * 1.0);

    const int maxWidth = int(0.80 * size().width());
    const int maxHeight = int(0.80 * size().height());

    const int proposedHeight = int(maxWidth * ratio);

    const int width = proposedHeight <= maxHeight ? maxWidth
                                                  : int(maxHeight / ratio);

    const int height = int(width * ratio);
    QRect targetLogoRect(0,0, width, height);
    targetLogoRect.moveCenter(rect().center() + QPoint(0, -int(size().height() * 0.00)));
    p.drawImage(targetLogoRect, m_logo, m_logo.rect());
}

MyWidget1::MyWidget1(MyWidget::QWidget *parent)
    : MyWidget(QStringLiteral(":/assets/triangles.png"), QStringLiteral(":/assets/KDAB_bubble_white.png"), parent)
{
}

void MyWidget1::paintEvent(QPaintEvent *)
{
    QPainter p(this);
    p.fillRect(rect(), QColor(0xCC, 0xCC, 0xCC));
    p.drawImage(m_background.rect(), m_background, m_background.rect());

    drawLogo(p);
}

MyWidget2::MyWidget2(MyWidget::QWidget *parent)
    : MyWidget(QString(), QStringLiteral(":/assets/KDAB_bubble_blue.png"), parent)
{
}

void MyWidget2::paintEvent(QPaintEvent *)
{
    QPainter p(this);
    p.fillRect(rect(), Qt::white);
    drawLogo(p);
}

MyWidget3::MyWidget3(MyWidget::QWidget *parent)
    : MyWidget(QStringLiteral(":/assets/base.png"), QStringLiteral(":/assets/KDAB_bubble_fulcolor.png"), parent)
    , m_triangle(QImage(QStringLiteral(":/assets/tri.png")))
{
}

void MyWidget3::paintEvent(QPaintEvent *)
{
    QPainter p(this);
    p.fillRect(rect(), QColor(0xD5, 0xD5, 0xD5));

    p.drawImage(m_background.rect(), m_background, m_background.rect());

    const QRect targetRect = QRect({ width() - m_triangle.width(), height() - m_triangle.height() }, m_triangle.size());

    p.drawImage(targetRect, m_triangle, m_triangle.rect());
    drawLogo(p);
}
