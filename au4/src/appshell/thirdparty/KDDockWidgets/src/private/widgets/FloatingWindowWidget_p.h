/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#ifndef KD_FLOATING_WINDOWWIDGET_P_H
#define KD_FLOATING_WINDOWWIDGET_P_H

#include "../FloatingWindow_p.h"

QT_BEGIN_NAMESPACE
class QVBoxLayout;
QT_END_NAMESPACE

namespace KDDockWidgets {

class DOCKS_EXPORT FloatingWindowWidget : public FloatingWindow
{
    Q_OBJECT
public:
    explicit FloatingWindowWidget(QRect suggestedGeometry, MainWindowBase *parent = nullptr);
    explicit FloatingWindowWidget(Frame *frame, QRect suggestedGeometry, MainWindowBase *parent = nullptr);

protected:
    void paintEvent(QPaintEvent *) override;
    bool event(QEvent *ev) override;

    QVBoxLayout *const m_vlayout;
    QMetaObject::Connection m_screenChangedConnection;

private:
    void init();
    void updateMargins();
    Q_DISABLE_COPY(FloatingWindowWidget)
};

}

#endif
