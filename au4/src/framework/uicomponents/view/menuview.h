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

#ifndef MU_UICOMPONENTS_MENUVIEW_H
#define MU_UICOMPONENTS_MENUVIEW_H

#include "popupview.h"

namespace mu::uicomponents {
class MenuView : public PopupView
{
    Q_OBJECT
    Q_INTERFACES(QQmlParserStatus)

    Q_PROPERTY(int contentWidth READ contentWidth WRITE setContentWidth NOTIFY contentWidthChanged)
    Q_PROPERTY(int contentHeight READ contentHeight WRITE setContentHeight NOTIFY contentHeightChanged)

    Q_PROPERTY(Qt::AlignmentFlag cascadeAlign READ cascadeAlign WRITE setCascadeAlign NOTIFY cascadeAlignChanged)

public:
    explicit MenuView(QQuickItem* parent = nullptr);
    ~MenuView() override = default;

    Q_INVOKABLE int viewVerticalMargin() const;

    Qt::AlignmentFlag cascadeAlign() const;

    int contentWidth() const;
    void setContentWidth(int newContentWidth);

    int contentHeight() const;
    void setContentHeight(int newContentHeight);

public slots:
    void setCascadeAlign(Qt::AlignmentFlag cascadeAlign);

signals:
    void cascadeAlignChanged(Qt::AlignmentFlag cascadeAlign);

    void contentWidthChanged();
    void contentHeightChanged();

private:
    void componentComplete() override;

    void updateGeometry() override;

    QRect viewGeometry() const override;

    Qt::AlignmentFlag parentCascadeAlign(const QQuickItem* parent) const;

    QQuickItem* parentMenuContentItem() const;

private:
    Qt::AlignmentFlag m_cascadeAlign = Qt::AlignmentFlag::AlignRight;

    int m_contentWidth = -1;
    int m_contentHeight = -1;
};
}

#endif // MU_UICOMPONENTS_MENUVIEW_H
