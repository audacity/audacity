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

#ifndef MU_UICOMPONENTS_DROPDOWNVIEW_H
#define MU_UICOMPONENTS_DROPDOWNVIEW_H

#include "popupview.h"

class QQuickCloseEvent;

namespace mu::uicomponents {
class DropdownView : public PopupView
{
    Q_OBJECT

    Q_PROPERTY(int focusItemY READ focusItemY WRITE setFocusItemY NOTIFY focusItemYChanged)

public:
    explicit DropdownView(QQuickItem* parent = nullptr);
    ~DropdownView() override = default;

    int focusItemY() const;
    void setFocusItemY(int newFocusItemY);

signals:
    void focusItemYChanged();

private:
    void updateGeometry() override;

    int m_focusItemY = -1;
};
}

#endif // MU_UICOMPONENTS_DROPDOWNVIEW_H
