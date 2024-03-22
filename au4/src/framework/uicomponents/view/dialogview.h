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

#ifndef MU_UICOMPONENTS_DIALOGVIEW_H
#define MU_UICOMPONENTS_DIALOGVIEW_H

#include <QEventLoop>

#include "popupview.h"

namespace mu::uicomponents {
class DialogView : public PopupView
{
    Q_OBJECT

public:
    explicit DialogView(QQuickItem* parent = nullptr);
    ~DialogView() override = default;

    Q_INVOKABLE void exec();
    Q_INVOKABLE void show();
    Q_INVOKABLE void hide();
    Q_INVOKABLE void raise();
    Q_INVOKABLE void accept();
    Q_INVOKABLE void reject(int code = -1);

private:
    bool isDialog() const override;
    void onHidden() override;

    QScreen* resolveScreen() const override;

    void updateGeometry() override;

    QRect viewGeometry() const override;

    QEventLoop m_loop;
};
}

#endif // MU_UICOMPONENTS_DIALOGVIEW_H
