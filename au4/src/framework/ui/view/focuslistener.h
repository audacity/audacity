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
#ifndef MU_UI_FOCUSLISTENER_H
#define MU_UI_FOCUSLISTENER_H

#include <QQuickItem>

namespace mu::ui {
class FocusListener : public QObject
{
    Q_OBJECT

    Q_PROPERTY(QQuickItem * item READ item WRITE setItem NOTIFY itemChanged)

public:
    explicit FocusListener(QObject* parent = nullptr);

    QQuickItem* item() const;

signals:
    void itemChanged();

public slots:
    void setItem(QQuickItem* item);

private:
    void listenFocusChanged();

    bool eventFilter(QObject* watched, QEvent* event);

    QQuickItem* m_item = nullptr;
};
}

#endif // MU_UI_FOCUSLISTENER_H
