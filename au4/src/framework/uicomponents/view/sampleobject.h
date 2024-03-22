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
#ifndef MU_UICOMPONENTS_SAMPLEOBJECT_H
#define MU_UICOMPONENTS_SAMPLEOBJECT_H

#include <QObject>

namespace mu::uicomponents {
class SampleObject : public QObject
{
    Q_OBJECT
    Q_PROPERTY(State state READ state NOTIFY stateChanged)

    Q_ENUMS(State)

public:
    explicit SampleObject(QObject* parent = nullptr);

    enum State {
        First = 0,
        Second,
        Third
    };

    State state() const;

    Q_INVOKABLE void next();

signals:
    void stateChanged(State state);

private:

    State m_state = First;
};
}

#endif // MU_UICOMPONENTS_SAMPLEOBJECT_H
