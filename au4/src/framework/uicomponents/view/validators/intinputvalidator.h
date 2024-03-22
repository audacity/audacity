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
#ifndef INTINPUTVALIDATOR_H
#define INTINPUTVALIDATOR_H

#include <QString>
#include <QValidator>

namespace mu::uicomponents {
class IntInputValidator : public QValidator
{
    Q_OBJECT
    Q_PROPERTY(int top READ top WRITE setTop)
    Q_PROPERTY(int bottom READ bottom WRITE setBottom)

public:
    explicit IntInputValidator(QObject* parent = nullptr);

    void fixup(QString& string) const override;
    State validate(QString& inputStr, int& cursorPos) const override;

    int top() const;
    int bottom() const;

public slots:
    void setTop(int);
    void setBottom(int);

private:
    int m_top = 999;
    int m_bottom = -999;
};
}

#endif // INTINPUTVALIDATOR_H
