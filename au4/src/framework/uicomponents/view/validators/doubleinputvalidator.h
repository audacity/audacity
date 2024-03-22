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
#ifndef DOUBLEINPUTVALIDATOR_H
#define DOUBLEINPUTVALIDATOR_H

#include <QString>
#include <QValidator>

namespace mu::uicomponents {
class DoubleInputValidator : public QValidator
{
    Q_OBJECT
    Q_PROPERTY(qreal top READ top WRITE setTop)
    Q_PROPERTY(qreal bottom READ bottom WRITE setBottom)
    Q_PROPERTY(int decimal READ decimal WRITE setDecimal)

public:
    explicit DoubleInputValidator(QObject* parent = nullptr);

    void fixup(QString& string) const override;
    State validate(QString& inputStr, int& cursorPos) const override;

    qreal top() const;
    qreal bottom() const;
    int decimal() const;

public slots:
    void setTop(qreal);
    void setBottom(qreal);
    void setDecimal(int);

private:
    qreal m_top = 999.0;
    qreal m_bottom = -999.0;
    int m_decimal = 2;
};
}

#endif // DOUBLEINPUTVALIDATOR_H
