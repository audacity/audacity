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
#include "intinputvalidator.h"

using namespace mu::uicomponents;

IntInputValidator::IntInputValidator(QObject* parent)
    : QValidator(parent)
{
}

void IntInputValidator::fixup(QString& string) const
{
    if (string.isEmpty() || string.endsWith("-")) {
        string.append("0");
    }
    if (string.toInt() == 0) {
        string = "0";
    }

    if (string.toInt() > m_top) {
        string = QString::number(m_top);
    } else if (string.toInt() < m_bottom) {
        string = QString::number(m_bottom);
    }
}

QValidator::State IntInputValidator::validate(QString& inputStr, int& cursorPos) const
{
    QValidator::State state = Invalid;

    const int maxAbsoluteValue = std::max(std::abs(m_top), std::abs(m_bottom));
    const int maxNumberOfDigits = maxAbsoluteValue > 0
                                  ? std::floor(std::log10(maxAbsoluteValue)) + 1
                                  : 1;
    if (inputStr.contains(QRegularExpression(QString("^\\-?\\d{1,%1}$").arg(maxNumberOfDigits)))) {
        if ((maxNumberOfDigits >= 2 && inputStr.contains(QRegularExpression(QString("^\\-?0{2,%1}").arg(maxNumberOfDigits))))
            || (inputStr.startsWith("-") && inputStr.toDouble() == 0.0)) {
            state = Intermediate;
        } else {
            state = Acceptable;
        }
    } else if (inputStr.contains(QRegularExpression("^\\-?$"))) {
        state = Intermediate;
    } else {
        cursorPos = 0;
        return Invalid;
    }

    if (inputStr.toInt() > m_top || inputStr.toInt() < m_bottom) {
        state = Intermediate;
    }

    return state;
}

int IntInputValidator::top() const
{
    return m_top;
}

int IntInputValidator::bottom() const
{
    return m_bottom;
}

void IntInputValidator::setTop(int top)
{
    m_top = top;
}

void IntInputValidator::setBottom(int bottom)
{
    m_bottom = bottom;
}
