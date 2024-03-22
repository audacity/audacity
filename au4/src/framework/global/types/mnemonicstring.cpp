/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2022 MuseScore BVBA and others
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
#include "mnemonicstring.h"

using namespace mu;

#ifndef NO_QT_SUPPORT
static QString processMnemonic(const QString& str, bool showUnderlines)
{
    if (str.size() <= 1) {
        return str;
    }

    QString result;
    result.reserve(str.size());

    const QChar* c = str.data();
    int l = str.length();
    while (l) {
        if (*c == '&') {
            ++c;
            --l;
            if (!l) {
                break;
            }

            if (showUnderlines && *c != '&') {
                result.append(QStringLiteral("<u>")).append(*c).append(QStringLiteral("</u>"));
                ++c;
                --l;
                continue;
            }
        }

        result.append(*c);
        ++c;
        --l;
    }

    return result;
}

QString MnemonicString::qTranslatedWithMnemonicUnderline() const
{
    return processMnemonic(m_raw.qTranslated(), true);
}

QString MnemonicString::qTranslatedWithoutMnemonic() const
{
    return processMnemonic(m_raw.qTranslated(), false);
}

#endif
