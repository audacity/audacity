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

#ifndef MU_UI_MUSICALSYMBOLCODE_H
#define MU_UI_MUSICALSYMBOLCODE_H

#include <QObject>

namespace mu::ui {
/**
 * @brief The MusicalSymbolCodes class simplifies access to the icons from the musical font
 *
 * @details Each enum value is a UTF-16-like address of the icon in the icon font.
 *          The current default musical font (Leland.otf) is located in the 'MuseScore/fonts/leland' folder,
 *          The most actual version can be found by this persistent URL: @link https://www.dropbox.com/s/fhiu26x7sgboxjf/Leland.otf?dl=0
 */

class MusicalSymbolCodes
{
    Q_GADGET

public:
    enum class Code : char16_t {
        ZERO = 0xE080,
        ONE = 0xE081,
        TWO = 0xE082,
        THREE = 0xE083,
        FOUR = 0xE084,
        FIVE = 0xE085,
        SIX = 0xE086,
        SEVEN = 0xE087,
        EIGHT = 0xE088,
        NINE = 0xE089,
        TIMESIG_COMMON = 0xE08A,
        TIMESIG_CUT = 0xE08B,

        NOTEHEAD_NORMAL = 0xE0A4,
        NOTEHEAD_CROSS = 0xE0A9,
        NOTEHEAD_PLUS = 0xE0AF,
        NOTEHEAD_XCIRCLE = 0xE0B3,
        NOTEHEAD_WITHX = 0xE0B7,
        NOTEHEAD_TRIANGLE_UP = 0xE0BE,
        NOTEHEAD_TRIANGLE_DOWN = 0xE0C7,
        NOTEHEAD_SLASHED1 = 0xE0CF,
        NOTEHEAD_SLASHED2 = 0xE0D0,
        NOTEHEAD_DIAMOND = 0xE0DB,
        NOTEHEAD_DIAMOND_OLD = 0xE0E2,
        NOTEHEAD_CIRCLED = 0xE0E4,
        NOTEHEAD_CIRCLED_LARGE = 0xE0E8,
        NOTEHEAD_LARGE_ARROW = 0xE0F0,

        NOTEHEAD_SLASH = 0xE101,
        NOTEHEAD_LARGE_DIAMOND = 0xE104,
        NOTEHEAD_SOL = 0xE1B1,
        NOTEHEAD_LA = 0xE1B3,
        NOTEHEAD_FA = 0xE1B5,
        NOTEHEAD_MI = 0xE1B9,
        NOTEHEAD_DO = 0xE1BB,
        NOTEHEAD_RE = 0xE1BD,
        NOTEHEAD_TI = 0xE1BF,

        NOTEHEAD_HEAVY_CROSS = 0xE0FA,
        NOTEHEAD_HEAVY_CROSS_HAT = 0xE0F9,

        SEMIBREVE = 0xE1D2,
        MINIM = 0xE1D3,
        CROTCHET = 0xE1D5,
        QUAVER = 0xE1D7,
        SEMIQUAVER = 0xE1D9,
        DEMISEMIQUAVER = 0xE1DB,
        DOT = 0xE1E7,

        MENSURAL_PROLATION_1 = 0xE910,
        MENSURAL_PROLATION_2 = 0xE911,
        MENSURAL_PROLATION_3 = 0xE912,
        MENSURAL_PROLATION_4 = 0xE913,
        MENSURAL_PROLATION_5 = 0xE914,
        MENSURAL_PROLATION_7 = 0xE916,
        MENSURAL_PROLATION_8 = 0xE917,
        MENSURAL_PROLATION_10 = 0xE919,
        MENSURAL_PROLATION_11 = 0xE91A,

        NOTEHEAD_BREVIS_ALT = 0xF43F,

        NONE
    };

    Q_ENUM(Code)
};

inline QString musicalSymbolToString(MusicalSymbolCodes::Code symbolCode, bool withDot = false)
{
    QString noteSymbol = QString(QChar(static_cast<char16_t>(symbolCode)));

    if (withDot) {
        noteSymbol += QChar(static_cast<char16_t>(MusicalSymbolCodes::Code::DOT));
    }

    return noteSymbol;
}
}

#endif // MU_UI_MUSICALSYMBOLCODE_H
