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
#include "utils.h"

#include "translation.h"

using namespace mu;

static constexpr const char* sharpNotes[] = {
    QT_TRANSLATE_NOOP("global", "C"),
    QT_TRANSLATE_NOOP("global", "C♯"),
    QT_TRANSLATE_NOOP("global", "D"),
    QT_TRANSLATE_NOOP("global", "D♯"),
    QT_TRANSLATE_NOOP("global", "E"),
    QT_TRANSLATE_NOOP("global", "F"),
    QT_TRANSLATE_NOOP("global", "F♯"),
    QT_TRANSLATE_NOOP("global", "G"),
    QT_TRANSLATE_NOOP("global", "G♯"),
    QT_TRANSLATE_NOOP("global", "A"),
    QT_TRANSLATE_NOOP("global", "A♯"),
    QT_TRANSLATE_NOOP("global", "B")
};

static constexpr const char* flatNotes[] = {
    QT_TRANSLATE_NOOP("global", "C"),
    QT_TRANSLATE_NOOP("global", "D♭"),
    QT_TRANSLATE_NOOP("global", "D"),
    QT_TRANSLATE_NOOP("global", "E♭"),
    QT_TRANSLATE_NOOP("global", "E"),
    QT_TRANSLATE_NOOP("global", "F"),
    QT_TRANSLATE_NOOP("global", "G♭"),
    QT_TRANSLATE_NOOP("global", "G"),
    QT_TRANSLATE_NOOP("global", "A♭"),
    QT_TRANSLATE_NOOP("global", "A"),
    QT_TRANSLATE_NOOP("global", "B♭"),
    QT_TRANSLATE_NOOP("global", "B")
};

std::string mu::pitchToString(int pitch, bool addoctave, bool useFlats /* = false */)
{
    if (pitch < 0 || pitch > 127) {
        return std::string();
    }

    auto source = useFlats ? flatNotes : sharpNotes;

    int i = pitch % 12;
    if (addoctave) {
        int octave = (pitch / 12) - 1;
        return trc("global", source[i]) + std::to_string(octave);
    }
    return trc("global", source[i]);
}
