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

#ifndef ICONCODE_H
#define ICONCODE_H

#include <QtCore/QtCore>
#include <QtQml/qqmlregistration.h>

/**
 * @brief The IconCode class simplifies access to the icons from the icon font
 *
 * @details Each enum value is a UTF-16-like address of the icon in the icon font.
 *          The current icon-font (MusescoreIcon.ttf) is located in the 'Audacity/fonts' folder,
 *          The most actual version can be found by this persistent URL: @link https://www.dropbox.com/s/ip59ren10u69hr7/MusescoreIcon.ttf?dl=0
 */
class IconCode
{
   Q_GADGET
   QML_NAMED_ELEMENT(IconCode)
   QML_UNCREATABLE("")

public:
   enum class Code : char16_t {
      SMALL_ARROW_DOWN = 0xEF12,
      MENU_THREE_DOTS = 0xEF13,

      ZOOM_OUT = 0xEF16,
      ZOOM_IN = 0xEF18,

      LOOP = 0xEF1F,
      METRONOME = 0xEF20,

      SPEAKER_ON = 0xEF4E,
      SPEAKER_OFF = 0xEF52,
      VISIBILITY_ON = 0xEF53,
      VISIBILITY_OFF = 0xEF54,
      SETUP = 0xEF55,

      TIME_SIGNATURE = 0xEF64,

      // TODO: Replace temporary TIMECODE value
      TIMECODE = SETUP,

      TOOLBAR_GRIP = 0xF3A2,

      TV = 0xF35E,
      FLAG = 0xF3C7,

      AUTOMATION = 0xF3CB,

      MUTE = 0xF3D5,
      SOLO = 0xF3D6,

      MICROPHONE = 0xF41B,

      ZOOM_TOGGLE = 0xF437,
      ZOOM_FIT_PROJECT = 0xF438,
      ZOOM_FIT_SELECTION = 0xF439,

      SILENCE = 0xF43A,
      TRIM = 0xF43B,

      MAGNET = 0xF43E,

      SPECTRAL_EDITING = 0xF442,
      SPECTRAL_BOX_SELECT = 0xF443,
      SPECTRAL_BRUSH = 0xF444,
      SPECTRAL_LASSO = 0xF445,
      SOLID_PLAY = 0xF446,
      SOLID_STOP = 0xF447,
      SOLID_REWIND = 0xF448,
      SOLID_FAST_FORWARD = 0xF449,
      SOLID_RECORD = 0xF44A,
      SOLID_PAUSE = 0xF44B,

      NONE = 0xFFFF
   };

   Q_ENUM(Code)
};

static inline QChar IconCodeToChar(IconCode::Code code)
{
   return QChar(static_cast<char16_t>(code));
}

#endif // ICONCODE_H
