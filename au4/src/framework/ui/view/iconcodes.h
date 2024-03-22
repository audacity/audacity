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

#ifndef MU_UI_ICONCODE_H
#define MU_UI_ICONCODE_H

#include <QObject>

#ifdef WARNING
#undef WARNING
#endif
#ifdef INFO
#undef INFO
#endif
#ifdef ERROR
#undef ERROR
#endif
#ifdef QUESTION
#undef QUESTION
#endif

#ifdef FILE_OPEN
#undef FILE_OPEN
#endif

namespace mu::ui {
/**
 * @brief The IconCode class simplifies access to the icons from the icon font
 *
 * @details Each enum value is a UTF-16-like address of the icon in the icon font.
 *          The current icon-font (MusescoreIcon.ttf) is located in the 'MuseScore/fonts/mscore' folder,
 *          The most actual version can be found by this persistent URL: @link https://www.dropbox.com/s/ip59ren10u69hr7/MusescoreIcon.ttf?dl=0
 */

class IconCode
{
    Q_GADGET

public:
    enum class Code : char16_t {
        SMALL_ARROW_UP = 0xEF10,
        SMALL_ARROW_RIGHT = 0xEF11,
        SMALL_ARROW_DOWN = 0xEF12,
        MENU_THREE_DOTS = 0xEF13,
        CLOSE_X_ROUNDED = 0xEF14,
        SPLIT_OUT_ARROWS = 0xEF15,
        ZOOM_OUT = 0xEF16,
        SEARCH = 0xEF17,
        ZOOM_IN = 0xEF18,
        UNDO = 0xEF19,
        REDO = 0xEF1A,
        PAGE_VIEW = 0xEF1B,
        CONTINUOUS_VIEW = 0xEF1C,
        PLAY = 0xEF1D,
        STOP = 0xEF1E,
        LOOP = 0xEF1F,
        METRONOME = 0xEF20,
        TUNING_FORK = 0xEF21,
        NEW_FILE = 0xEF22,
        OPEN_FILE = 0xEF23,
        SHARE_FILE = 0xEF24,
        CLOUD_FILE = 0xEF25,
        REWIND = 0xEF26,
        MIXER = 0xEF27,
        CONFIGURE = 0xEF28,
        SAVE = 0xEF29,
        PLUS = 0xEF2A,
        MINUS = 0xEF2B,
        DELETE_TANK = 0xEF2C,
        FEEDBACK = 0xEF2D,
        LINK = 0xEF2E,
        TICK = 0xEF2F,
        CROSS = 0xEF30,
        TICK_RIGHT_ANGLE = 0xEF31,
        HORIZONTAL = 0xEF32,
        VERTICAL = 0xEF33,
        ARROW_RIGHT = 0xEF34,
        ARROW_LEFT = 0xEF35,
        ARROW_DOWN = 0xEF36,
        ARROW_UP = 0xEF37,
        POSITION_ARROWS = 0xEF38,
        TEXT_ALIGN_BASELINE = 0xEF39,
        TEXT_ALIGN_BOTTOM = 0xEF3A,
        TEXT_ALIGN_TOP = 0xEF3B,
        TEXT_ALIGN_MIDDLE = 0xEF3C,
        TEXT_ALIGN_LEFT = 0xEF3D,
        TEXT_ALIGN_CENTER = 0xEF3E,
        TEXT_ALIGN_RIGHT = 0xEF3F,
        TEXT_ITALIC = 0xEF40,
        TEXT_UNDERLINE = 0xEF41,
        TEXT_BOLD = 0xEF42,
        TEXT_STRIKE = 0xF424,
        APPLY_GLOBAL_STYLE = 0xEF43,
        HAIRPIN = 0xEF44,
        ACCIDENTAL_SHARP = 0xEF45,
        SLUR = 0xEF46,
        DYNAMIC_FORTE = 0xEF47,
        CRESCENDO_LINE = 0xEF48,
        FRAME_SQUARE = 0xEF49,
        FRAME_CIRCLE = 0xEF4A,
        MUSIC_NOTES = 0xEF4B,
        TEXT_SUBSCRIPT = 0xEF4C,
        TEXT_SUPERSCRIPT = 0xEF4D,
        AUDIO = 0xEF4E,
        EYE_OPEN = 0xEF53,
        EYE_CLOSED = 0xEF54,
        SETTINGS_COG = 0xEF55,
        BEAM_FEATHERING_RIGHT_HEIGHT = 0xEF56,
        BEAM_FEATHERING_LEFT_HEIGHT = 0xEF57,
        BEAM_HEIGHT_LEFT = 0xEF5A,
        BEAM_HEIGHT_RIGHT = 0xEF5B,
        LOCK_CLOSED = 0xEF5C,
        LOCK_OPEN = 0xEF5D,
        DOT_ABOVE_LINE = 0xEF5E,
        DOT_BELOW_LINE = 0xEF5F,
        TEXT_BELOW_STAFF = 0xEF60,
        TEXT_ABOVE_STAFF = 0xEF61,
        GLISSANDO = 0xEF62,
        EDIT = 0xEF63,
        TIME_SIGNATURE = 0xEF64,
        PEDAL_MARKING = 0xEF65,
        MARKER = 0xEF66,
        JUMP = 0xEF67,
        REPEAT_START = 0xEF68,
        FERMATA = 0xEF69,
        SECTION_BREAK = 0xEF6A,
        SPACER = 0xEF6B,
        VERTICAL_FRAME = 0xEF6C,
        HORIZONTAL_FRAME = 0xEF6D,
        TEXT_FRAME = 0xEF6E,
        ORNAMENT = 0xEF6F,
        ARTICULATION = 0xEF70,
        BRACKET = 0xEF71,
        BRACE = 0xEF72,
        CLEF_BASS = 0xEF73,
        MORTAR_BOARD = 0xEF74,
        FRETBOARD_DIAGRAM = 0xEF75,
        FRETBOARD_MARKER_TRIANGLE = 0xEF76,
        FRETBOARD_MARKER_CIRCLE_OUTLINE = 0xEF77,
        FRETBOARD_MARKER_CIRCLE_FILLED = 0xEF78,
        AMBITUS = 0xEF79,
        AMBITUS_LEANING_LEFT = 0xEF7A,
        AMBITUS_LEANING_RIGHT = 0xEF7B,
        BRACKET_PARENTHESES = 0xEF7C,
        BRACKET_PARENTHESES_SQUARE = 0xEF7D,
        SPLIT_VIEW_HORIZONTAL = 0xEF7F,
        SPLIT_VIEW_VERTICAL = 0xEF80,
        KEY_SIGNATURE = 0xEF81,
        LINE_DASHED = 0xEF82,
        LINE_WIDE_DASHED = 0xF434,
        LINE_DOTTED = 0xEF83,
        LINE_NORMAL = 0xEF84,
        LINE_WITH_END_HOOK = 0xEF85,
        LINE_WITH_ANGLED_END_HOOK = 0xEF86,
        LINE_PEDAL_STAR_ENDING = 0xEF87,
        BEAM_FEATHERED_ACCELERATE = 0xEF88,
        BEAM_FEATHERED_DECELERATE = 0xEF89,

        RIGHT_GAP = 0xEF90,
        LEFT_GAP = 0xEF91,
        GAP_BELOW = 0xEF92,
        GAP_ABOVE = 0xEF93,
        LEFT_MARGIN = 0xEF94,
        RIGHT_MARGIN = 0xEF95,
        BOTTOM_MARGIN = 0xEF96,
        TOP_MARGIN = 0xEF97,
        MIDI_INPUT = 0xEF98,
        ACCOUNT = 0xEF99,

        STAR = 0xEF9A,
        LINE_WITH_TWO_INVERTED_HOOKS = 0xEF9B,
        LINE_WITH_T_LIKE_END_HOOK = 0xEF9C,
        LINE_WITH_T_LINE_START_HOOK = 0xEF9D,
        LINE_WITH_START_HOOK = 0xEF9E,
        LINE_WITH_ANGLED_START_HOOK = 0xEF9F,
        LINE_WITH_INVERTED_START_HOOK = 0xEFA0,
        IMAGE_MOUNTAINS = 0xEFA1,

        GRADUATION_CAP = 0xF19D,

        SCORE = 0xF319,
        CLEF_TREBLE = 0xF31A,

        AUTO_TEXT = 0xF329,
        BEAM_NONE = 0xF33A,
        BEAM_BREAK_LEFT = 0xF33B,
        BEAM_JOIN = 0xF33D,
        BEAM_BREAK_INNER_8TH = 0xF33E,
        BEAM_BREAK_INNER_16TH = 0xF33F,

        QUESTION_MARK = 0xF340,

        NOTE_HEAD_QUARTER = 0xF341,
        NOTE_HEAD_HALF = 0xF342,
        NOTE_HEAD_WHOLE = 0xF343,
        NOTE_HEAD_BREVIS = 0xF344,
        NOTE_HEAD = 0xF42F,
        NOTE_HEAD_PARENTHESES = 0xF430,

        PLAY_REPEATS = 0xF345,
        BARLINE_WINGED = 0xF34C,
        BARLINE_UNWINGED = 0xF34D,
        ORIENTATION_PORTRAIT = 0xF350,
        ORIENTATION_LANDSCAPE = 0xF351,

        CHORD_SYMBOL = 0xF352,
        GUITAR_BEND = 0xF353,
        MULTIMEASURE_REST = 0xF355,
        IMPORT = 0xF357,
        UPDATE = 0xF358,
        OPEN_LINK = 0xF359,
        GUITAR_TREMOLO_BAR = 0xF35C,
        SHORTCUTS = 0xF35D,

        TREMOLO_TWO_NOTES = 0xF35F,
        TREMOLO_STYLE_DEFAULT = 0xF35F,
        TREMOLO_STYLE_TRADITIONAL = 0xF360,
        TREMOLO_ONE_NOTE = 0xF361,
        TREMOLO_STYLE_TRADITIONAL_ALTERNATE = 0xF362,

        LONGO = 0xF364,
        NOTE_WHOLE_DOUBLE = 0xF365,
        NOTE_WHOLE = 0xF366,
        NOTE_HALF = 0xF367,
        NOTE_QUARTER = 0xF368,
        NOTE_8TH = 0xF369,
        NOTE_16TH = 0xF36A,
        NOTE_32ND = 0xF36B,
        NOTE_64TH = 0xF36C,
        NOTE_128TH = 0xF36D,
        NOTE_256TH = 0xF36E,
        NOTE_512TH = 0xF36F,
        NOTE_1024TH = 0xF370,
        REST = 0xF371,
        NOTE_DOTTED = 0xF372,
        NOTE_TIE = 0xF373,
        NOTE_FLIP = 0xF374,
        NOTE_SLUR = 0xF375,
        NOTE_TUPLET = 0xF376,

        KEY_SIGNATURE_NONE = 0xF377,
        KEY_SIGNATURE_1_SHARP = 0xF378,
        KEY_SIGNATURE_2_SHARPS = 0xF379,
        KEY_SIGNATURE_3_SHARPS = 0xF37A,
        KEY_SIGNATURE_4_SHARPS = 0xF37B,
        KEY_SIGNATURE_5_SHARPS = 0xF37C,
        KEY_SIGNATURE_6_SHARPS = 0xF37D,
        KEY_SIGNATURE_7_SHARPS = 0xF37E,
        KEY_SIGNATURE_1_FLAT = 0xF37F,
        KEY_SIGNATURE_2_FLAT = 0xF380,
        KEY_SIGNATURE_3_FLAT = 0xF381,
        KEY_SIGNATURE_4_FLAT = 0xF382,
        KEY_SIGNATURE_5_FLAT = 0xF383,
        KEY_SIGNATURE_6_FLAT = 0xF384,
        KEY_SIGNATURE_7_FLAT = 0xF385,

        SHARP = 0xF386,
        NATURAL = 0xF387,
        FLAT = 0xF388,
        FLAT_DOUBLE = 0xF389,
        SHARP_DOUBLE = 0xF38A,
        MARCATO = 0xF38B,
        ACCENT = 0xF38C,
        TENUTO = 0xF38D,
        STACCATO = 0xF38E,

        BYPASS = 0xF38F,

        VOICE_1 = 0xF391,
        VOICE_2 = 0xF392,
        VOICE_3 = 0xF393,
        VOICE_4 = 0xF394,
        NOTE_DOTTED_2 = 0xF395,
        NOTE_DOTTED_3 = 0xF396,
        NOTE_DOTTED_4 = 0xF397,

        COPY = 0xF398,
        PASTE = 0xF399,
        CUT = 0xF39A,

        CHEVRON_RIGHT = 0xF39B,
        CHEVRON_LEFT = 0xF39C,

        NOTE_PLUS = 0xF39D,
        NOTE_TO_RIGHT = 0xF39E,
        RHYTHM_ONLY = 0xF39F,
        RE_PITCH = 0xF3A0,
        FOOT_PEDAL = 0xF3A1,

        TOOLBAR_GRIP = 0xF3A2,

        GRID = 0xF3A4,

        PAGE = 0xF3A5,

        LIST = 0xF3AA,

        CAMERA = 0xF3AE,

        CONTINUOUS_VIEW_VERTICAL = 0xF3AF,

        PAN_SCORE = 0xF3B0,
        COUNT_IN = 0xF3B1,

        GLOBE = 0xF3B6,
        PRINT = 0xF3B7,

        DOWN = 0xF3C1,
        UP = 0xF3C2,

        LOOP_IN = 0xF3C7,
        LOOP_OUT = 0xF3C8,
        PAUSE = 0xF3C9,

        WARNING = 0xF3CE,
        INFO = 0xF3CF,
        ERROR = 0xF3D0,
        QUESTION = 0xF3D1,

        MUTE = 0xF3D5,
        SOLO = 0xF3D6,

        OTTAVA = 0xF40D,
        PALM_MUTE = 0xF40E,
        LET_RING = 0xF40F,
        VOLTA = 0xF410,
        VIBRATO = 0xF411,
        DIMINUENDO = 0xF414,
        CRESCENDO = 0xF415,

        APP_MINIMIZE = 0xF41C,
        APP_MAXIMIZE = 0xF41D,
        APP_UNMAXIMIZE = 0xF41E,
        APP_CLOSE = 0xF41F,

        NOTEFLAGS_TRADITIONAL = 0xF420,
        NOTEFLAGS_STRAIGHT = 0xF421,

        TUPLET_NUMBER_WITH_BRACKETS = 0xF422,
        TUPLET_NUMBER_ONLY = 0xF423,

        USE_WIDE_BEAMS_REGULAR = 0xF425,
        USE_WIDE_BEAMS_WIDE = 0xF426,

        ACCIACCATURA = 0xF427,
        APPOGGIATURA = 0xF428,
        GRACE4 = 0xF429,
        GRACE16 = 0xF42A,
        GRACE32 = 0xF42B,
        GRACE8_AFTER = 0xF42C,
        GRACE16_AFTER = 0xF42D,
        GRACE32_AFTER = 0xF42E,

        MEASURE_REPEAT = 0xF431,

        INSERT_ONE_MEASURE = 0xF432,
        STAFF_TYPE_CHANGE = 0xF433,

        CLOUD = 0xF435,

        CROSS_STAFF_BEAMING = 0xF43D,

        TEMPO_CHANGE = 0xF43F,

        PLUGIN = 0xF440,
        LYRICS = 0xF441,

        STOP_FILL = 0xF447,

        REST_8TH = 0xF44C,

        SHARE_AUDIO = 0xF44F,

        DYNAMIC_CENTER_1 = 0xF451,
        DYNAMIC_CENTER_2 = 0xF452,
        EXPRESSION = 0xF453,

        CLOUD_FILL = 0xF454,
        BRAILLE = 0xF455,

        GUITAR_BEND_REGULAR = 0xF45F,
        GUITAR_PRE_BEND = 0xF460,
        GUITAR_GRACE_NOTE_BEND = 0xF461,
        GUITAR_SLIGHT_BEND = 0xF462,
        GUITAR_BEND_STYLE_1 = 0xF463,
        GUITAR_BEND_STYLE_FULL = 0xF464,

        TIE_OUTSIDE = 0xF45D,
        TIE_INSIDE = 0xF45E,
        TIE_CHORD_OUTSIDE = 0xF466,
        TIE_CHORD_INSIDE = 0xF467,

        TRIANGLE_SYMBOL = 0xF46D,

        NONE = 0xFFFF
    };

    Q_ENUM(Code)
};

static inline QChar iconCodeToChar(IconCode::Code code)
{
    return QChar(static_cast<char16_t>(code));
}
}

#endif // MU_UI_ICONCODE_H
