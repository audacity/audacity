#ifndef AUDACITY_CURSORS_H
#define AUDACITY_CURSORS_H

#include "../src/Audacity.h"
#include "../src/MemoryX.h"

extern const char ** DisabledCursorXpm;
extern const char ** EnvCursorXpm;
extern const char ** TimeCursorXpm;
extern const char ** IBeamCursorXpm;
extern const char ** DrawCursorXpm;
extern const char ** ZoomInCursorXpm;
extern const char ** ZoomOutCursorXpm;
extern const char ** BottomFrequencyCursorXpm;
extern const char ** TopFrequencyCursorXpm;
extern const char ** BandWidthCursorXpm;
extern const char ** StretchCursorXpm;
extern const char ** StretchLeftCursorXpm;
extern const char ** StretchRightCursorXpm;

std::unique_ptr<wxCursor> MakeCursor(int WXUNUSED(CursorId), const char ** pXpm, int HotX, int HotY);

#endif //AUDACITY_CURSORS_H
