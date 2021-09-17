// Mac, Win and Linux all support 32 x 32 cursors now.

// Cursors MUST be 32x32 (or they will be resized).
// You only get black, white and transparent to use.

// N.B. Under windows the mask color must be in 
// the color palette.  To cater for Win2K a mask
// color of saturated red was used.  (middle grey 



#include "Cursors32/CrosshairCursor.xpm"
#include "Cursors32/DisabledCursor.xpm"
#include "Cursors32/RearrangeCursor.xpm"
#include "Cursors32/RearrangingCursor.xpm"
#include "Cursors32/EnvCursor.xpm"
#include "Cursors32/TimeCursor.xpm"
#include "Cursors32/IBeamCursor.xpm"
#include "Cursors32/DrawCursor.xpm"
#include "Cursors32/ZoomInCursor.xpm"
#include "Cursors32/ZoomOutCursor.xpm"
#include "Cursors32/LabelCursorLeft.xpm"
#include "Cursors32/LabelCursorRight.xpm"
#include "Cursors32/BottomFrequencyCursor.xpm"
#include "Cursors32/TopFrequencyCursor.xpm"
#include "Cursors32/BandWidthCursor.xpm"
#ifdef USE_MIDI
#include "Cursors32/StretchCursor.xpm"
#include "Cursors32/StretchLeftCursor.xpm"
#include "Cursors32/StretchRightCursor.xpm"
#endif
#include "Cursors32/SubViewsCursor.xpm"

AUDACITY_DLL_API
std::unique_ptr<wxCursor> MakeCursor(int WXUNUSED(CursorId), const char * const pXpm[36], int HotX, int HotY);
