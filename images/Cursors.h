// Mac only supports 16x16 pixel cursors.
#ifdef __WXMAC__
#define CURSORS_SIZE16
#else
#define CURSORS_SIZE32
#endif


#ifdef CURSORS_SIZE32

// Cursors MUST be 32x32 (or they will be resized).
// You only get black, white and transparent to use.

// N.B. Under windows the mask color must be in 
// the color palette.  To cater for Win2K a mask
// color of saturated red was used.  (middle grey 



#include "Cursors32/DisabledCursor.xpm"
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

#else

#include "Cursors16/DisabledCursor.xpm"
#include "Cursors16/EnvCursor.xpm"
#include "Cursors16/TimeCursor.xpm"
#include "Cursors16/IBeamCursor.xpm"
#include "Cursors16/DrawCursor.xpm"
#include "Cursors16/ZoomInCursor.xpm"
#include "Cursors16/ZoomOutCursor.xpm"
#include "Cursors16/LabelCursorLeft.xpm"
#include "Cursors16/LabelCursorRight.xpm"
#include "Cursors16/BottomFrequencyCursor.xpm"
#include "Cursors16/TopFrequencyCursor.xpm"
#include "Cursors16/BandWidthCursor.xpm"
#ifdef USE_MIDI
#include "Cursors16/StretchCursor.xpm"
#include "Cursors16/StretchLeftCursor.xpm"
#include "Cursors16/StretchRightCursor.xpm"
#endif

#endif

std::unique_ptr<wxCursor> MakeCursor(int WXUNUSED(CursorId), const char * pXpm[36], int HotX, int HotY);
