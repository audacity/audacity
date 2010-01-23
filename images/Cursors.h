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
#endif
