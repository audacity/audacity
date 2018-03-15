/**********************************************************************

  Audacity: A Digital Audio Editor

  Cursors.cpp

  Dominic Mazzoni
  and lots of other contributors

********************************************************************/

// Mac, Win and Linux all support 32 x 32 cursors now.

// Cursors MUST be 32x32 (or they will be resized).
// You only get black, white and transparent to use.

// N.B. Under windows the mask color must be in 
// the color palette.  To cater for Win2K a mask
// color of saturated red was used.  (middle grey 

#include "../images/Cursors.h"

#include "../images/Cursors32/DisabledCursor.xpm"
#include "../images/Cursors32/EnvCursor.xpm"
#include "../images/Cursors32/TimeCursor.xpm"
#include "../images/Cursors32/IBeamCursor.xpm"
#include "../images/Cursors32/DrawCursor.xpm"
#include "../images/Cursors32/ZoomInCursor.xpm"
#include "../images/Cursors32/ZoomOutCursor.xpm"
#include "../images/Cursors32/BottomFrequencyCursor.xpm"
#include "../images/Cursors32/TopFrequencyCursor.xpm"
#include "../images/Cursors32/BandWidthCursor.xpm"
#ifdef USE_MIDI
#include "../images/Cursors32/StretchCursor.xpm"
#include "../images/Cursors32/StretchLeftCursor.xpm"
#include "../images/Cursors32/StretchRightCursor.xpm"
#endif // USE_MIDI

#include <wx/image.h>
#include <wx/bitmap.h>
#include <wx/cursor.h>

const char ** DisabledCursorXpm = DisabledCursor_xpm;
const char ** EnvCursorXpm = EnvCursor_xpm;
const char ** TimeCursorXpm = TimeCursor_xpm;
const char ** IBeamCursorXpm = IBeamCursor_xpm;
const char ** DrawCursorXpm = DrawCursor_xpm;
const char ** ZoomInCursorXpm = ZoomInCursor_xpm;
const char ** ZoomOutCursorXpm = ZoomOutCursor_xpm;
const char ** BottomFrequencyCursorXpm = BottomFrequencyCursor_xpm;
const char ** TopFrequencyCursorXpm = TopFrequencyCursor_xpm;
const char ** BandWidthCursorXpm = BandWidthCursor_xpm;
#ifdef USE_MIDI
const char ** StretchCursorXpm = StretchCursor_xpm;
const char ** StretchLeftCursorXpm = StretchLeftCursor_xpm;
const char ** StretchRightCursorXpm = StretchRightCursor_xpm;
#endif // USE_MIDI

std::unique_ptr<wxCursor> MakeCursor( int WXUNUSED(CursorId), const char ** pXpm,  int HotX, int HotY )
{
   wxImage Image = wxImage(wxBitmap(pXpm).ConvertToImage());
   Image.SetMaskColour(255,0,0);
   Image.SetMask();// Enable mask.

   Image.SetOption( wxIMAGE_OPTION_CUR_HOTSPOT_X, HotX );
   Image.SetOption( wxIMAGE_OPTION_CUR_HOTSPOT_Y, HotY );
   return std::make_unique<wxCursor>( Image );
}

