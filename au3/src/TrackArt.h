/**********************************************************************

  Audacity: A Digital Audio Editor

  @file TrackArt.h

  Paul Licameli split from TrackArtist.h

**********************************************************************/
#ifndef __AUDACITY_TRACK_ART__
#define __AUDACITY_TRACK_ART__

class Channel;
class Track;
struct TrackPanelDrawingContext;
class wxBrush;
class wxDC;
class wxRect;
class wxString;
#include <wx/types.h>

namespace TrackArt {
static constexpr int ClipFrameRadius{ 6 };

//Draws clip affordance and title string, if not empty.
//Returns affordance's rectangle.
AUDACITY_DLL_API
wxRect DrawClipAffordance(
    wxDC& dc, const wxRect& clipRect, bool highlight = false, bool selected = false);

AUDACITY_DLL_API
bool
DrawClipTitle(wxDC& dc, const wxRect& affordanceRect, const wxString& title);

AUDACITY_DLL_API
void DrawClipEdges(wxDC& dc, const wxRect& clipRect, bool selected = false);

//Used to draw clip boundaries without contents/details when it's not
//sensible to show them
AUDACITY_DLL_API
void DrawClipFolded(wxDC& dc, const wxRect& rect);

// Helper: draws the "sync-locked" watermark tiled to a rectangle
AUDACITY_DLL_API
void DrawSyncLockTiles(
    TrackPanelDrawingContext& context, const wxRect& rect);

//TODO: refactor
/// @brief Helper: draws background with selection rect
/// @param useBeatsAlternateColor is only meaningful in Beats mode
AUDACITY_DLL_API
void DrawBackgroundWithSelection(TrackPanelDrawingContext& context, const wxRect& rect, const Channel& channel, const wxBrush& selBrush,
                                 const wxBrush& unselBrush, bool useSelection = true, bool useBeatsAlternateColor = false);

AUDACITY_DLL_API
void DrawCursor(TrackPanelDrawingContext& context, const wxRect& rect, const Track* track);

AUDACITY_DLL_API
void DrawNegativeOffsetTrackArrows(TrackPanelDrawingContext& context, const wxRect& rect);

AUDACITY_DLL_API
wxString TruncateText(wxDC& dc, const wxString& text, const int maxWidth);

AUDACITY_DLL_API
void DrawSnapLines(wxDC* dc, wxInt64 snap0, wxInt64 snap1);
}

extern AUDACITY_DLL_API int GetWaveYPos(float value, float min, float max, int height, bool dB, bool outer, float dBr, bool clip);
extern float FromDB(float value, double dBRange);
extern AUDACITY_DLL_API float ValueOfPixel(int yy, int height, bool offset, bool dB, double dBRange, float zoomMin, float zoomMax);

#endif
