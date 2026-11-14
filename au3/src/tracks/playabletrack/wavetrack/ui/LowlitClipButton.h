/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  LowlitClipButton.h

  Describes the buttons that appear on the audio clips in their "lowlit" state,
  i.e., the opposite of the "highlit", or "highlighted", state. These buttons
  are highlit when the mouse hovers over them, and lowlit otherwise.

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "ClipButtonId.h"
#include "ClipParameters.h"
#include "UIHandle.h"
#include "WaveTrack.h"

class wxRect;
class wxPoint;
class wxDC;
class ClipInterface;
class ZoomInfo;

struct ClipButtonDrawingArgs
{
    const wxRect& rect;
    const ClipInterface& clip;
    wxDC& dc;
};

template<ClipButtonId id> struct ClipButtonSpecializations
{
    static bool NeedsDrawing(const ClipInterface&);
    static void DrawOnClip(ClipButtonDrawingArgs&);
    static int GetWidth(const ClipInterface& clip);
};

template<>
struct ClipButtonSpecializations<ClipButtonId::Overflow>
{
    static bool NeedsDrawing(const ClipInterface& clip);
    static void DrawOnClip(ClipButtonDrawingArgs&);
    static int GetWidth(const ClipInterface& clip);
};

template<>
struct ClipButtonSpecializations<ClipButtonId::Pitch>
{
    static bool NeedsDrawing(const ClipInterface& clip);
    static void DrawOnClip(ClipButtonDrawingArgs&);
    static int GetWidth(const ClipInterface& clip);
};

template<>
struct ClipButtonSpecializations<ClipButtonId::Speed>
{
    static bool NeedsDrawing(const ClipInterface& clip);
    static void DrawOnClip(ClipButtonDrawingArgs&);
    static int GetWidth(const ClipInterface& clip);
};

namespace LowlitClipButton {
struct RectangleArgs
{
    const ClipInterface& clip;
    const ZoomInfo& zoomInfo;
    // The upper ribbon of a track's rectangle, where the affordances of one or
    // more clips are drawn.
    const wxRect& trackAffordanceRect;
};

namespace Detail {
std::optional<wxRect>
GetButtonRectangle(ClipButtonId buttonId, const RectangleArgs& args);

std::optional<wxRect>
GetButtonInnerRectangle(ClipButtonId buttonId, const RectangleArgs& args);
} // namespace Detail

template<ClipButtonId id>
bool HitTest(const RectangleArgs& args, const wxPoint& mousePos)
{
    if (!ClipButtonSpecializations<id>::NeedsDrawing(args.clip)) {
        return false;
    }
    const auto buttonRect = Detail::GetButtonRectangle(id, args);
    return buttonRect.has_value() && buttonRect->Contains(mousePos);
}

template<ClipButtonId id>
std::optional<wxRect> DrawOnClip(const RectangleArgs& args, wxDC& dc)
{
    if (!ClipButtonSpecializations<id>::NeedsDrawing(args.clip)) {
        return {}
    }
    const auto rect = Detail::GetButtonInnerRectangle(id, args);
    if (!rect) {
        return {}
    }
    ClipButtonDrawingArgs drawingArgs { *rect, args.clip, dc };
    ClipButtonSpecializations<id>::DrawOnClip(drawingArgs);
    return rect;
}
} // namespace LowlitClipButton
