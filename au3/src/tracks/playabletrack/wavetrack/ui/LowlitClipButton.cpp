/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  LowlitClipButton.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "LowlitClipButton.h"
#include "ClipParameters.h"

namespace {
constexpr auto widthReservedForTitle = 50;
constexpr auto topMargin = 4;
constexpr auto leftMargin = 0;
constexpr auto bottomMargin = 1;

wxRect RemoveSpaceReservedForTitle(const wxRect& rect)
{
    const auto toRemove = std::min(rect.width, widthReservedForTitle);
    return { rect.x + toRemove, rect.y, rect.width - toRemove, rect.height };
}

int GetButtonWidth(ClipButtonId buttonId, const ClipInterface& clip)
{
    return buttonId == ClipButtonId::Overflow
           ? ClipButtonSpecializations<ClipButtonId::Overflow>::GetWidth(clip)
           : buttonId == ClipButtonId::Speed
           ? ClipButtonSpecializations<ClipButtonId::Speed>::GetWidth(clip)
           : ClipButtonSpecializations<ClipButtonId::Pitch>::GetWidth(clip);
}
} // namespace

std::optional<wxRect> LowlitClipButton::Detail::GetButtonRectangle(
    ClipButtonId buttonId, const LowlitClipButton::RectangleArgs& args)
{
    const auto clipRect = ClipParameters::GetClipRect(
        args.clip, args.zoomInfo, args.trackAffordanceRect);
    const auto rect
        =RemoveSpaceReservedForTitle(clipRect.Intersect(args.trackAffordanceRect));

    // In this order, from left to right: pitch (maybe), speed (maybe), overflow.
    auto offset = 0;
    if (buttonId != ClipButtonId::Overflow) {
        offset
            +=ClipButtonSpecializations<ClipButtonId::Overflow>::GetWidth(args.clip);
        if (
            buttonId == ClipButtonId::Pitch
            && ClipButtonSpecializations<ClipButtonId::Speed>::NeedsDrawing(
                args.clip)) {
            offset
                +=ClipButtonSpecializations<ClipButtonId::Speed>::GetWidth(args.clip);
        }
    }

    const auto buttonWidth = GetButtonWidth(buttonId, args.clip);
    if (rect.width < buttonWidth + offset) {
        return {}
    }
    const auto x = rect.x + rect.width - offset - buttonWidth;
    return std::make_optional<wxRect>(x, rect.y, buttonWidth, rect.height);
}

std::optional<wxRect> LowlitClipButton::Detail::GetButtonInnerRectangle(
    ClipButtonId buttonId, const LowlitClipButton::RectangleArgs& args)
{
    const auto outerRect = GetButtonRectangle(buttonId, args);
    if (!outerRect.has_value()) {
        return {}
    }
    const auto rightMargin = buttonId == ClipButtonId::Overflow ? 5 : 2;
    return wxRect { outerRect->x + leftMargin, outerRect->y + topMargin,
                    outerRect->width - leftMargin - rightMargin,
                    outerRect->height - topMargin - bottomMargin };
}
