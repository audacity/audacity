/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ClipPitchAndSpeedButtonHandle.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "ClipPitchAndSpeedButtonHandle.h"
#include "AllThemeResources.h"
#include "BasicUI.h"
#include "HitTestResult.h"
#include "LowlitClipButton.h"
#include "PitchAndSpeedDialog.h"
#include "Project.h"
#include "ProjectHistory.h"
#include "RefreshCode.h"
#include "Theme.h"
#include "TimeStretching.h"
#include "TimeAndPitchInterface.h"
#include "TrackPanelMouseEvent.h"
#include "WaveClip.h"
#include "WaveClipUIUtilities.h"
#include "WaveTrackUtilities.h"
#include "wxWidgetsWindowPlacement.h"
#include <wx/dc.h>

namespace {
wxString GetPitchShiftText(int clipCentShift)
{
    wxString pitchShiftText;
    if (clipCentShift != 0) {
        pitchShiftText = wxString::Format("%.2f", std::abs(clipCentShift) / 100.);
        while (pitchShiftText.EndsWith("0")) {
            pitchShiftText.RemoveLast();
        }
        if (pitchShiftText.EndsWith(".") || pitchShiftText.EndsWith(",")) {
            pitchShiftText.RemoveLast();
        }
    }
    return pitchShiftText;
}

wxString GetPlaybackSpeedText(double clipStretchRatio)
{
    if (TimeAndPitchInterface::IsPassThroughMode(clipStretchRatio)) {
        return {}
    }

    // clang-format off
    // We reckon that most of the time, a rounded percentage value is sufficient.
    // There are two exceptions:
    // - The clip is only slightly stretched such that the rounded value is 100.
    //   There should be an indicator if and only if the clip is stretched, this
    //   must be reliable. Yet showing "100%" would be confusing. Hence in that
    //   case we constrain the values to [99.1, ..., 99.9, 100.1,
    //   ..., 100.9], i.e., omitting 100.0.
    // - The clip is stretched so much that the playback speed is less than 1%.
    //   Make sure in that case that we never show 0%, but always at least 0.1%.
    // clang-format on

    const auto playbackSpeed = 100 / clipStretchRatio;
    wxString fullText;

    // We compare with .95 rather than 1, since playback speeds within [100.95,
    // 101) get rounded to 101.0 and (99, 99.05] to 99.0 by `wxString::Format`.
    // Let these be processed by the integer-display branch of this if statement
    // instead.
    if (fabs(playbackSpeed - 100.) < .95) {
        // Never show 100.0%
        fullText += wxString::Format(
            "%.1f%%", playbackSpeed > 100 ? std::max(playbackSpeed, 100.1)
            : std::min(playbackSpeed, 99.9));
    } else if (playbackSpeed < 1) {
        // Never show 0.0%
        fullText += wxString::Format("%.1f%%", std::max(playbackSpeed, 0.1));
    } else {
        const auto roundedPlaybackSpeed
            =static_cast<int>(std::round(playbackSpeed));
        fullText += wxString::Format("%d%%", roundedPlaybackSpeed);
    }
    return fullText;
}

void DrawPitchOrSpeedIconIfItFits(
    wxDC& dc, const wxRect& rect, const wxBitmap& icon, const wxString& text)
{
    const auto textExtent = dc.GetTextExtent(text);
    const auto textWidth = textExtent.GetWidth();
    const auto textHeight = textExtent.GetHeight();

    const auto iconHeight = icon.GetHeight();
    const auto iconWidth = textWidth == 0 ? 0 : icon.GetWidth();
    const auto contentWidth = iconWidth + textWidth;
    if (contentWidth == 0 || contentWidth > rect.width) {
        return;
    }
    const auto height = rect.GetHeight();
    const auto iconTop = rect.GetTop() + (height - iconHeight) / 2;
    const auto x = rect.x + (rect.width - contentWidth) / 2;
    dc.DrawBitmap(icon, x, iconTop);
    const auto y = rect.GetTop() + (height - textHeight) / 2;
    dc.DrawText(text, x + iconWidth, y);
}
} // namespace

ClipPitchAndSpeedButtonHandle::ClipPitchAndSpeedButtonHandle(
    Type type, const std::shared_ptr<WaveTrack>& track,
    const std::shared_ptr<WaveTrack::Interval>& clip)
    : HighlitClipButtonHandle{type == Type::Pitch ? ClipButtonId::Pitch
                              : ClipButtonId::Speed,
                              track, clip}
    , mType{type}
{
}

UIHandle::Result ClipPitchAndSpeedButtonHandle::DoRelease(
    const TrackPanelMouseEvent& event, AudacityProject* pProject,
    wxWindow* pParent)
{
    if (event.event.CmdDown()) {
        if (mType == Type::Pitch) {
            mClip->SetCentShift(0);
            ProjectHistory::Get(*pProject).PushState(
                XO("Reset Clip Pitch"), XO("Reset Clip Pitch"));
        } else if (!TimeStretching::SetClipStretchRatio(*mTrack, *mClip, 1)) {
            BasicUI::ShowErrorDialog(
                wxWidgetsWindowPlacement { pParent }, XO("Not enough space"),
                XO("There is not enough space to expand the clip to its original speed."),
                {});
            return RefreshCode::RefreshNone;
        } else {
            WaveClipUIUtilities::SelectClip(*pProject, *mClip);
            ProjectHistory::Get(*pProject).PushState(
                XO("Reset Clip Speed"), XO("Reset Clip Speed"));
        }
    } else {
        const auto focusedGroup = mType == Type::Pitch
                                  ? PitchAndSpeedDialogGroup::Pitch
                                  : PitchAndSpeedDialogGroup::Speed;
        BasicUI::CallAfter([project = pProject->weak_from_this(), track = mTrack,
                            clip = mClip, focusedGroup] {
            if (auto pProject = project.lock()) {
                PitchAndSpeedDialog::Get(*pProject)
                .Retarget(track, clip)
                .SetFocus(focusedGroup);
            }
        });
    }
    return RefreshCode::RefreshNone;
}

HitTestPreview ClipPitchAndSpeedButtonHandle::Preview(
    const TrackPanelMouseState& state, AudacityProject* pProject)
{
    const auto ctrlDown = state.state.CmdDown();
    const bool macOs = wxPlatformInfo::Get().GetOperatingSystemId() & wxOS_MAC;
    if (mType == Type::Pitch) {
        return { ctrlDown ? XO("Click to reset clip pitch.")
                 : macOs ? XO("Click to change clip pitch, Cmd + click to reset.")
                 : XO("Click to change clip pitch, Ctrl + click to reset."),
                 nullptr }
    } else {
        return { ctrlDown ? XO("Click to reset clip speed.")
                 : macOs ? XO("Click to change clip speed, Cmd + click to reset.")
                 : XO("Click to change clip speed, Ctrl + click to reset."),
                 nullptr }
    }
}

void ClipPitchAndSpeedButtonHandle::DoDraw(const wxRect& rect, wxDC& dc)
{
    const ClipInterface& clip = *mClip;
    ClipButtonDrawingArgs args { rect, clip, dc };
    if (mType == Type::Pitch) {
        ClipButtonSpecializations<ClipButtonId::Pitch>::DrawOnClip(args);
    } else {
        ClipButtonSpecializations<ClipButtonId::Speed>::DrawOnClip(args);
    }
}

int ClipButtonSpecializations<ClipButtonId::Pitch>::GetWidth(
    const ClipInterface& clip)
{
    // If we are to show some decimals, reserve a bit more space.
    return clip.GetCentShift() % 100 == 0 ? 32 : 55;
}

int ClipButtonSpecializations<ClipButtonId::Speed>::GetWidth(
    const ClipInterface&)
{
    return 60;
}

bool ClipButtonSpecializations<ClipButtonId::Pitch>::NeedsDrawing(
    const ClipInterface& clip)
{
    return clip.GetCentShift() != 0;
}

bool ClipButtonSpecializations<ClipButtonId::Speed>::NeedsDrawing(
    const ClipInterface& clip)
{
    return !TimeAndPitchInterface::IsPassThroughMode(clip.GetStretchRatio());
}

void ClipButtonSpecializations<ClipButtonId::Pitch>::DrawOnClip(
    ClipButtonDrawingArgs& args)
{
    const auto& clip = args.clip;
    const auto& rect = args.rect;
    auto& dc = args.dc;
    const auto clipCentShift = clip.GetCentShift();
    if (clipCentShift == 0) {
        return;
    }
    const auto pitchText = GetPitchShiftText(clipCentShift);
    DrawPitchOrSpeedIconIfItFits(
        dc, rect,
        theTheme.Bitmap(
            clipCentShift > 0 ? pitchUpIndicator : pitchDownIndicator),
        pitchText);
}

void ClipButtonSpecializations<ClipButtonId::Speed>::DrawOnClip(
    ClipButtonDrawingArgs& args)
{
    const auto& clip = args.clip;
    const auto& rect = args.rect;
    auto& dc = args.dc;
    const auto clipStretchRatio = clip.GetStretchRatio();
    if (TimeAndPitchInterface::IsPassThroughMode(clipStretchRatio)) {
        return;
    }
    const auto speedText = GetPlaybackSpeedText(clipStretchRatio);
    DrawPitchOrSpeedIconIfItFits(
        dc, rect, theTheme.Bitmap(speedIndicator), speedText);
}
