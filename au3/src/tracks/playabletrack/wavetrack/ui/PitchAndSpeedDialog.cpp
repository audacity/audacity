/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file PitchAndSpeedDialog.cpp

 Dmitry Vedenko

 **********************************************************************/
#include "PitchAndSpeedDialog.h"
#include "AudioIO.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectHistory.h"
#include "ProjectWindow.h"
#include "ProjectWindows.h"
#include "TimeAndPitchInterface.h"
#include "TimeStretching.h"
#include "TrackPanel.h"
#include "TrackPanelMouseEvent.h"
#include "UndoManager.h"
#include "ViewInfo.h"
#include "WaveClip.h"
#include "WaveClipUIUtilities.h"
#include "WaveTrackUtilities.h"
#include "WindowAccessible.h"

#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/layout.h>
#include <wx/spinctrl.h>
#include <wx/textctrl.h>

#include "ShuttleGui.h"
#include "ShuttleGuiScopedSizer.h"
#include "SpinControl.h"
#include "WaveClip.h"
#include "wxWidgetsWindowPlacement.h"

#include <regex>

namespace {
constexpr auto semitoneCtrlId = wxID_HIGHEST + 1;
constexpr auto speedCtrlId = wxID_HIGHEST + 3;

struct HitClip
{
    std::shared_ptr<WaveTrack> track;
    std::shared_ptr<WaveTrack::Interval> clip;
};

std::optional<HitClip>
GetHitClip(AudacityProject& project, const TrackPanelMouseEvent& event)
{
    const auto pos = event.event.GetPosition();
    const auto& viewInfo = ViewInfo::Get(project);
    const auto t = viewInfo.PositionToTime(pos.x, event.rect.GetX());
    auto& trackPanel = TrackPanel::Get(project);
    for (auto leader : TrackList::Get(project).Any<WaveTrack>()) {
        const auto trackRect = trackPanel.FindTrackRect(leader);
        if (!trackRect.Contains(pos)) {
            continue;
        }
        auto [begin, end] = leader->Intervals();
        while (begin != end)
        {
            auto clip = *begin++;
            if (clip->WithinPlayRegion(t)) {
                return HitClip { std::static_pointer_cast<WaveTrack>(
                                     leader->SharedPointer()),
                                 clip }
            }
        }
    }
    return {};
}

bool IsExactlySelected(AudacityProject& project, const ClipTimes& clip)
{
    auto& viewInfo = ViewInfo::Get(project);
    return clip.GetPlayStartTime() == viewInfo.selectedRegion.t0()
           && clip.GetPlayEndTime() == viewInfo.selectedRegion.t1();
}

PitchAndSpeedDialog::PitchShift ToSemitonesAndCents(int oldCents, int newCents)
{
    // Rules:
    // 1. cents must be in the range [-100, 100]
    // 2. on semitone updates, keep semitone and cent sign consistent

    PitchAndSpeedDialog::PitchShift shift { 0, newCents };
    while (shift.cents <= -100)
    {
        --shift.semis;
        shift.cents += 100;
    }
    while (shift.cents >= 100)
    {
        ++shift.semis;
        shift.cents -= 100;
    }

    const auto onlySemitonesChanged = [](int oldCents, int newCents) {
        while (oldCents < 0) {
            oldCents += 100;
        }
        while (newCents < 0) {
            newCents += 100;
        }
        return oldCents / 100 != newCents / 100;
    }(oldCents, newCents);

    if (onlySemitonesChanged && shift.cents > 0 && shift.semis < 0) {
        return { shift.semis + 1, shift.cents - 100 }
    } else if (onlySemitonesChanged && shift.cents < 0 && shift.semis > 0) {
        return { shift.semis - 1, shift.cents + 100 }
    } else {
        return shift;
    }
}

void ClampPitchShift(PitchAndSpeedDialog::PitchShift& shift)
{
    static_assert(TimeAndPitchInterface::MaxCents % 100 == 0);
    static_assert(TimeAndPitchInterface::MinCents % 100 == 0);
    const auto cents = shift.semis * 100 + shift.cents;
    if (cents > TimeAndPitchInterface::MaxCents) {
        shift = { TimeAndPitchInterface::MaxCents / 100, 0 }
    } else if (cents < TimeAndPitchInterface::MinCents) {
        shift = { TimeAndPitchInterface::MinCents / 100, 0 }
    }
}

PitchAndSpeedDialog::PitchShift GetClipShift(const WaveClip& clip)
{
    const auto totalShift = clip.GetCentShift();
    return { totalShift / 100, totalShift % 100 };
}

static const AttachedWindows::RegisteredFactory key {
    [](AudacityProject& project) -> wxWeakRef<wxWindow> {
        return safenew PitchAndSpeedDialog(project);
    }
};
} // namespace

PitchAndSpeedDialog& PitchAndSpeedDialog::Get(AudacityProject& project)
{
    return GetAttachedWindows(project).Get<PitchAndSpeedDialog>(key);
}

const PitchAndSpeedDialog&
PitchAndSpeedDialog::Get(const AudacityProject& project)
{
    return Get(const_cast<AudacityProject&>(project));
}

void PitchAndSpeedDialog::Destroy(AudacityProject& project)
{
    auto& attachedWindows = GetAttachedWindows(project);
    auto* pPanel = attachedWindows.Find(key);
    if (pPanel) {
        pPanel->wxWindow::Destroy();
        attachedWindows.Assign(key, nullptr);
    }
}

PitchAndSpeedDialog::PitchAndSpeedDialog(AudacityProject& project)
    : wxDialogWrapper(
        FindProjectFrame(&project), wxID_ANY, XO("Pitch and Speed"), wxDefaultPosition,
        { 480, 250 }, wxDEFAULT_DIALOG_STYLE)
    , mProject{project}
    , mProjectCloseSubscription{ProjectWindow::Get(mProject).Subscribe(
                                    [this](ProjectWindowDestroyedMessage) { Destroy(mProject); })},
    mTitle { GetTitle() }
{
    Bind(wxEVT_CLOSE_WINDOW, [this](const auto&) { Show(false); });

    Bind(wxEVT_CHAR_HOOK, [this](wxKeyEvent& event) {
        if (event.GetKeyCode() == WXK_ESCAPE) {
            Show(false);
        } else {
            event.Skip();
        }
    });

    if (const auto audioIo = AudioIO::Get()) {
        mAudioIOSubscription
            =audioIo->Subscribe([this](const AudioIOEvent& event) {
            if (event.pProject != &mProject) {
                return;
            }
            switch (event.type) {
                case AudioIOEvent::CAPTURE:
                case AudioIOEvent::PLAYBACK:
                    if (const auto child = wxDialog::FindWindowById(speedCtrlId)) {
                        child->Enable(!event.on);
                    }
                    break;
                case AudioIOEvent::MONITOR:
                case AudioIOEvent::PAUSE:
                    break;
                default:
                    // Unknown event type
                    assert(false);
            }
        });
    }
}

void PitchAndSpeedDialog::TryRetarget(const TrackPanelMouseEvent& event)
{
    const auto target = GetHitClip(mProject, event);
    if (!target.has_value() || target->clip == mLeftClip.lock()) {
        return;
    }
    Retarget(target->track, target->clip);
}

PitchAndSpeedDialog& PitchAndSpeedDialog::Retarget(
    const std::shared_ptr<WaveTrack>& track,
    const WaveTrack::IntervalHolder& clip)
{
    mConsolidateHistory = false;
    wxDialog::SetTitle(mTitle + " - " + clip->GetName());
    const auto leftClip = clip;
    mClipDeletedSubscription
        =leftClip->Observer::Publisher<WaveClipDtorCalled>::Subscribe(
              [this](WaveClipDtorCalled) { Show(false); });
    mClipCentShiftChangeSubscription
        =leftClip->Observer::Publisher<CentShiftChange>::Subscribe(
              [this](const CentShiftChange& cents) {
        mShift = ToSemitonesAndCents(
            mShift.semis * 100 + mShift.cents, cents.newValue);
        UpdateDialog();
    });
    mClipSpeedChangeSubscription
        =leftClip->Observer::Publisher<StretchRatioChange>::Subscribe(
              [this](const StretchRatioChange& stretchRatio) {
        mClipSpeed = 100.0 / stretchRatio.newValue;
        UpdateDialog();
    });

    mTrack = track;
    mLeftClip = leftClip;
    mClipSpeed = 100.0 / leftClip->GetStretchRatio();
    mOldClipSpeed = mClipSpeed;
    mShift = GetClipShift(*leftClip);
    mOldShift = mShift;
    mFormantPreservation = leftClip->GetPitchAndSpeedPreset()
                           == PitchAndSpeedPreset::OptimizeForVoice;
    mOldFormantPreservation = mFormantPreservation;

    ShuttleGui s(this, mFirst ? eIsCreating : eIsSettingToDialog);

    {
        ScopedVerticalLay v { s };
        PopulateOrExchange(s);
    }

    if (mFirst) {
        Layout();
        Fit();
        Centre();
        mFirst = false;
    }

    return *this;
}

PitchAndSpeedDialog& PitchAndSpeedDialog::SetFocus(
    const std::optional<PitchAndSpeedDialogGroup>& group)
{
    const auto item
        =group.has_value()
          ? wxWindow::FindWindowById(
              *group == PitchAndSpeedDialogGroup::Pitch ? semitoneCtrlId
              : speedCtrlId,
              this)
          : nullptr;
    if (item) {
        item->SetFocus();
    }
    wxDialog::Show(true);
    wxDialog::Raise();
    wxDialog::SetFocus();
    return *this;
}

void PitchAndSpeedDialog::PopulateOrExchange(ShuttleGui& s)
{
    {
        ScopedInvisiblePanel panel { s, 15 };
        s.SetBorder(0);
        {
            ScopedStatic scopedStatic { s, XO("Clip Pitch") };
            {
                ScopedHorizontalLay h { s, wxLeft };
                s.SetBorder(2);
                // Use `TieSpinCtrl` rather than `AddSpinCtrl`, too see updates
                // instantly when `UpdateDialog` is called.
                const auto semiSpin = s.Id(semitoneCtrlId)
                                      .TieSpinCtrl(
                    XO("se&mitones:"), mShift.semis,
                    TimeAndPitchInterface::MaxCents / 100,
                    TimeAndPitchInterface::MinCents / 100);
                semiSpin->Bind(wxEVT_SPINCTRL, [this, semiSpin](const auto&) {
                    // The widget's value isn't updated yet on macos, so we need
                    // to asynchronously query it later.
                    CallAfter([this, semiSpin] {
                        const auto prevSemis = mShift.semis;
                        mShift.semis = semiSpin->GetValue();
                        // If we have e.g. -3 semi, -1 cents, and the user
                        // changes the sign of the semitones, the logic in
                        // `SetSemitoneShift` would result in 2 semi, 99
                        // cents. If the user changes sign again, we would now
                        // get 1 semi, -1 cents. Mirrorring (e.g. -3 semi, -1
                        // cents -> 3 semi, 1 cents) is not a good idea because
                        // that would ruin the work of users painstakingly
                        // adjusting the cents of an instrument. So instead, we
                        // map -3 semi, -1 cents to 3 semi, 99 cents.
                        if (mShift.cents != 0) {
                            if (prevSemis < 0 && mShift.semis > 0) {
                                ++mShift.semis;
                            } else if (prevSemis > 0 && mShift.semis < 0) {
                                --mShift.semis;
                            }
                        }
                        SetSemitoneShift();
                    });
                });
                const auto centSpin
                    =s.TieSpinCtrl(XO("&cents:"), mShift.cents, 100, -100);
                centSpin->Bind(wxEVT_SPINCTRL, [this, centSpin](const auto&) {
                    CallAfter([this, centSpin] {
                        mShift.cents = centSpin->GetValue();
                        SetSemitoneShift();
                    });
                });
            }
        }

        s.AddSpace(0, 12);
        s.SetBorder(0);

        {
            ScopedStatic scopedStatic { s, XO("Clip Speed") };
            {
                ScopedHorizontalLay h { s, wxLeft };
                const auto txtCtrl
                    =s.Id(speedCtrlId)
                      .Name(XO("Clip Speed"))
                      .TieSpinControl(
                          wxSize(60, -1), XO("&speed %: "), mClipSpeed, 1000.0, 1.0);
#if wxUSE_ACCESSIBILITY
                txtCtrl->SetAccessible(safenew WindowAccessible(txtCtrl));
#endif
                const auto playbackOngoing
                    =ProjectAudioIO::Get(mProject).IsAudioActive();
                txtCtrl->Enable(!playbackOngoing);
                txtCtrl->Bind(
                    wxEVT_SPINCTRL, [this, txtCtrl](wxCommandEvent& event) {
                    mClipSpeed = txtCtrl->GetValue();
                    if (!SetClipSpeed()) {
                        if (auto target = LockTarget()) {
                            WaveTrackUtilities::ExpandClipTillNextOne(
                                *target->track, *target->clip);
                            UpdateDialog();
                        }
                    }
                });
            }
        }

        s.AddSpace(0, 12);
        s.SetBorder(0);

        {
            ScopedStatic scopedStatic { s, XO("General") };
            {
                ScopedHorizontalLay h { s, wxLeft };
                s.SetBorder(2);
                s.TieCheckBox(XO("&Optimize for Voice"), mFormantPreservation)
                ->Bind(wxEVT_CHECKBOX, [this](auto&) {
                    mFormantPreservation = !mFormantPreservation;
                    if (auto target = LockTarget()) {
                        target->clip->SetPitchAndSpeedPreset(
                            mFormantPreservation
                            ? PitchAndSpeedPreset::OptimizeForVoice
                            : PitchAndSpeedPreset::Default);
                    }
                });
            }
        }
    }
}

bool PitchAndSpeedDialog::SetClipSpeed()
{
    auto target = LockTarget();
    if (!target) {
        return false;
    }

    const auto wasExactlySelected
        =IsExactlySelected(mProject, *mLeftClip.lock());

    if (!TimeStretching::SetClipStretchRatio(
            *target->track, *target->clip, 100 / mClipSpeed)) {
        return false;
    }

    if (wasExactlySelected) {
        WaveClipUIUtilities::SelectClip(mProject, *target->clip);
    }

    UpdateHistory(XO("Changed Speed"));

    return true;
}

void PitchAndSpeedDialog::UpdateDialog()
{
    ShuttleGui S(this, eIsSettingToDialog);
    PopulateOrExchange(S);
}

void PitchAndSpeedDialog::UpdateHistory(const TranslatableString& desc)
{
    ProjectHistory::Get(mProject).PushState(
        desc, desc, mConsolidateHistory ? UndoPush::CONSOLIDATE : UndoPush::NONE);
    mConsolidateHistory = true;
}

std::optional<PitchAndSpeedDialog::StrongTarget>
PitchAndSpeedDialog::LockTarget()
{
    if (const auto track = mTrack.lock()) {
        if (const auto leftClip = mLeftClip.lock()) {
            return StrongTarget {
                track, leftClip
            }
        }
    }
    return {};
}

void PitchAndSpeedDialog::SetSemitoneShift()
{
    auto target = LockTarget();
    if (!target) {
        return;
    }
    ClampPitchShift(mShift);
    const auto success
        =target->clip->SetCentShift(mShift.semis * 100 + mShift.cents);
    assert(success);
    TrackPanel::Get(mProject).RefreshTrack(target->track.get());
    UpdateHistory(XO("Changed Pitch"));
}
