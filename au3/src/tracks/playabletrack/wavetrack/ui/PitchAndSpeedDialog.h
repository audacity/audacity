/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file PitchAndSpeedDialog.h

 Dmitry Vedenko

 **********************************************************************/
#pragma once

#include "ClientData.h"
#include "Observer.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include "wxPanelWrapper.h"

struct TrackPanelMouseEvent;
class AudacityProject;
class ShuttleGui;
class WaveClip;

enum class PitchAndSpeedDialogGroup
{
    Pitch,
    Speed
};

class PitchAndSpeedDialog final : public wxDialogWrapper, public ClientData::Base
{
public:
    static PitchAndSpeedDialog& Get(AudacityProject& project);
    static const PitchAndSpeedDialog& Get(const AudacityProject& project);
    static void Destroy(AudacityProject& project);

    PitchAndSpeedDialog(AudacityProject& project);

    PitchAndSpeedDialog& Retarget(
        const std::shared_ptr<WaveTrack>& track, const WaveTrack::IntervalHolder& wideClip);
    PitchAndSpeedDialog&
    SetFocus(const std::optional<PitchAndSpeedDialogGroup>& group);
    void TryRetarget(const TrackPanelMouseEvent& event);

    struct PitchShift
    {
        int semis = 0;
        int cents = 0;
        bool operator==(const PitchShift& other) const
        {
            return semis == other.semis && cents == other.cents;
        }

        bool operator!=(const PitchShift& other) const
        {
            return !(*this == other);
        }
    };

private:
    bool Show(bool show) override
    {
        return wxDialogWrapper::Show(show);
    }

    void SetFocus() override
    {
        wxDialogWrapper::SetFocus();
    }

    void PopulateOrExchange(ShuttleGui& s);
    void SetSemitoneShift();
    bool SetClipSpeed();
    void UpdateDialog();
    void UpdateHistory(const TranslatableString& desc);

    struct StrongTarget
    {
        const std::shared_ptr<WaveTrack> track;
        WaveClipHolder clip;
    };

    std::optional<StrongTarget> LockTarget();

    AudacityProject& mProject;
    const wxString mTitle;
    Observer::Subscription mProjectCloseSubscription;
    Observer::Subscription mAudioIOSubscription;
    Observer::Subscription mClipDeletedSubscription;
    Observer::Subscription mClipCentShiftChangeSubscription;
    Observer::Subscription mClipSpeedChangeSubscription;
    double mClipSpeed = 100;
    double mOldClipSpeed = mClipSpeed;
    PitchShift mShift;
    PitchShift mOldShift;
    std::weak_ptr<WaveClip> mLeftClip;
    std::weak_ptr<WaveClip> mRightClip;
    std::weak_ptr<WaveTrack> mTrack;
    bool mConsolidateHistory = true;
    bool mFirst = true;
    bool mFormantPreservation = false;
    bool mOldFormantPreservation = mFormantPreservation;
};
