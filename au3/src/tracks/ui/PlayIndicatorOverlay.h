/**********************************************************************

Audacity: A Digital Audio Editor

PlayIndicatorOverlay.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_PLAY_INDICATOR_OVERLAY__
#define __AUDACITY_PLAY_INDICATOR_OVERLAY__

#include <memory>
#include "ClientData.h"
#include "../../widgets/Overlay.h" // to inherit
#include "Observer.h"

class AudacityProject;

// Common class for overlaying track panel or ruler
class PlayIndicatorOverlayBase : public Overlay, public ClientData::Base
{
public:
    PlayIndicatorOverlayBase(AudacityProject* project, bool isMaster);
    virtual ~PlayIndicatorOverlayBase();

    void Update(int newIndicatorX) { mNewIndicatorX = newIndicatorX; }

private:
    unsigned SequenceNumber() const override;
    std::pair<wxRect, bool> DoGetRectangle(wxSize size) override;
    void Draw(OverlayPanel& panel, wxDC& dc) override;

protected:

    AudacityProject* const mProject;
    const bool mIsMaster;
    int mLastIndicatorX { -1 };
    int mNewIndicatorX { -1 };
    bool mNewIsCapturing { false };
    bool mLastIsCapturing { false };
};

// Master object for track panel, creates the other object for the ruler
class PlayIndicatorOverlay final : public PlayIndicatorOverlayBase
{
public:
    explicit
    PlayIndicatorOverlay(AudacityProject* project);

private:
    void OnTimer(Observer::Message);

    std::shared_ptr<PlayIndicatorOverlayBase> mPartner;
    Observer::Subscription mSubscription;
};

#endif
