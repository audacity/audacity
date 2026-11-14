/**********************************************************************

Audacity: A Digital Audio Editor

ButtonHandle.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_BUTTON_HANDLE__
#define __AUDACITY_BUTTON_HANDLE__

#include "../../UIHandle.h"

class wxMouseEvent;
class wxMouseState;

class Track;
class TranslatableString;

/// \brief A UIHandle for a TrackPanel button, such as the Mute and Solo
/// buttons.
class AUDACITY_DLL_API ButtonHandle /* not final */ : public UIHandle
{
    ButtonHandle(const ButtonHandle&) = delete;

public:
    std::shared_ptr<Track> GetTrack() const { return mpTrack.lock(); }
    bool IsDragging() const override;

protected:
    explicit ButtonHandle(const std::shared_ptr<Track>& pTrack, const wxRect& rect);

    ButtonHandle& operator=(const ButtonHandle&) = default;

    virtual ~ButtonHandle();

    std::shared_ptr<const Track> FindTrack() const override;

    // This NEW abstract virtual simplifies the duties of further subclasses.
    // This class will decide whether to refresh the clicked cell for button state
    // change.
    // Subclass can decide to refresh other things and the results will be ORed.
    virtual Result CommitChanges(const wxMouseEvent& event, AudacityProject* pProject, wxWindow* pParent) = 0;

    // Define a message for the status bar and tooltip.
    virtual TranslatableString Tip(
        const wxMouseState& state, AudacityProject& project) const = 0;

    void Enter(bool forward, AudacityProject*) final override;

    Result Click(const TrackPanelMouseEvent& event, AudacityProject* pProject)
    final override;

    Result Drag(const TrackPanelMouseEvent& event, AudacityProject* pProject)
    final override;

    HitTestPreview Preview(const TrackPanelMouseState& state, AudacityProject* pProject)
    final override;

    Result Release(const TrackPanelMouseEvent& event, AudacityProject* pProject, wxWindow* pParent) final override;

    Result Cancel(AudacityProject* pProject) final override;

    std::weak_ptr<Track> mpTrack;
    wxRect mRect;
    bool mWasIn{ true };
    bool mIsDragging{};
};

#endif
