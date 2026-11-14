/*!********************************************************************

   Audacity: A Digital Audio Editor

   @file RealtimeEffectPanel.cpp

   @author Vitaly Sverchinsky

**********************************************************************/

#pragma once

#include <memory>
#include <wx/scrolwin.h>
#include <wx/weakref.h>
#include <wx/splitter.h>

#include "ThemedWrappers.h"
#include "Observer.h"

class SampleTrack;

class wxButton;
class wxStaticText;
class wxBitmapButton;

class RealtimeEffectListWindow;
class AudacityProject;

/**
 * \brief UI Panel that displays realtime effects from the effect stack of
 * an individual track, provides controls for accessing effect settings,
 * stack manipulation (reorder, add, remove)
 */
class RealtimeEffectPanel : public wxSplitterWindow
{
    AButton* mToggleTrackEffects{ nullptr };
    AButton* mToggleMasterEffects{ nullptr };
    wxStaticText* mTrackTitle { nullptr };
    wxWindow* mTrackEffectsPanel{ nullptr };
    wxWindow* mProjectEffectsPanel{ nullptr };
    RealtimeEffectListWindow* mTrackEffectList{ nullptr };
    RealtimeEffectListWindow* mMasterEffectList{ nullptr };
    wxWindow* mTrackEffectsHeader{ nullptr };
    AudacityProject& mProject;

    std::weak_ptr<SampleTrack> mCurrentTrack;

    Observer::Subscription mTrackListChanged;
    Observer::Subscription mUndoSubscription;
    Observer::Subscription mFocusChangeSubscription;

    std::vector<std::shared_ptr<SampleTrack> > mPotentiallyRemovedTracks;

    // RealtimeEffectPanel is wrapped using ThemedWindowWrapper,
    // so we cannot subscribe to Prefs directly
    struct PrefsListenerHelper;
    std::unique_ptr<PrefsListenerHelper> mPrefsListenerHelper;

public:
    static RealtimeEffectPanel& Get(AudacityProject& project);
    static const RealtimeEffectPanel& Get(const AudacityProject& project);

    RealtimeEffectPanel(
        AudacityProject& project, wxWindow* parent, wxWindowID id, const wxPoint& pos = wxDefaultPosition,
        const wxSize& size = wxDefaultSize, long style = 0, const wxString& name = wxPanelNameStr);

    ~RealtimeEffectPanel() override;

    void ShowPanel(SampleTrack* track, bool focus);
    void HidePanel();

    /**
     * \brief Shows effects from the effect stack of the track
     * \param track Pointer to the existing track, or null
     */
    void SetTrack(const std::shared_ptr<SampleTrack>& track);
    void ResetTrack();

    bool IsTopNavigationDomain(NavigationKind) const override { return true; }

    void SetFocus() override;

private:

    void MakeTrackEffectPane();
    void MakeMasterEffectPane();

    void OnCharHook(wxKeyEvent& evt);
};
