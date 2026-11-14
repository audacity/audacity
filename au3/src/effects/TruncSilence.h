/**********************************************************************

  Audacity: A Digital Audio Editor

  TruncSilence.h

  Lynn Allan (from DM's Normalize)
  //ToDo ... put BlendFrames in Effects, Project, or other class
  //ToDo ... Use ZeroCrossing logic to improve blend
  //ToDo ... BlendFrames on "fade-out"
  //ToDo ... BlendFrameCount is a user-selectable parameter
  //ToDo ... Detect transient signals that are too short to interrupt the TruncatableSilence
  Philip Van Baren (more options and boundary fixes)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_TRUNC_SILENCE__
#define __AUDACITY_EFFECT_TRUNC_SILENCE__

#include "TruncSilenceBase.h"
#include "StatefulEffectUIServices.h"
#include <wx/weakref.h>

class ShuttleGui;
class wxChoice;
class wxTextCtrl;
class wxCheckBox;

class EffectTruncSilence final : public TruncSilenceBase, public StatefulEffectUIServices
{
public:
    std::unique_ptr<EffectEditor> PopulateOrExchange(
        ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectOutputs* pOutputs) override;
    bool TransferDataToWindow(const EffectSettings& settings) override;
    bool TransferDataFromWindow(EffectSettings& settings) override;

    DECLARE_EVENT_TABLE()

    void OnControlChange(wxCommandEvent& evt);
    void UpdateUI();

private:
    wxWeakRef<wxWindow> mUIParent {};

    wxTextCtrl* mThresholdText;
    wxChoice* mActionChoice;
    wxTextCtrl* mInitialAllowedSilenceT;
    wxTextCtrl* mTruncLongestAllowedSilenceT;
    wxTextCtrl* mSilenceCompressPercentT;
    wxCheckBox* mIndependent;
};

#endif
