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

#include "Effect.h"
#include "../Experimental.h"

#include <wx/list.h>

// Declaration of RegionList
struct REGION;
typedef struct REGION Region;
WX_DECLARE_LIST(Region, RegionList);

class EffectTruncSilence: public Effect {

public:

   EffectTruncSilence();

   virtual wxString GetEffectName() {
      return wxString(_("Truncate Silence..."));
   }

   virtual std::set<wxString> GetEffectCategories() {
     std::set<wxString> result;
     result.insert(wxT("http://audacityteam.org/namespace#TimelineChanger"));
     return result;
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("TruncateSilence"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Truncating Silence..."));
   }
   virtual bool Init();
   virtual void End();
   virtual bool CheckWhetherSkipEffect();
   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );

   virtual bool Process();

 private:
   //ToDo ... put BlendFrames in Effects, Project, or other class
   void BlendFrames(float* buffer, int leftIndex, int rightIndex, int blendFrameCount);
   void Intersect(RegionList &dest, const RegionList &src);

 private:
   sampleCount mBlendFrameCount;
   int mTruncInitialAllowedSilentMs;
   int mTruncLongestAllowedSilentMs;
   int mTruncDbChoiceIndex;
   double mSilenceCompressRatio;

friend class TruncSilenceDialog;
};

//----------------------------------------------------------------------------
// TruncSilenceDialog
//----------------------------------------------------------------------------

class TruncSilenceDialog: public EffectDialog
{
public:
   // constructors and destructors
   TruncSilenceDialog(EffectTruncSilence * effect,
                      wxWindow * parent);

   void PopulateOrExchange(ShuttleGui & S);
   void OnPreview(wxCommandEvent & event);
   void OnDurationChange(wxCommandEvent & event);

private:
   EffectTruncSilence *mEffect;
   wxStaticText * pWarning;

   wxStaticText * mTruncationMessage;

   wxStaticText *mRatioLabel;

private:
   DECLARE_EVENT_TABLE()
};

#endif
