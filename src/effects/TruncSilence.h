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

#include <wx/arrstr.h>
#include <wx/choice.h>
#include <wx/event.h>
#include <wx/list.h>
#include <wx/string.h>
#include <wx/textctrl.h>

#include "../ShuttleGui.h"

#include "Effect.h"

#define TRUNCATESILENCE_PLUGIN_SYMBOL XO("Truncate Silence")

// Declaration of RegionList
struct REGION;
typedef struct REGION Region;
WX_DECLARE_LIST(Region, RegionList);

class EffectTruncSilence : public Effect
{
public:
   EffectTruncSilence();
   virtual ~EffectTruncSilence();

   // IdentInterface implementation

   virtual wxString GetSymbol();
   virtual wxString GetDescription();

   // EffectIdentInterface implementation

   virtual EffectType GetType();

   // EffectClientInterface implementation

   virtual bool GetAutomationParameters(EffectAutomationParameters & parms);
   virtual bool SetAutomationParameters(EffectAutomationParameters & parms);

   // Effect implementation

   virtual bool Startup();
   virtual bool Process();
   virtual void PopulateOrExchange(ShuttleGui & S);
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

private:
   // EffectTruncSilence implementation

   //ToDo ... put BlendFrames in Effects, Project, or other class
   void BlendFrames(float* buffer, int leftIndex, int rightIndex, int blendFrameCount);
   void Intersect(RegionList &dest, const RegionList &src);

   void OnControlChange(wxCommandEvent & evt);
   void UpdateUI();

private:
   int mTruncDbChoiceIndex;
   int mActionIndex;
   double mInitialAllowedSilence;
   double mTruncLongestAllowedSilence;
   double mSilenceCompressPercent;

   wxArrayString mDbChoices;

   sampleCount mBlendFrameCount;

   wxChoice *mTruncDbChoice;
   wxChoice *mActionChoice;
   wxTextCtrl *mInitialAllowedSilenceT;
   wxTextCtrl *mTruncLongestAllowedSilenceT;
   wxTextCtrl *mSilenceCompressPercentT;

   DECLARE_EVENT_TABLE();
};

#endif
