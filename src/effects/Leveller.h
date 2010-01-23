/**********************************************************************

  Audacity: A Digital Audio Editor

  Leveller.h

  Lynn Allan

**********************************************************************/

#ifndef __AUDACITY_EFFECT_LEVELER__
#define __AUDACITY_EFFECT_LEVELER__

#include "SimpleMono.h"

#include <wx/choice.h>
#include <wx/string.h>
#include <wx/textctrl.h>

class EffectLeveller: public EffectSimpleMono
{
 friend class LevellerDialog;

 public:
   EffectLeveller();

   virtual wxString GetEffectName() {
      return wxString(_("Leveller..."));
   }

   virtual std::set<wxString> GetEffectCategories() {
     std::set<wxString> result;
     result.insert(wxT("http://lv2plug.in/ns/lv2core#CompressorPlugin"));
     return result;
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("Leveller"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Applying Leveller..."));
   }
   virtual bool Init();
   virtual void End();
   virtual bool CheckWhetherSkipEffect();
   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );

 protected:
   virtual bool ProcessSimpleMono(float *buffer, sampleCount len);

 private:
   void   CalcLevellerFactors();
   int    mLevellerDbChoiceIndex;
   int    mLevellerNumPasses;
   double mFrameSum;
   double mLevellerDbSilenceThreshold;
   float  LevelOneFrame(float frame);
};

//----------------------------------------------------------------------------
// LevellerDialog
//----------------------------------------------------------------------------

class LevellerDialog: public EffectDialog
{
 public:
   // constructors and destructors
   LevellerDialog(EffectLeveller *effect, wxWindow * parent);

   // method declarations
   void PopulateOrExchange(ShuttleGui & S);
//   bool TransferDataToWindow();
//   bool TransferDataFromWindow();

 private:
	// handlers
   void OnPreview( wxCommandEvent &event );

 private:
   EffectLeveller *mEffect;
   wxChoice *mLevellerDbSilenceThresholdChoice;
   wxChoice *mLevellerNumPassesChoice;

   DECLARE_EVENT_TABLE()

 public:
   int mLevellerDbChoiceIndex;
   int mLevellerNumPasses;
};

#endif

