/**********************************************************************

  Audacity: A Digital Audio Editor

  FindClipping.h

  Dominic Mazzoni
  Vaughan Johnson (dialog)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_FINDCLIPPING__
#define __AUDACITY_EFFECT_FINDCLIPPING__

class wxString;

#include <wx/dialog.h>

#include <wx/intl.h>

#include "Effect.h"

class wxStaticText;

class WaveTrack;

class EffectFindClipping:public Effect
{
 friend class FindClippingDialog;

 public:

   EffectFindClipping();

   virtual wxString GetEffectName()
   {
      return wxString(_("Find Clipping..."));
   }

   virtual std::set<wxString> GetEffectCategories()
   {
      std::set<wxString> result;
      result.insert(wxT("http://lv2plug.in/ns/lv2core#AnalyserPlugin"));
      return result;
   }

   virtual wxString GetEffectIdentifier()
   {
      return wxString(wxT("FindClipping"));
   }

   virtual wxString GetEffectAction()
   {
      return wxString(_("Detecting clipping"));
   }

   virtual wxString GetEffectDescription();

   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );

   virtual bool Process();

 private:
   bool ProcessOne(LabelTrack *l, int count, WaveTrack * t,
                   sampleCount start, sampleCount len);

   int mStart;   ///< Using int rather than sampleCount because values are only ever small numbers
   int mStop;    ///< Using int rather than sampleCount because values are only ever small numbers
};

//----------------------------------------------------------------------------
// FindClippingDialog
//----------------------------------------------------------------------------
class FindClippingDialog:public EffectDialog {
 public:
   FindClippingDialog(EffectFindClipping * effect, wxWindow * parent);

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataFromWindow();

 private:
   EffectFindClipping *mEffect;
};

#endif // __AUDACITY_EFFECT_FINDCLIPPING__
