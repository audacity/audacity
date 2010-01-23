/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangeLength.h

  Lynn Allan (from DM's ChangeTempo)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_CHANGE_LENGTH__
#define __AUDACITY_EFFECT_CHANGE_LENGTH__

#include <wx/dialog.h>
#include "Effect.h"

class wxTextCtrl;

class EffectChangeLength: public Effect {

public:

   EffectChangeLength();

   virtual wxString GetEffectName() {
      return wxString(_("Change Length..."));
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("ChangeLength"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Changing Length..."));
   }
   virtual bool Init();
   virtual void End();
   virtual bool CheckWhetherSkipEffect();
   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );
   virtual bool Process();

 private:
   WaveTrack   *mTrack;
   double      mFromLength;
   double      mToLength;

friend class ChangeLengthDialog;
};

//----------------------------------------------------------------------------
// ChangeLengthDialog
//----------------------------------------------------------------------------

class ChangeLengthDialog: public wxDialog
{
public:
   // constructors and destructors
   ChangeLengthDialog(wxWindow *parent, wxWindowID id, const wxString &title);

   double      mFromLength;
   double      mToLength;

   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

private:
   void OnOk( wxCommandEvent &event );
   void OnCancel( wxCommandEvent &event );
   void OnRecalculate(wxCommandEvent & event); 
   void OnText_ToLength(wxCommandEvent & event); 

   wxTextCtrl * m_pTextCtrl_ToRange;
   wxTextCtrl * m_pTextCtrl_PercentChange;
   wxTextCtrl * m_pTextCtrl_FromLength;
   wxTextCtrl * m_pTextCtrl_ToLength;

   double      mPercentChange;

private:
   DECLARE_EVENT_TABLE()
};

#endif
