/**********************************************************************

  Audacity: A Digital Audio Editor

  SpikeCleaner.h

  Lynn Allan

**********************************************************************/

#ifndef __AUDACITY_EFFECT_SPIKE_CLEANER__
#define __AUDACITY_EFFECT_SPIKE_CLEANER__

#include "SimpleMono.h"

#include <wx/dialog.h>

class wxTextCtrl;
class wxChoice;

class EffectSpikeCleaner: public EffectSimpleMono {

public:
   EffectSpikeCleaner();

   virtual wxString GetEffectName() {
      return wxString(_("Spike Cleaner..."));
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("SpikeCleaner"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Applying Spike Cleaner..."));
   }
   virtual bool Init();
   virtual bool CheckWhetherSkipEffect();
   virtual void End();
   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );

protected:
   virtual bool ProcessSimpleMono(float *buffer, sampleCount len);

private:
   friend class SpikeCleanerDialog;

   int mSpikeMaxDurationMs;
   int mSpikeDbChoiceIndex;
};

//----------------------------------------------------------------------------
// SpikeCleanerDialog
//----------------------------------------------------------------------------

class SpikeCleanerDialog: public wxDialog
{
public:
   // constructors and destructors
   SpikeCleanerDialog(wxWindow *parent, wxWindowID id, const wxString &title);

   int        mSpikeMaxDurationMs;
   int        mSpikeDbChoiceIndex;

   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

private:
   wxTextCtrl *mSpikeMaxDurationMsText;
   wxChoice   *mSpikeDbSilenceThresholdChoice;

   void OnOk( wxCommandEvent &event );
   void OnCancel( wxCommandEvent &event );
private:
   DECLARE_EVENT_TABLE()
};

#endif

