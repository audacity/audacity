/**********************************************************************

   Audacity: A Digital Audio Editor
   Paulstretch.h

   Nasca Octavian Paul (Paul Nasca)

 **********************************************************************/

#ifndef __AUDACITY_EFFECT_PAULSTRETCH__
#define __AUDACITY_EFFECT_PAULSTRETCH__

#include "SimpleMono.h"
#include <wx/dialog.h>
#include <wx/intl.h>


class WaveTrack;

class EffectPaulstretch:public Effect{

   public:
      EffectPaulstretch();

      virtual wxString GetEffectName() {
         /* i18n-hint: This is the name of the effect, i.e. a proper noun, which
          * wouldn't normally get translated. It's the combination of the author's
          * name (Paul) with what it does (stretch sound)
          */
         return wxString(_("Paulstretch..."));
      }

      virtual wxString GetEffectAction() {
         /* i18n-hint: This is the text that is shown whilst the effect is being
          * processed. The effect stretches the input in time, making the sound
          * much longer and spread out whilst at the same pitch. Paulstretch is the
          * name of the effect (it's also translated on it's own).
          */
         return wxString(_("Stretching with Paulstretch"));
      }

      virtual wxString GetEffectIdentifier() {return wxT("Paulstretch");}

      // Useful only after PromptUser values have been set. 
      virtual wxString GetEffectDescription(); 

      virtual bool PromptUser();
      virtual bool TransferParameters( Shuttle & shuttle );
      virtual bool Process();

   protected:

      bool ProcessOne(WaveTrack *track,double t0,double t1,int count);
      float amount;
      float time_resolution;//seconds

      friend class PaulstretchDialog;
   private:
      double m_t1;
};


//--------------------------------------------------------------------------
// PaulstretchDialog
//--------------------------------------------------------------------------

class PaulstretchDialog:public EffectDialog {
   public:
      PaulstretchDialog(EffectPaulstretch * effect, wxWindow * parent);

      void PopulateOrExchange(ShuttleGui & S);
      bool TransferDataToWindow();
      bool TransferDataFromWindow();

   private:
      // handlers
      void OnPreview( wxCommandEvent &event );

   private:
      bool				m_bLoopDetect;
      EffectPaulstretch *	m_pEffect;

      // controls
      wxTextCtrl *	m_pTextCtrl_Amount;
      wxTextCtrl *	m_pTextCtrl_TimeResolution;

   public:
      // effect parameters
      float amount;
      float time_resolution;//seconds
   private:
      DECLARE_EVENT_TABLE()
};


#endif

