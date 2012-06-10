/**********************************************************************

   Audacity: A Digital Audio Editor
   Paulstretch.h

   Nasca Octavian Paul

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
         return wxString(_("Paulstretch..."));
      }

      virtual wxString GetEffectAction() {
         return wxString(_("Stretching with Paulstretch"));
      }

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

