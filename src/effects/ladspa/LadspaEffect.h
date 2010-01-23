/**********************************************************************

  Audacity: A Digital Audio Editor

  LadspaEffect.h

  Dominic Mazzoni

**********************************************************************/

class wxSlider;
class wxStaticText;
class wxTextCtrl;
class wxCheckBox;

#include <wx/dialog.h>

#include "../Effect.h"
#include "ladspa.h"

void LoadLadspaPlugins();

class LadspaEffect:public Effect {

 public:

   LadspaEffect(const LADSPA_Descriptor *data, 
                const std::set<wxString>& categories = std::set<wxString>());
   virtual ~LadspaEffect();

   virtual wxString GetEffectName();
   
   virtual std::set<wxString> GetEffectCategories();

   virtual wxString GetEffectIdentifier();
   
   virtual wxString GetEffectAction();

   virtual bool Init();

   virtual bool PromptUser();
   
   virtual bool Process();
   
   virtual void End();

 private:
   bool ProcessStereo(int count, WaveTrack * left, WaveTrack *right,
                      sampleCount lstart, sampleCount rstart,
                      sampleCount len);

   wxString pluginName;

   const LADSPA_Descriptor *mData;
   sampleCount mBlockSize;
   float **fInBuffer;
   float **fOutBuffer;
   unsigned long inputs;
   unsigned long outputs;
   unsigned long numInputControls;
   unsigned long *inputPorts;
   unsigned long *outputPorts;
   float *inputControls;
   float *outputControls;
   int mainRate;

   std::set<wxString> mCategories;
};

class LadspaEffectDialog:public wxDialog {
   DECLARE_DYNAMIC_CLASS(LadspaEffectDialog)

 public:
   LadspaEffectDialog(LadspaEffect *effect,
                      wxWindow * parent,
                      const LADSPA_Descriptor *data,
                      float *inputControls,
                      int sampleRate,
                      double length);

   ~LadspaEffectDialog();

   void OnCheckBox(wxCommandEvent & event);
   void OnSlider(wxCommandEvent & event);
   void OnTextCtrl(wxCommandEvent & event);
   void OnOK(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);
   void OnPreview(wxCommandEvent & event);
   void ControlSetFocus(wxFocusEvent & event);

   double GetLength();

   DECLARE_EVENT_TABLE()

 private:
   void HandleText();
   void ConnectFocus(wxControl *c);
   void DisconnectFocus(wxControl *c);
   bool inSlider;
   bool inText;

   double mLength;
   int sampleRate;
   const LADSPA_Descriptor *mData;
   wxSlider **sliders;
   wxTextCtrl **fields;
   wxStaticText **labels;
   wxCheckBox **toggles;
   unsigned long *ports;
   unsigned long numParams;
   float *inputControls;
   LadspaEffect *effect;
   wxTextCtrl *mSeconds;
};

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: d8622b59-5c08-4e7f-a170-2502ff8af8e5

