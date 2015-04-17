/**********************************************************************

  Audacity: A Digital Audio Editor

  VampEffect.h

  Chris Cannam

  Vamp is an audio analysis and feature extraction plugin API.
  http://www.vamp-plugins.org/

**********************************************************************/

#include "../../Audacity.h"

#if defined(USE_VAMP)

#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/event.h>
#include <wx/slider.h>
#include <wx/string.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>

#include <vamp-hostsdk/PluginLoader.h>

#include "../../LabelTrack.h"

#include "../Effect.h"

#define VAMPEFFECTS_VERSION wxT("1.0.0.0")
#define VAMPEFFECTS_FAMILY wxT("Vamp")

class VampEffect : public Effect
{
public:
   VampEffect(Vamp::Plugin *plugin,
              const wxString & path,
              int output,
              bool hasParameters);
   virtual ~VampEffect();

   // IdentInterface implementation

   virtual wxString GetPath();
   virtual wxString GetSymbol();
   virtual wxString GetName();
   virtual wxString GetVendor();
   virtual wxString GetVersion();
   virtual wxString GetDescription();

   // EffectIdentInterface implementation

   virtual EffectType GetType();
   virtual wxString GetFamily();
   virtual bool IsInteractive();
   virtual bool IsDefault();

   // EffectClientInterface implementation

   virtual int GetAudioInCount();
   virtual bool GetAutomationParameters(EffectAutomationParameters & parms);
   virtual bool SetAutomationParameters(EffectAutomationParameters & parms);

   // Effect implementation

   virtual bool Init();
   virtual bool Process();
   virtual void End();
   virtual void PopulateOrExchange(ShuttleGui & S);
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

private:
   // VampEffect implemetation

   void AddFeatures(LabelTrack *track, Vamp::Plugin::FeatureSet & features);

   void UpdateFromPlugin();

   void OnCheckBox(wxCommandEvent & evt);
   void OnChoice(wxCommandEvent & evt);
   void OnSlider(wxCommandEvent & evt);
   void OnTextCtrl(wxCommandEvent & evt);

private:
   Vamp::Plugin *mPlugin;
   wxString mPath;
   int mOutput;
   bool mHasParameters;

   Vamp::HostExt::PluginLoader::PluginKey mKey;
   wxString mName;
   double mRate;

   bool mInSlider;
   bool mInText;

   Vamp::Plugin::ParameterList mParameters;

   float *mValues;

   wxSlider **mSliders;
   wxTextCtrl **mFields;
   wxStaticText **mLabels;
   wxCheckBox **mToggles;
   wxChoice **mChoices;
   wxChoice *mProgram;

   DECLARE_EVENT_TABLE();
};

#endif
