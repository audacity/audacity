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

#include "../Effect.h"

class LabelTrack;

#define VAMPEFFECTS_VERSION wxT("1.0.0.0")
#define VAMPEFFECTS_FAMILY wxT("Vamp")

class VampEffect final : public Effect
{
public:
   VampEffect(std::unique_ptr<Vamp::Plugin> &&plugin,
              const wxString & path,
              int output,
              bool hasParameters);
   virtual ~VampEffect();

   // IdentInterface implementation

   wxString GetPath() override;
   wxString GetSymbol() override;
   wxString GetName() override;
   wxString GetVendor() override;
   wxString GetVersion() override;
   wxString GetDescription() override;

   // EffectIdentInterface implementation

   EffectType GetType() override;
   wxString GetFamily() override;
   bool IsInteractive() override;
   bool IsDefault() override;

   // EffectClientInterface implementation

   unsigned GetAudioInCount() override;
   bool GetAutomationParameters(EffectAutomationParameters & parms) override;
   bool SetAutomationParameters(EffectAutomationParameters & parms) override;

   // Effect implementation

   bool Init() override;
   bool Process() override;
   void End() override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

private:
   // VampEffect implemetation

   void AddFeatures(LabelTrack *track, Vamp::Plugin::FeatureSet & features);

   void UpdateFromPlugin();

   void OnCheckBox(wxCommandEvent & evt);
   void OnChoice(wxCommandEvent & evt);
   void OnSlider(wxCommandEvent & evt);
   void OnTextCtrl(wxCommandEvent & evt);

private:
   std::unique_ptr<Vamp::Plugin> mPlugin;
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

   DECLARE_EVENT_TABLE()
};

#endif
