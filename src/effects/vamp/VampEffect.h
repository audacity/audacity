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

#include "../../SampleFormat.h"
#include "../Effect.h"

class LabelTrack;

#define VAMPEFFECTS_VERSION wxT("1.0.0.0")
/* i18n-hint: Vamp is the proper name of a software protocol for sound analysis.
   It is not an abbreviation for anything.  See http://vamp-plugins.org */
#define VAMPEFFECTS_FAMILY XO("Vamp")

class VampEffect final : public Effect
{
public:
   VampEffect(std::unique_ptr<Vamp::Plugin> &&plugin,
              const wxString & path,
              int output,
              bool hasParameters);
   virtual ~VampEffect();

   // ComponentInterface implementation

   wxString GetPath() override;
   ComponentInterfaceSymbol GetSymbol() override;
   ComponentInterfaceSymbol GetVendor() override;
   wxString GetVersion() override;
   wxString GetDescription() override;

   // EffectDefinitionInterface implementation

   EffectType GetType() override;
   ComponentInterfaceSymbol GetFamilyId() override;
   bool IsInteractive() override;
   bool IsDefault() override;

   // EffectClientInterface implementation

   unsigned GetAudioInCount() override;
   bool GetAutomationParameters(CommandParameters & parms) override;
   bool SetAutomationParameters(CommandParameters & parms) override;

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

   Floats mValues;

   ArrayOf<wxSlider *> mSliders;
   ArrayOf<wxTextCtrl *> mFields;
   ArrayOf<wxStaticText *> mLabels;
   ArrayOf<wxCheckBox *> mToggles;
   ArrayOf<wxChoice *> mChoices;
   wxChoice *mProgram;

   DECLARE_EVENT_TABLE()
};

#endif
