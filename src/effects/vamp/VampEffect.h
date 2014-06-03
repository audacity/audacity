/**********************************************************************

  Audacity: A Digital Audio Editor

  VampEffect.h

  Chris Cannam

  Vamp is an audio analysis and feature extraction plugin API.
  http://www.vamp-plugins.org/

**********************************************************************/

#include "../Effect.h"
#include "../../LabelTrack.h"

class wxSlider;
class wxStaticText;
class wxTextCtrl;
class wxCheckBox;
class wxComboBox;

#include <wx/dialog.h>

#include <vamp-hostsdk/PluginLoader.h>

void LoadVampPlugins();

class VampEffect : public Effect {

 public:

   VampEffect(Vamp::HostExt::PluginLoader::PluginKey key,
              int output,
              bool hasParameters,
              wxString name,
              wxString category = wxString());
   virtual ~VampEffect();

   virtual wxString GetEffectName();

   virtual std::set<wxString> GetEffectCategories();

   virtual wxString GetEffectIdentifier();

   virtual wxString GetEffectAction();

   virtual bool Init();

   virtual bool PromptUser();

   virtual bool Process();

   virtual void End();

 private:

   VampEffect(const VampEffect &);
   VampEffect &operator=(const VampEffect &);

   Vamp::HostExt::PluginLoader::PluginKey mKey;
   int mOutput;
   bool mHasParameters;
   wxString mName;
   double mRate;
   wxString mCategory;

   Vamp::Plugin *mPlugin;

   void AddFeatures(LabelTrack *track,
                    Vamp::Plugin::FeatureSet &features);
};


class VampEffectDialog : public wxDialog {

   DECLARE_DYNAMIC_CLASS(VampEffectDialog)

 public:
   VampEffectDialog(VampEffect *effect,
                    wxWindow *parent,
                    Vamp::Plugin *plugin);
   ~VampEffectDialog();

   void OnCheckBox(wxCommandEvent & event);
   void OnComboBox(wxCommandEvent & event);
   void OnSlider(wxCommandEvent & event);
   void OnTextCtrl(wxCommandEvent & event);
   void OnOK(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);
   void OnPreview(wxCommandEvent & event);
   void ControlSetFocus(wxFocusEvent & event);

   DECLARE_EVENT_TABLE()

 private:
   void HandleText();
   void UpdateFromPlugin();
   void ConnectFocus(wxControl *c);
   void DisconnectFocus(wxControl *c);
   bool inSlider;
   bool inText;

   VampEffect *mEffect;
   Vamp::Plugin *mPlugin;
   Vamp::Plugin::ParameterList mParameters;

   wxSlider **sliders;
   wxTextCtrl **fields;
   wxStaticText **labels;
   wxCheckBox **toggles;
   wxComboBox **combos;
   wxComboBox *programCombo;
};
