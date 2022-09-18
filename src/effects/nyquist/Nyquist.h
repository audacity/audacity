/**********************************************************************

  Audacity: A Digital Audio Editor

  Nyquist.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NYQUIST__
#define __AUDACITY_EFFECT_NYQUIST__

#include "NyquistUIControls.h"
#include "NyquistEnvironment.h"
#include "../Effect.h"
#include "SampleCount.h"
#include "../../widgets/wxPanelWrapper.h"

class wxArrayString;
class wxFileName;
class wxCheckBox;
class wxTextCtrl;

#define NYQUISTEFFECTS_VERSION wxT("1.0.0.0")

struct NyqValue;
using NyquistBindings = std::vector<NyqValue>;
struct NyquistParser;
struct NyquistUIControls;

class AUDACITY_DLL_API NyquistEffect
   : public EffectWithSettings<NyquistSettings, StatefulEffect>
{
public:

   /** @param fName File name of the Nyquist script defining this effect. If
    * an empty string, then prompt the user for the Nyquist code to interpret.
    */
   NyquistEffect(const wxString &fName);
   virtual ~NyquistEffect();

   // ComponentInterface implementation

   PluginPath GetPath() const override;
   ComponentInterfaceSymbol GetSymbol() const override;
   VendorSymbol GetVendor() const override;
   wxString GetVersion() const override;
   TranslatableString GetDescription() const override;
   
   ManualPageID ManualPage() const override;
   FilePath HelpPage() const override;

   // EffectDefinitionInterface implementation

   EffectType GetType() const override;
   EffectType GetClassification() const override;
   EffectFamilySymbol GetFamily() const override;
   bool IsInteractive() const override;
   bool IsDefault() const override;
   bool EnablesDebug() const override;

   bool SaveSettings(
      const EffectSettings &settings, CommandParameters & parms) const override;
   bool LoadSettings(
      const CommandParameters & parms, EffectSettings &settings) const final;
   virtual bool DoLoadSettings(
      const CommandParameters *pParms, EffectSettings &settings);

   bool VisitSettings(SettingsVisitor &visitor, EffectSettings &settings)
      override;
   bool VisitSettings(
      ConstSettingsVisitor &visitor, const EffectSettings &settings)
      const final;
   virtual bool DoVisitSettings(
      ConstSettingsVisitor &visitor, const EffectSettings &settings)
      const;

   // Effect implementation

   bool Init() override;
   bool Process(EffectInstance &instance, EffectSettings &settings) override;
   virtual bool AcceptsAllNyquistTypes();
   int ShowHostInterface( wxWindow &parent,
      const EffectDialogFactory &factory,
      std::shared_ptr<EffectInstance> &pInstance, EffectSettingsAccess &access,
      bool forceModal = false) override;
   std::unique_ptr<EffectUIValidator> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance, EffectSettingsAccess &access)
   override;
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool TransferDataFromWindow(EffectSettings &settings) override;

   // NyquistEffect implementation
   // For Nyquist Workbench support
   void RedirectOutput();
   void SetCommand(const wxString &cmd);
   void Continue();
   void Break();
   void Stop();

   void SetDebug(bool value) { mDebug = value; }
   NyquistUIControls &GetControls();
   const NyquistUIControls &GetControls() const;
   NyquistBindings &GetBindings();
   const NyquistBindings &GetBindings() const;
   void SetControls(std::vector</*const*/ NyqControl> controls);
   void SetBindings(std::vector<NyqValue> bindings);
   std::vector</*const*/ NyqControl> MoveControls();
   std::vector<NyqValue> MoveBindings();

private:
   NyquistEnvironment mEnvironment;

   static int mReentryCount;
   // NyquistEffect implementation

   bool ProcessOne(NyquistEnvironment &environment, NyquistTrack &nyquistTrack);

   bool IsOk();
   const TranslatableString &InitializationError() const;

   static FilePaths GetNyquistSearchPath();

   static wxString NyquistToWxString(const char *nyqString);

   void ParseFile();

protected:
   bool ParseCommand(const wxString & cmd);
   virtual bool RecoverParseTypeFailed();

private:
   bool ParseProgram(wxInputStream & stream);

   void OnDebug(wxCommandEvent & evt);

   std::pair<bool, FilePath> CheckHelpPage() const;

protected:
   NyquistParser &GetParser() { return *mParser; }
   const NyquistParser &GetParser() const { return *mParser; }

private:
   //! @invariant not null
   std::unique_ptr<NyquistParser> mParser;

   wxString          mXlispPath;

   wxDateTime        mFileModified; ///< When the script was last modified on disk

protected:
   bool              mExternal{ false };

private:
   wxString          mCmd;      // the command to be processed

private:
   bool              mHelpFileExists;
   FilePath          mHelpPage;

protected:
   bool              mDebug{ false }; // When true, debug window is shown.

private:
   bool              mProjectChanged;

private:
   int               mTrackIndex;
   bool              mFirstInGroup;
   double            mOutputTime;
   unsigned          mCount;
   unsigned          mNumSelectedChannels;

   wxString          mProps;
   wxString          mPerTrackProps;

   friend class NyquistEffectsModule;
};

class NyquistOutputDialog final : public wxDialogWrapper
{
public:
   NyquistOutputDialog(wxWindow * parent, wxWindowID id,
                       const TranslatableString & title,
                       const TranslatableString & prompt,
                       const TranslatableString &message);

private:
   void OnOk(wxCommandEvent & event);

private:
   DECLARE_EVENT_TABLE()
};


#endif
