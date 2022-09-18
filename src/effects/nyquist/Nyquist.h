/**********************************************************************

  Audacity: A Digital Audio Editor

  Nyquist.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NYQUIST__
#define __AUDACITY_EFFECT_NYQUIST__

#include "NyquistParser.h"
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

struct NyquistProgram : NyquistParser
{
   using NyquistParser::NyquistParser;

   static FilePaths GetNyquistSearchPath();

   // When there is a more general EffectContext for all effects, these will
   // move into that
   struct EffectContext {
      const TrackList *const mInputTracks{};
      TrackList *const mOutputTracks{};
      wxWindow *const mUIParent{};
      const int mNumWaveGroups;
      const bool mIsPreviewing;

      // spectral selection
      const double      mF0;
      const double      mF1;

      // time selection -- which may be modified by processing!
      double            &mT0;
      double            &mT1;

      bool              &mDebug; // When true, debug window is shown.
   };

   struct Context {
      EffectContext     &mContext;

      const unsigned    mNumSelectedChannels;
      const bool        mAcceptsAll{};
      const bool        mExternal{};

      wxString          mProps;
      wxString          mPerTrackProps;
      int               mTrackIndex{ 0 };
      unsigned          mCount{ 0 };
      bool              mProjectChanged{ false };
      double            mOutputTime{ 0 };

      // Keep track of whether the current track is first selected in its
      // sync-lock group (we have no idea what the length of the returned
      // audio will be, so we have to handle sync-lock group behavior the
      // "old" way).
      bool              mFirstInGroup{ true };

   };

   wxString          mCmd;      // the command to be processed
   bool              mHelpFileExists;
   FilePath          mHelpPage;

   NyquistControls &GetControls() { return mControls; }
   const NyquistControls &GetControls() const { return mControls; }
   NyquistBindings &GetBindings() { return mBindings; }
   const NyquistBindings &GetBindings() const { return mBindings; }

   // All state is externalized into context,
   // so the member function can be const
   bool Process(const AudacityProject *project,
      NyquistEnvironment &environment, Context &context,
      EffectSettings &settings) const;
   bool ProcessOne(NyquistEnvironment &environment, Context &context,
      NyquistTrack &nyquistTrack) const;

   std::pair<bool, FilePath> CheckHelpPage() const;
   static wxString NyquistToWxString(const char *nyqString);
};

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

   bool IsOk();
   const TranslatableString &InitializationError() const;

   void ParseFile();

protected:
   bool ParseCommand(const wxString & cmd);
   virtual bool RecoverParseTypeFailed();

private:
   bool ParseProgram(wxInputStream & stream);

   void OnDebug(wxCommandEvent & evt);

protected:
   NyquistParser &GetParser() { return *mProgram; }
   const NyquistParser &GetParser() const { return *mProgram; }

private:
   //! @invariant not null
   std::unique_ptr<NyquistProgram> mProgram;

   wxString          mXlispPath;

   wxDateTime        mFileModified; ///< When the script was last modified on disk

protected:
   bool              mExternal{ false };
   bool              mDebug{ false }; // When true, debug window is shown.

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
