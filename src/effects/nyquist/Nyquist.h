/**********************************************************************

  Audacity: A Digital Audio Editor

  Nyquist.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NYQUIST__
#define __AUDACITY_EFFECT_NYQUIST__

#include "NyquistUIControls.h"
#include "../Effect.h"
#include "SampleCount.h"
#include "../../widgets/wxPanelWrapper.h"

#include "nyx.h"

class wxArrayString;
class wxFileName;
class wxCheckBox;
class wxTextCtrl;

#define NYQUISTEFFECTS_VERSION wxT("1.0.0.0")

// Protect Nyquist from selections greater than 2^31 samples (bug 439)
#define NYQ_MAX_LEN (std::numeric_limits<long>::max())

#define NYQUIST_WORKER_ID wxT("Nyquist Worker")

struct NyquistParser {
   NyquistParser(const wxString &fName, Effect &effect);

   struct Tokenizer {
      bool sl { false };
      bool q { false };
      int paren{ 0 };
      wxString tok;
      wxArrayStringEx tokens;

      bool Tokenize(
         const wxString &line, bool eof,
         size_t trimStart, size_t trimEnd);
   };

   bool Parse(Tokenizer &tokenizer,
      const wxString &line, bool eof, bool first);

   EffectType GetType() const;

protected:
   //! Name of the Effect (untranslated)
   TranslatableString mName;
   //! in correspondence with mControls
   NyquistBindings   mBindings;
   NyquistUIControls mControls;
   //! Name of the Nyquist script file this effect is loaded from
   wxFileName        mFileName;

   bool              mOK{ false };
   bool              mIsTool{ false };
   EffectType        mType{ EffectTypeTool };
   bool              mIsSpectral{ false };
   bool              mIsSal{ false };
   bool              mFoundType{};
   bool              mTrace{ false };   // True when *tracenable* or *sal-traceback* are enabled
   bool              mCompiler{ false };
   TranslatableString mInitError;
   // Syntactic version of Nyquist plug-in (not to be confused with
   // mReleaseVersion)
   int               mVersion{ 4 };
   TranslatableString mAction{ XO("Applying Nyquist Effect...") };
   TranslatableString mInfo;

   bool              mLinear{ false };
   bool              mPreview{ false };

   sampleCount       mMaxLen{ NYQ_MAX_LEN };
   //! Default (auto):  Merge if length remains unchanged.
   int               mMergeClips{ -1 };
   //! Default: Restore split lines.
   bool              mRestoreSplits{ true };
   TranslatableString mAuthor{ XO("n/a") };
   // Version number of the specific plug-in (not to be confused with mVersion)
   // For shipped plug-ins this will be the same as the Audacity release
   // version when the plug-in was last modified.
   TranslatableString mReleaseVersion{ XO("n/a") };
   TranslatableString mCopyright{ XO("n/a") };
   wxString          mManPage;   // ONLY use if a help page exists in the manual.
   wxString          mHelpFile;
   bool              mDebugButton{};  // Set to false to disable Debug button.

   wxArrayString     mCategories;

private:
   static wxString UnQuote(const wxString &s, bool allowParens = true,
      wxString *pExtraString = nullptr);
   static TranslatableString UnQuoteMsgid(
      const wxString &s, bool allowParens = true,
      wxString *pExtraString = nullptr);
   
   static std::vector<EnumValueSymbol> ParseChoice(const wxString & text);
   static FileExtensions ParseFileExtensions(const wxString & text);
   static FileNames::FileType ParseFileType(const wxString & text);
   static FileNames::FileTypes ParseFileTypes(const wxString & text);
};

class AUDACITY_DLL_API NyquistEffect
   : public EffectWithSettings<NyquistSettings, StatefulEffect>
   , protected NyquistParser
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
   void SetControls(std::vector</*const*/ NyqControl> controls)
      { mControls.SetControls(move(controls)); }
   void SetBindings(std::vector<NyqValue> bindings)
      { mBindings = move(bindings); }
   std::vector</*const*/ NyqControl> MoveControls()
      { return mControls.MoveControls(); }
   std::vector<NyqValue> MoveBindings() { return move(mBindings); }

private:
   static int mReentryCount;
   // NyquistEffect implementation

   bool ProcessOne();

   bool IsOk();
   const TranslatableString &InitializationError() const { return mInitError; }

   static FilePaths GetNyquistSearchPath();

   static wxString NyquistToWxString(const char *nyqString);

   static int StaticGetCallback(float *buffer, int channel,
                                int64_t start, int64_t len, int64_t totlen,
                                void *userdata);
   static int StaticPutCallback(float *buffer, int channel,
                                int64_t start, int64_t len, int64_t totlen,
                                void *userdata);
   static void StaticOutputCallback(int c, void *userdata);
   static void StaticOSCallback(void *userdata);

   int GetCallback(float *buffer, int channel,
                   int64_t start, int64_t len, int64_t totlen);
   int PutCallback(float *buffer, int channel,
                   int64_t start, int64_t len, int64_t totlen);
   void OutputCallback(int c);
   void OSCallback();

   void ParseFile();

protected:
   bool ParseCommand(const wxString & cmd);
   virtual bool RecoverParseTypeFailed();

private:
   bool ParseProgram(wxInputStream & stream);

   void OnDebug(wxCommandEvent & evt);

   std::pair<bool, FilePath> CheckHelpPage() const;

private:

   wxString          mXlispPath;

   wxDateTime        mFileModified; ///< When the script was last modified on disk

   bool              mStop{ false };
   bool              mBreak{ false };
   bool              mCont{ false };

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
   bool              mRedirectOutput{ false };
   bool              mProjectChanged;
   wxString          mDebugOutputStr;
   TranslatableString mDebugOutput;

private:
   unsigned          mCurNumChannels;
   WaveTrack         *mCurTrack[2];
   sampleCount       mCurStart[2];
   sampleCount       mCurLen;
   int               mTrackIndex;
   bool              mFirstInGroup;
   double            mOutputTime;
   unsigned          mCount;
   unsigned          mNumSelectedChannels;
   double            mProgressIn;
   double            mProgressOut;
   double            mProgressTot;
   double            mScale;

   using Buffer = std::unique_ptr<float[]>;
   Buffer            mCurBuffer[2];
   sampleCount       mCurBufferStart[2];
   size_t            mCurBufferLen[2];

   WaveTrack        *mOutputTrack[2]{ nullptr, nullptr };

   wxString          mProps;
   wxString          mPerTrackProps;

   std::exception_ptr mpException {};

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
