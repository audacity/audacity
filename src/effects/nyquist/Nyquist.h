/**********************************************************************

  Audacity: A Digital Audio Editor

  Nyquist.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NYQUIST__
#define __AUDACITY_EFFECT_NYQUIST__

#include "StatefulEffect.h"
#include "FileNames.h"
#include "SampleCount.h"
#include "wxPanelWrapper.h"

#include "nyx.h"

class wxArrayString;
class wxFileName;
class wxCheckBox;
class wxTextCtrl;

#define NYQUISTEFFECTS_VERSION wxT("1.0.0.0")

enum NyqControlType
{
   NYQ_CTRL_INT,
   NYQ_CTRL_FLOAT,
   NYQ_CTRL_STRING,
   NYQ_CTRL_CHOICE,
   NYQ_CTRL_INT_TEXT,
   NYQ_CTRL_FLOAT_TEXT,
   NYQ_CTRL_TEXT,
   NYQ_CTRL_TIME,
   NYQ_CTRL_FILE,
};

class NyqControl
{
public:
   NyqControl() = default;
   NyqControl( const NyqControl& ) = default;
   NyqControl &operator = ( const NyqControl & ) = default;
   //NyqControl( NyqControl && ) = default;
   //NyqControl &operator = ( NyqControl && ) = default;

   int type;
   wxString var;
   wxString name;
   wxString label;
   std::vector<EnumValueSymbol> choices;
   FileNames::FileTypes fileTypes;
   wxString valStr;
   wxString lowStr;
   wxString highStr;
   double val;
   double low;
   double high;
   int ticks;
};

struct NyquistSettings {
   // other settings, for the Nyquist prompt; else null
   EffectSettings proxySettings;
   bool proxyDebug{ false };
   std::vector<NyqControl> controls;

   // Other fields, to do
};

class AUDACITY_DLL_API NyquistEffect final
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
      const CommandParameters & parms, EffectSettings &settings) const override;
   bool DoLoadSettings(
      const CommandParameters & parms, EffectSettings &settings);

   bool VisitSettings(SettingsVisitor &visitor, EffectSettings &settings)
      override;
   bool VisitSettings(
      ConstSettingsVisitor &visitor, const EffectSettings &settings)
      const override;
   int SetLispVarsFromParameters(const CommandParameters & parms, bool bTestOnly);

   // Effect implementation

   bool Init() override;
   bool Process(EffectInstance &instance, EffectSettings &settings) override;
   int ShowHostInterface(EffectPlugin &plugin, wxWindow &parent,
      const EffectDialogFactory &factory,
      std::shared_ptr<EffectInstance> &pInstance, EffectSettingsAccess &access,
      bool forceModal = false) override;
   std::unique_ptr<EffectEditor> PopulateOrExchange(
      ShuttleGui & S, EffectInstance &instance,
      EffectSettingsAccess &access, const EffectOutputs *pOutputs) override;
   bool TransferDataToWindow(const EffectSettings &settings) override;
   bool TransferDataFromWindow(EffectSettings &settings) override;

   // NyquistEffect implementation
   // For Nyquist Workbench support
   void RedirectOutput();
   void SetCommand(const wxString &cmd);
   void Continue();
   void Break();
   void Stop();

private:
   wxWeakRef<wxWindow> mUIParent{};

   static int mReentryCount;
   // NyquistEffect implementation

   bool ProcessOne();

   void BuildPromptWindow(ShuttleGui & S);
   void BuildEffectWindow(ShuttleGui & S);

   bool TransferDataToPromptWindow();
   bool TransferDataToEffectWindow();

   bool TransferDataFromPromptWindow();
   bool TransferDataFromEffectWindow();

   bool IsOk();
   const TranslatableString &InitializationError() const { return mInitError; }

   static FilePaths GetNyquistSearchPath();

   static wxString NyquistToWxString(const char *nyqString);
   wxString EscapeString(const wxString & inStr);
   static std::vector<EnumValueSymbol> ParseChoice(const wxString & text);

   FileExtensions ParseFileExtensions(const wxString & text);
   FileNames::FileType ParseFileType(const wxString & text);
   FileNames::FileTypes ParseFileTypes(const wxString & text);

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
   bool ParseCommand(const wxString & cmd);
   bool ParseProgram(wxInputStream & stream);
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
   bool Parse(Tokenizer &tokenizer, const wxString &line, bool eof, bool first);

   static TranslatableString UnQuoteMsgid(const wxString &s, bool allowParens = true,
                           wxString *pExtraString = nullptr);
   static wxString UnQuote(const wxString &s, bool allowParens = true,
                           wxString *pExtraString = nullptr);
   static double GetCtrlValue(const wxString &s);

   void OnLoad(wxCommandEvent & evt);
   void OnSave(wxCommandEvent & evt);
   void OnDebug(wxCommandEvent & evt);

   void OnText(wxCommandEvent & evt);
   void OnSlider(wxCommandEvent & evt);
   void OnChoice(wxCommandEvent & evt);
   void OnTime(wxCommandEvent & evt);
   void OnFileButton(wxCommandEvent & evt);

   static void resolveFilePath(wxString & path, FileExtension extension = {});
   bool validatePath(wxString path);
   wxString ToTimeFormat(double t);

   std::pair<bool, FilePath> CheckHelpPage() const;

private:

   wxString          mXlispPath;

   wxFileName        mFileName;  ///< Name of the Nyquist script file this effect is loaded from
   wxDateTime        mFileModified; ///< When the script was last modified on disk

   bool              mStop;
   bool              mBreak;
   bool              mCont;

   bool              mFoundType;
   bool              mCompiler;
   bool              mTrace;   // True when *tracenable* or *sal-traceback* are enabled
   bool              mIsSal;
   bool              mExternal;
   bool              mIsSpectral;
   bool              mIsTool;
   /** True if the code to execute is obtained interactively from the user via
    * the "Nyquist Effect Prompt", or "Nyquist Prompt", false for all other effects (lisp code read from
    * files)
    */
   const bool        mIsPrompt;
   bool              mOK;
   TranslatableString mInitError;
   wxString          mInputCmd; // history: exactly what the user typed
   wxString          mParameters; // The parameters of to be fed to a nested prompt
   wxString          mCmd;      // the command to be processed
   TranslatableString mName;   ///< Name of the Effect (untranslated)
   TranslatableString mPromptName; // If a prompt, we need to remember original name.
   TranslatableString mAction;
   TranslatableString mInfo;
   TranslatableString mAuthor;
   // Version number of the specific plug-in (not to be confused with mVersion)
   // For shipped plug-ins this will be the same as the Audacity release version
   // when the plug-in was last modified.
   TranslatableString mReleaseVersion;
   TranslatableString mCopyright;
   wxString          mManPage;   // ONLY use if a help page exists in the manual.
   wxString          mHelpFile;
   bool              mHelpFileExists;
   FilePath          mHelpPage;
   EffectType        mType;
   EffectType        mPromptType; // If a prompt, need to remember original type.

   bool              mEnablePreview;
   bool              mDebugButton;  // Set to false to disable Debug button.

   bool              mDebug;        // When true, debug window is shown.
   bool              mRedirectOutput;
   bool              mProjectChanged;
   wxString          mDebugOutputStr;
   TranslatableString mDebugOutput;

   int               mVersion;   // Syntactic version of Nyquist plug-in (not to be confused with mReleaseVersion)
   std::vector<NyqControl>   mControls;

   unsigned          mCurNumChannels;
   WaveTrack         *mCurTrack[2];
   sampleCount       mCurStart[2];
   sampleCount       mCurLen;
   sampleCount       mMaxLen;
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

   WaveTrack        *mOutputTrack[2];

   wxArrayString     mCategories;

   wxString          mProps;
   wxString          mPerTrackProps;

   bool              mRestoreSplits;
   int               mMergeClips;

   wxTextCtrl *mCommandText;

   std::exception_ptr mpException {};

   DECLARE_EVENT_TABLE()

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
