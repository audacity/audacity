/**********************************************************************

  Audacity: A Digital Audio Editor

  Nyquist.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NYQUIST__
#define __AUDACITY_EFFECT_NYQUIST__

#include <wx/button.h>
#include <wx/datetime.h>
#include <wx/dialog.h>
#include <wx/filename.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/stattext.h>
#include <wx/textbuf.h>
#include <wx/textctrl.h>
#include <wx/tokenzr.h>

#include "../Effect.h"

#include "nyx.h"

#define NYQUISTEFFECTS_VERSION wxT("1.0.0.0")
#define NYQUISTEFFECTS_FAMILY wxT("Nyquist")

#define NYQUIST_PROMPT_ID wxT("Nyquist Prompt")
#define NYQUIST_WORKER_ID wxT("Nyquist Worker")

enum NyqControlType
{
   NYQ_CTRL_INT,
   NYQ_CTRL_REAL,
   NYQ_CTRL_STRING,
   NYQ_CTRL_CHOICE,
   NYQ_CTRL_INT_TEXT,
   NYQ_CTRL_FLOAT_TEXT
};

class NyqControl
{
public:
   int type;
   wxString var;
   wxString name;
   wxString label;
   wxString valStr;
   wxString lowStr;
   wxString highStr;
   double val;
   double low;
   double high;
   int ticks;
};

WX_DECLARE_USER_EXPORTED_OBJARRAY(NyqControl,  NyqControlArray, AUDACITY_DLL_API);

class AUDACITY_DLL_API NyquistEffect final : public Effect
{
public:

   /** @param fName File name of the Nyquist script defining this effect. If
    * an empty string, then prompt the user for the Nyquist code to interpret.
    */
   NyquistEffect(const wxString &fName);
   virtual ~NyquistEffect();

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

   bool GetAutomationParameters(EffectAutomationParameters & parms) override;
   bool SetAutomationParameters(EffectAutomationParameters & parms) override;

   // Effect implementation
   
   bool Init() override;
   bool CheckWhetherSkipEffect() override;
   bool Process() override;
   bool ShowInterface(wxWindow *parent, bool forceModal = false) override;
   void PopulateOrExchange(ShuttleGui & S) override;
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

   // NyquistEffect implementation

   // For Nyquist Workbench support
   void RedirectOutput();
   void SetCommand(const wxString &cmd);
   void Continue();
   void Break();
   void Stop();

private:
   // NyquistEffect implementation

   bool ProcessOne();

   void BuildPromptWindow(ShuttleGui & S);
   void BuildEffectWindow(ShuttleGui & S);

   bool TransferDataToPromptWindow();
   bool TransferDataToEffectWindow();

   bool TransferDataFromPromptWindow();
   bool TransferDataFromEffectWindow();

   bool IsOk();

   static wxArrayString GetNyquistSearchPath();

   static wxString NyquistToWxString(const char *nyqString);
   wxString EscapeString(const wxString & inStr);
   wxArrayString ParseChoice(const NyqControl & ctrl);

   static int StaticGetCallback(float *buffer, int channel,
                                long start, long len, long totlen,
                                void *userdata);
   static int StaticPutCallback(float *buffer, int channel,
                                long start, long len, long totlen,
                                void *userdata);
   static void StaticOutputCallback(int c, void *userdata);
   static void StaticOSCallback(void *userdata);

   int GetCallback(float *buffer, int channel,
                   long start, long len, long totlen);
   int PutCallback(float *buffer, int channel,
                   long start, long len, long totlen);
   void OutputCallback(int c);
   void OSCallback();

   void ParseFile();
   bool ParseCommand(const wxString & cmd);
   bool ParseProgram(wxInputStream & stream);
   void Parse(const wxString &line);

   wxString UnQuote(const wxString &s);
   double GetCtrlValue(const wxString &s);

   void OnLoad(wxCommandEvent & evt);
   void OnSave(wxCommandEvent & evt);
   void OnDebug(wxCommandEvent & evt);

   void OnText(wxCommandEvent & evt);
   void OnSlider(wxCommandEvent & evt);
   void OnChoice(wxCommandEvent & evt);

private:

   wxString          mXlispPath;

   wxFileName        mFileName;  ///< Name of the Nyquist script file this effect is loaded from
   wxDateTime        mFileModified; ///< When the script was last modified on disk

   bool              mStop;
   bool              mBreak;
   bool              mCont;

   bool              mFoundType;
   bool              mCompiler;
   bool              mIsSal;
   bool              mExternal;
   bool              mIsSpectral;
   /** True if the code to execute is obtained interactively from the user via
    * the "Nyquist Prompt", false for all other effects (lisp code read from
    * files)
    */
   bool              mIsPrompt;
   bool              mOK;
   wxString          mInputCmd; // history: exactly what the user typed
   wxString          mCmd;      // the command to be processed
   wxString          mName;   ///< Name of the Effect
   wxString          mAction;
   wxString          mInfo;
   wxString          mAuthor;
   wxString          mCopyright;
   EffectType        mType;

   bool              mEnablePreview;
   bool              mDebug;
   bool              mRedirectOutput;
   bool              mProjectChanged;
   wxString          mDebugOutput;

   int               mVersion;
   NyqControlArray   mControls;

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

   SampleBuffer      mCurBuffer[2];
   sampleCount       mCurBufferStart[2];
   size_t            mCurBufferLen[2];

   std::unique_ptr<WaveTrack> mOutputTrack[2];

   wxArrayString     mCategories;

   wxString          mProps;
   wxString          mPerTrackProps;

   bool              mRestoreSplits;
   int               mMergeClips;

   wxTextCtrl *mCommandText;
   wxCheckBox *mVersionCheckBox;

   DECLARE_EVENT_TABLE()

   friend class NyquistEffectsModule;
};

class NyquistOutputDialog final : public wxDialogWrapper
{
public:
   NyquistOutputDialog(wxWindow * parent, wxWindowID id,
                       const wxString & title,
                       const wxString & prompt,
                       const wxString &message);

private:
   void OnOk(wxCommandEvent & event);

private:
   DECLARE_EVENT_TABLE()
};


#endif
