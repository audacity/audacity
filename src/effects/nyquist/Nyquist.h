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
#include <wx/textctrl.h>
#include <wx/tokenzr.h>

#include "../Effect.h"

#include "nyx.h"

#include <string>

class NyqControl
{
 public:
   wxString var;
   wxString name;
   int type;
   wxString label;
   wxString valStr;
   wxString lowStr;
   wxString highStr;
   double val;
   double low;
   double high;
   int ticks;
};
#define NYQ_CTRL_INT 0
#define NYQ_CTRL_REAL 1
#define NYQ_CTRL_STRING 2
#define NYQ_CTRL_CHOICE 3

WX_DECLARE_USER_EXPORTED_OBJARRAY(NyqControl,  NyqControlArray, AUDACITY_DLL_API);

class AUDACITY_DLL_API EffectNyquist:public Effect
{
 public:

   /** @param fName File name of the Nyquist script defining this effect. If
    * an empty string, then prompt the user for the Nyquist code to interpret.
    */
   EffectNyquist(wxString fName);
   virtual ~EffectNyquist();

   bool SetXlispPath();

   bool LoadedNyFile() {
      return mOK;
   }

   void Continue();
   void Break();
   void Stop();

   void SetCommand(wxString cmd);
   wxString GetOutput();

   /** Get the name of the effect (taken from the script that is loaded). Note
    * that this name is currently not translated because the translations system
    * doesn't see the lisp files containing the Nyquist effect scripts.
    * @return The name of the effect */
   virtual wxString GetEffectName() {
      return mName;
   }

   virtual std::set<wxString> GetEffectCategories() {
      std::set<wxString> cats;
      for (size_t i = 0; i < mCategories.GetCount(); i++) {
         cats.insert(mCategories[i]);
      }
      return cats;
   }

   virtual wxString GetEffectIdentifier() {
      if (mInteractive) {
         // Disabled for now...
         return wxT("");
      }

      wxStringTokenizer st(mName, wxT(" "));
      wxString id;

      // CamelCase the name
      while (st.HasMoreTokens()) {
         wxString tok = st.GetNextToken();

         id += tok.Left(1).MakeUpper() + tok.Mid(1);
      }

      return id;
   }

   virtual wxString GetEffectAction() {
      return mAction;
   }

   virtual bool PromptUser();

   virtual bool Process();

   // Batch chain support
   virtual bool SupportsChains();
   virtual bool TransferParameters( Shuttle & shuttle );

 private:

   static wxString NyquistToWxString(const char *nyqString);

   bool ProcessOne();

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

   void Parse(wxString line);
   void ParseFile();
   wxString UnQuote(wxString s);
   double GetCtrlValue(wxString s);

 private:

   wxString          mXlispPath;

   wxFileName        mFileName;  ///< Name of the Nyquist script file this effect is loaded from
   wxDateTime        mFileModified; ///< When the script was last modified on disk

   bool              mStop;
   bool              mBreak;
   bool              mCont;

   bool              mCompiler;
   bool              mIsSal;
   bool              mExternal;
   /** True if the code to execute is obtained interactively from the user via
    * the "Nyquist Prompt", false for all other effects (lisp code read from
    * files)
    */
   bool              mInteractive;
   bool              mOK;
   wxString          mInputCmd; // history: exactly what the user typed
   wxString          mCmd;      // the command to be processed
   wxString          mName;   ///< Name of the Effect
   wxString          mAction;
   wxString          mInfo;
   bool              mDebug;
   std::string       mDebugOutput;

   NyqControlArray   mControls;

   int               mCurNumChannels;
   WaveTrack         *mCurTrack[2];
   sampleCount       mCurStart[2];
   sampleCount       mCurLen;
   bool              mFirstInGroup;
   double            mOutputTime;
   int               mCount;
   double            mProgressIn;
   double            mProgressOut;
   double            mProgressTot;
   double            mScale;

   samplePtr         mCurBuffer[2];
   sampleCount       mCurBufferStart[2];
   sampleCount       mCurBufferLen[2];

   WaveTrack         *mOutputTrack[2];

   wxArrayString     mCategories;
};

class NyquistDialog:public wxDialog
{
 public:
   // constructors and destructors
   NyquistDialog(wxWindow * parent, wxWindowID id,
                 const wxString & title,
                 wxString info,
                 NyqControlArray *controlArray);

 private:
   NyqControlArray  *mControls;
   bool              mInHandler;

   void OnText(wxCommandEvent & event);
   void OnSlider(wxCommandEvent & event);
   void OnChoice( wxCommandEvent &event );
   void OnOk(wxCommandEvent & event);
   void OnDebug(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

 private:
   DECLARE_EVENT_TABLE()

};

class NyquistInputDialog:public wxDialog
{
 public:
   NyquistInputDialog(wxWindow * parent, wxWindowID id,
                      const wxString & title,
                      const wxString & prompt,
                      wxString initialCommand);

   wxString GetCommand();

 private:
   wxTextCtrl *mCommandText;

   void OnOk(wxCommandEvent & event);
   void OnDebug(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

 private:
   DECLARE_EVENT_TABLE()
};

class NyquistOutputDialog:public wxDialog
{
 public:
   NyquistOutputDialog(wxWindow * parent, wxWindowID id,
                       const wxString & title,
                       const wxString & prompt,
                       wxString message);

 private:
   void OnOk(wxCommandEvent & event);

 private:
   DECLARE_EVENT_TABLE()
};


#endif
