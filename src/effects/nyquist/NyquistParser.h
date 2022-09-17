/**********************************************************************

  Audacity: A Digital Audio Editor

  @fil NyquistParser.h

  Dominic Mazzoni

  Paul Licameli split from Nyquist.h

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NYQUIST_PARSER__
#define __AUDACITY_EFFECT_NYQUIST_PARSER__

#include "NyquistUIControls.h"
#include "PluginManager.h"
#include "SampleCount.h"

// Protect Nyquist from selections greater than 2^31 samples (bug 439)
#define NYQ_MAX_LEN (std::numeric_limits<long>::max())

#define NYQUIST_WORKER_ID wxT("Nyquist Worker")

class Effect;

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
#endif
