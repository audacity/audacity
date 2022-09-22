/*!********************************************************************

  Audacity: A Digital Audio Editor

  NyquistParser.cpp

  Dominic Mazzoni

  Paul Licameli split from Nyquist.cpp

**********************************************************************/
#include "NyquistParser.h"

#include "NyquistFormatting.h"
#include <wx/log.h>
#include <wx/tokenzr.h>
#include <float.h>

NyquistParser::NyquistParser(const wxString &fName, NyquistEffectBase &effect)
   : mName{
      (fName == NYQUIST_PROMPT_ID) ? NYQUIST_PROMPT_NAME
   /* i18n-hint: It is acceptable to translate this the same as for "Nyquist Prompt" */
      : (fName == NYQUIST_WORKER_ID) ? XO("Nyquist Worker")
   // Use the file name verbatim as effect name.
   // This is only a default name, overridden if we find a $name line:
      : Verbatim(wxFileName{ fName }.GetName())
   }
   , mControls{ effect, mBindings }
{
   if (!(fName == NYQUIST_PROMPT_ID || fName == NYQUIST_WORKER_ID)) {
      mFileName = fName;
   }
}

EffectType NyquistParser::GetType() const
{
   return mType;
}

std::vector<EnumValueSymbol> NyquistParser::ParseChoice(const wxString & text)
{
   std::vector<EnumValueSymbol> results;
   if (text[0] == wxT('(')) {
      // New style:  expecting a Lisp-like list of strings
      Tokenizer tzer;
      tzer.Tokenize(text, true, 1, 1);
      auto &choices = tzer.tokens;
      wxString extra;
      for (auto &choice : choices) {
         auto label = UnQuote(choice, true, &extra);
         if (extra.empty())
            results.push_back( TranslatableString{ label, {} } );
         else
            results.push_back(
               { extra, TranslatableString{ label, {} } } );
      }
   }
   else {
      // Old style: expecting a comma-separated list of
      // un-internationalized names, ignoring leading and trailing spaces
      // on each; and the whole may be quoted
      auto choices = wxStringTokenize(
         text[0] == wxT('"') ? text.Mid(1, text.length() - 2) : text,
         wxT(",")
      );
      for (auto &choice : choices)
         results.push_back( { choice.Trim(true).Trim(false) } );
   }
   return results;
}

FileExtensions NyquistParser::ParseFileExtensions(const wxString & text)
{
   // todo: error handling
   FileExtensions results;
   if (text[0] == wxT('(')) {
      Tokenizer tzer;
      tzer.Tokenize(text, true, 1, 1);
      for (const auto &token : tzer.tokens)
         results.push_back( UnQuote( token ) );
   }
   return results;
}

FileNames::FileType NyquistParser::ParseFileType(const wxString & text)
{
   // todo: error handling
   FileNames::FileType result;
   if (text[0] == wxT('(')) {
      Tokenizer tzer;
      tzer.Tokenize(text, true, 1, 1);
      auto &tokens = tzer.tokens;
      if ( tokens.size() == 2 )
         result =
            { UnQuoteMsgid( tokens[0] ), ParseFileExtensions( tokens[1] ) };
   }
   return result;
}

FileNames::FileTypes NyquistParser::ParseFileTypes(const wxString & text)
{
   // todo: error handling
   FileNames::FileTypes results;
   if (text[0] == wxT('(')) {
      Tokenizer tzer;
      tzer.Tokenize(text, true, 1, 1);
      auto &types = tzer.tokens;
      if ( !types.empty() && types[0][0] == wxT('(') )
         for (auto &type : types)
            results.push_back( ParseFileType( type ) );
   }
   if ( results.empty() ) {
      // Old-style is a specially formatted string, maybe translated
      // Parse it for compatibility
      auto str = UnQuote( text );
      auto pieces = wxSplit( str, '|' );
      // Should have an even number
      auto size = pieces.size();
      if ( size % 2 == 1 )
         --size, pieces.pop_back();
      for ( size_t ii = 0; ii < size; ii += 2 ) {
         FileExtensions extensions;
         auto extensionStrings = wxSplit( pieces[ii + 1], ';' );
         for ( const auto &extensionString : extensionStrings )
            if ( extensionString.StartsWith( wxT("*.") ) ) {
               auto ext = extensionString.substr( 2 );
               if (ext == wxT("*"))
                  // "*.*" to match all
                  ext.clear();
               extensions.push_back( ext );
            }
         results.push_back( { Verbatim( pieces[ii] ), extensions } );
      }
   }
   return results;
}

TranslatableString NyquistParser::UnQuoteMsgid(
   const wxString &s, bool allowParens, wxString *pExtraString)
{
   if (pExtraString)
      *pExtraString = wxString{};

   int len = s.length();
   if (len >= 2 && s[0] == wxT('\"') && s[len - 1] == wxT('\"')) {
      auto unquoted = s.Mid(1, len - 2);
      // Sorry, no context strings, yet
      // (See also comments in NyquistEffectsModule::AutoRegisterPlugins)
      return TranslatableString{ unquoted, {} };
   }
   else if (allowParens &&
            len >= 2 && s[0] == wxT('(') && s[len - 1] == wxT(')')) {
      Tokenizer tzer;
      tzer.Tokenize(s, true, 1, 1);
      auto &tokens = tzer.tokens;
      if (tokens.size() > 1) {
         if (pExtraString && tokens[1][0] == '(') {
            // A choice with a distinct internal string form like
            // ("InternalString" (_ "Visible string"))
            // Recur to find the two strings
            *pExtraString = UnQuote(tokens[0], false);
            return UnQuoteMsgid(tokens[1]);
         }
         else {
            // Assume the first token was _ -- we don't check that
            // And the second is the string, which is internationalized
            // Sorry, no context strings, yet
            return UnQuoteMsgid( tokens[1], false );
         }
      }
      else
         return {};
   }
   else
      // If string was not quoted, assume no translation exists
      return Verbatim( s );
}

wxString NyquistParser::UnQuote(
   const wxString &s, bool allowParens, wxString *pExtraString)
{
   return UnQuoteMsgid( s, allowParens, pExtraString ).Translation();
}

bool NyquistParser::Tokenizer::Tokenize(
   const wxString &line, bool eof,
   size_t trimStart, size_t trimEnd)
{
   auto endToken = [&]{
      if (!tok.empty()) {
         tokens.push_back(tok);
         tok = wxT("");
      }
   };

   for (auto c :
        make_iterator_range(line.begin() + trimStart, line.end() - trimEnd)) {
      if (q && !sl && c == wxT('\\')) {
         // begin escaped character, only within quotes
         sl = true;
         continue;
      }

      if (!sl && c == wxT('"')) {
         // Unescaped quote
         if (!q) {
            // start of string
            if (!paren)
               // finish previous token
               endToken();
            // Include the delimiter in the token
            tok += c;
            q = true;
         }
         else {
            // end of string
            // Include the delimiter in the token
            tok += c;
            if (!paren)
               endToken();
            q = false;
         }
      }
      else if (!q && !paren && (c == wxT(' ') || c == wxT('\t')))
         // Unenclosed whitespace
         // Separate tokens; don't accumulate this character
         endToken();
      else if (!q && c == wxT(';'))
         // semicolon not in quotes, but maybe in parentheses
         // Lisp style comments with ; (but not with #| ... |#) are allowed
         // within a wrapped header multi-line, so that i18n hint comments may
         // be placed before strings and found by xgettext
         break;
      else if (!q && c == wxT('(')) {
         // Start of list or sublist
         if (++paren == 1)
            // finish previous token; begin list, including the delimiter
            endToken(), tok += c;
         else
            // defer tokenizing of nested list to a later pass over the token
            tok += c;
      }
      else if (!q && c == wxT(')')) {
         // End of list or sublist
         if (--paren == 0)
            // finish list, including the delimiter
            tok += c, endToken();
         else if (paren < 0)
            // forgive unbalanced right paren
            paren = 0, endToken();
         else
            // nested list; deferred tokenizing
            tok += c;
      }
      else {
         if (sl && paren)
            // Escaped character in string inside list, to be parsed again
            // Put the escape back for the next pass
            tok += wxT('\\');
         if (sl && !paren && c == 'n')
            // Convert \n to newline, the only special escape besides \\ or \"
            // But this should not be used if a string needs to localize.
            // Instead, simply put a line break in the string.
            c = '\n';
         tok += c;
      }

      sl = false;
   }

   if (eof || (!q && !paren)) {
      endToken();
      return true;
   }
   else {
      // End of line but not of file, and a string or list is yet unclosed
      // If a string, accumulate a newline character
      if (q)
         tok += wxT('\n');
      return false;
   }
}

static void SetControlBounds(NyqControl &ctrl);

bool NyquistParser::Parse(
   Tokenizer &tzer, const wxString &line, bool eof, bool first)
{
   if ( !tzer.Tokenize(line, eof, first ? 1 : 0, 0) )
      return false;

   const auto &tokens = tzer.tokens;
   int len = tokens.size();
   if (len < 1) {
      return true;
   }

   // Consistency decision is for "plug-in" as the correct spelling
   // "plugin" (deprecated) is allowed as an undocumented convenience.
   if (len == 2 && tokens[0] == wxT("nyquist") &&
      (tokens[1] == wxT("plug-in") || tokens[1] == wxT("plugin"))) {
      mOK = true;
      return true;
   }

   if (len >= 2 && tokens[0] == wxT("type")) {
      wxString tok = tokens[1];
      mIsTool = false;
      if (tok == wxT("tool")) {
         mIsTool = true;
         mType = EffectTypeTool;
         // we allow
         // ;type tool
         // ;type tool process
         // ;type tool generate
         // ;type tool analyze
         // The last three are placed in the tool menu, but are processed as
         // process, generate or analyze.
         if (len >= 3)
            tok = tokens[2];
      }

      if (tok == wxT("process")) {
         mType = EffectTypeProcess;
      }
      else if (tok == wxT("generate")) {
         mType = EffectTypeGenerate;
      }
      else if (tok == wxT("analyze")) {
         mType = EffectTypeAnalyze;
      }

      if (len >= 3 && tokens[2] == wxT("spectral")) {;
         mIsSpectral = true;
      }
      return true;
   }

   if (len == 2 && tokens[0] == wxT("codetype")) {
      // This will stop ParseProgram() from doing a best guess as program type.
      if (tokens[1] == wxT("lisp")) {
         mIsSal = false;
         mFoundType = true;
      }
      else if (tokens[1] == wxT("sal")) {
         mIsSal = true;
         mFoundType = true;
      }
      return true;
   }

   if (len >= 2 && tokens[0] == wxT("debugflags")) {
      for (int i = 1; i < len; i++) {
         // "trace" sets *tracenable* (LISP) or *sal-traceback* (SAL)
         // and displays debug window IF there is anything to show.
         if (tokens[i] == wxT("trace")) {
            mTrace = true;
         }
         else if (tokens[i] == wxT("notrace")) {
            mTrace = false;
         }
         else if (tokens[i] == wxT("compiler")) {
            mCompiler = true;
         }
         else if (tokens[i] == wxT("nocompiler")) {
            mCompiler = false;
         }
      }
      return true;
   }

   // We support versions 1, 2 and 3
   // (Version 2 added support for string parameters.)
   // (Version 3 added support for choice parameters.)
   // (Version 4 added support for project/track/selection information.)
   if (len >= 2 && tokens[0] == wxT("version")) {
      long v;
      tokens[1].ToLong(&v);
      if (v < 1 || v > 4) {
         // This is an unsupported plug-in version
         mOK = false;
         mInitError = XO(
"This version of Audacity does not support Nyquist plug-in version %ld")
            .Format( v );
         return true;
      }
      mVersion = (int) v;
   }

   if (len >= 2 && tokens[0] == wxT("name")) {
      // Names do not yet support context strings for translations, or
      // internal names distinct from visible English names.
      // (See also comments in NyquistEffectsModule::AutoRegisterPlugins)
      auto name = UnQuote(tokens[1]);
      // Strip ... from name if it's present, perhaps in third party plug-ins
      // Menu system puts ... back if there are any controls
      // This redundant naming convention must NOT be followed for
      // shipped Nyquist effects with internationalization.  Else the msgid
      // later looked up will lack the ... and will not be found.
      if (name.EndsWith(wxT("...")))
         name = name.RemoveLast(3);
      mName = TranslatableString{ name, {} };
      return true;
   }

   if (len >= 2 && tokens[0] == wxT("action")) {
      mAction = TranslatableString{ UnQuote(tokens[1]), {} };
      return true;
   }

   if (len >= 2 && tokens[0] == wxT("info")) {
      mInfo = TranslatableString{ UnQuote(tokens[1]), {} };
      return true;
   }

   if (len >= 2 && tokens[0] == wxT("preview")) {
      if (tokens[1] == wxT("enabled") || tokens[1] == wxT("true")) {
         mControls.mEnablePreview = true;
         mLinear = false;
      }
      else if (tokens[1] == wxT("linear")) {
         mControls.mEnablePreview = true;
         mLinear = true;
      }
      else if (tokens[1] == wxT("selection")) {
         mControls.mEnablePreview = true;
         mPreview = true;
      }
      else if (tokens[1] == wxT("disabled") || tokens[1] == wxT("false")) {
         mControls.mEnablePreview = false;
      }
      return true;
   }

   // Maximum number of samples to be processed. This can help the
   // progress bar if effect does not process all of selection.
   if (len >= 2 && tokens[0] == wxT("maxlen")) {
      long long v; // Note that Nyquist may overflow at > 2^31 samples (bug 439)
      tokens[1].ToLongLong(&v);
      mMaxLen = (sampleCount) v;
   }

#if defined(EXPERIMENTAL_NYQUIST_SPLIT_CONTROL)
   if (len >= 2 && tokens[0] == wxT("mergeclips")) {
      long v;
      // -1 = auto (default), 0 = don't merge clips, 1 = do merge clips
      tokens[1].ToLong(&v);
      mMergeClips = v;
      return true;
   }

   if (len >= 2 && tokens[0] == wxT("restoresplits")) {
      long v;
      // Splits are restored by default. Set to 0 to prevent.
      tokens[1].ToLong(&v);
      mRestoreSplits = !!v;
      return true;
   }
#endif

   if (len >= 2 && tokens[0] == wxT("author")) {
      mAuthor = TranslatableString{ UnQuote(tokens[1]), {} };
      return true;
   }

   if (len >= 2 && tokens[0] == wxT("release")) {
      // Value must be quoted if the release version string contains spaces.
      mReleaseVersion =
         TranslatableString{ UnQuote(tokens[1]), {} };
      return true;
   }

   if (len >= 2 && tokens[0] == wxT("copyright")) {
      mCopyright = TranslatableString{ UnQuote(tokens[1]), {} };
      return true;
   }

   // Page name in Audacity development manual
   if (len >= 2 && tokens[0] == wxT("manpage")) {
      // do not translate
      mManPage = UnQuote(tokens[1], false);
      return true;
   }

   // Local Help file
   if (len >= 2 && tokens[0] == wxT("helpfile")) {
      // do not translate
      mHelpFile = UnQuote(tokens[1], false);
      return true;
   }

   // Debug button may be disabled for release plug-ins.
   if (len >= 2 && tokens[0] == wxT("debugbutton")) {
      if (tokens[1] == wxT("disabled") || tokens[1] == wxT("false")) {
         mDebugButton = false;
      }
      return true;
   }

   using namespace NyquistFormatting;

   if (len >= 3 && tokens[0] == wxT("control")) {
      NyqControl ctrl;
      NyqValue binding;

      if (len == 3 && tokens[1] == wxT("text")) {
         ctrl.var = tokens[1];
         ctrl.label = UnQuote( tokens[2] );
         ctrl.type = NYQ_CTRL_TEXT;
      }
      else if (len >= 5)
      {
         ctrl.var = tokens[1];
         ctrl.name = UnQuote( tokens[2] );
         // 3 is type, below
         ctrl.label = tokens[4];

         // valStr may or may not be a quoted string
         binding.valStr = len > 5 ? tokens[5] : wxString{};
         binding.val = GetCtrlValue(binding.valStr);
         if (binding.valStr.length() > 0 &&
               (binding.valStr[0] == wxT('(') ||
                binding.valStr[0] == wxT('"')))
            binding.valStr = UnQuote( binding.valStr );

         // 6 is minimum, below
         // 7 is maximum, below

         if (tokens[3] == wxT("string")) {
            ctrl.type = NYQ_CTRL_STRING;
            ctrl.label = UnQuote( ctrl.label );
         }
         else if (tokens[3] == wxT("choice")) {
            ctrl.type = NYQ_CTRL_CHOICE;
            ctrl.choices = ParseChoice(ctrl.label);
            ctrl.label = wxT("");
         }
         else if (tokens[3] == wxT("file")) {
            ctrl.type = NYQ_CTRL_FILE;
            ctrl.fileTypes = ParseFileTypes(tokens[6]);
            // will determine file dialog styles:
            ctrl.highStr = UnQuote( tokens[7] );
            ctrl.label = UnQuote(ctrl.label);
         }
         else {
            ctrl.label = UnQuote( ctrl.label );

            if (len < 8) {
               return true;
            }

            if ((tokens[3] == wxT("float")) ||
                  (tokens[3] == wxT("real"))) // Deprecated
               ctrl.type = NYQ_CTRL_FLOAT;
            else if (tokens[3] == wxT("int"))
               ctrl.type = NYQ_CTRL_INT;
            else if (tokens[3] == wxT("float-text"))
               ctrl.type = NYQ_CTRL_FLOAT_TEXT;
            else if (tokens[3] == wxT("int-text"))
               ctrl.type = NYQ_CTRL_INT_TEXT;
            else if (tokens[3] == wxT("time"))
                ctrl.type = NYQ_CTRL_TIME;
            else
            {
               wxString str;
               str.Printf(wxT("Bad Nyquist 'control' type specification: '%s' in plug-in file '%s'.\nControl not created."),
                        tokens[3], mFileName.GetFullPath());

               // Too disturbing to show alert before Audacity frame is up.
               //    Effect::MessageBox(
               //       str,
               //       wxOK | wxICON_EXCLAMATION,
               //       XO("Nyquist Warning") );

               // Note that the AudacityApp's mLogger has not yet been created,
               // so this brings up an alert box, but after the Audacity frame is up.
               wxLogWarning(str);
               return true;
            }

            ctrl.lowStr = UnQuote( tokens[6] );
            if (ctrl.type == NYQ_CTRL_INT_TEXT && ctrl.lowStr.IsSameAs(wxT("nil"), false)) {
               ctrl.low = INT_MIN;
            }
            else if (ctrl.type == NYQ_CTRL_FLOAT_TEXT && ctrl.lowStr.IsSameAs(wxT("nil"), false)) {
               ctrl.low = -(FLT_MAX);
            }
            else if (ctrl.type == NYQ_CTRL_TIME && ctrl.lowStr.IsSameAs(wxT("nil"), false)) {
                ctrl.low = 0.0;
            }
            else {
               ctrl.low = GetCtrlValue(ctrl.lowStr);
            }

            ctrl.highStr = UnQuote( tokens[7] );
            if (ctrl.type == NYQ_CTRL_INT_TEXT && ctrl.highStr.IsSameAs(wxT("nil"), false)) {
               ctrl.high = INT_MAX;
            }
            else if ((ctrl.type == NYQ_CTRL_FLOAT_TEXT || ctrl.type == NYQ_CTRL_TIME) &&
                      ctrl.highStr.IsSameAs(wxT("nil"), false))
            {
               ctrl.high = FLT_MAX;
            }
            else {
               ctrl.high = GetCtrlValue(ctrl.highStr);
            }

            if (ctrl.high < ctrl.low) {
               ctrl.high = ctrl.low;
            }

            if (binding.val < ctrl.low) {
               binding.val = ctrl.low;
            }

            if (binding.val > ctrl.high) {
               binding.val = ctrl.high;
            }

            ctrl.ticks = 1000;
            if (ctrl.type == NYQ_CTRL_INT &&
               (ctrl.high - ctrl.low < ctrl.ticks)) {
               ctrl.ticks = (int)(ctrl.high - ctrl.low);
            }
         }
      }

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
      // Skip creation of controls and bindings for spectral editing
      // Will instead initialize Lisp variables specially before processing,
      // taking values from the given selection
      if (!(ctrl.var.mName == "control-f0" || ctrl.var.mName == "control-f1"))
#endif
      {
         SetControlBounds(ctrl);
         mControls.emplace_back(std::move(ctrl));
         mBindings.push_back(binding);
      }
   }

   // Deprecated
   if (len >= 2 && tokens[0] == wxT("categories")) {
      for (size_t i = 1; i < tokens.size(); ++i) {
         mCategories.push_back(tokens[i]);
      }
   }
   return true;
}

void SetControlBounds(NyqControl &ctrl)
{
   using namespace NyquistFormatting;
   if (ctrl.type == NYQ_CTRL_INT_TEXT && ctrl.lowStr.IsSameAs(wxT("nil"), false)) {
      ctrl.low = INT_MIN;
   }
   else if ((ctrl.type == NYQ_CTRL_FLOAT_TEXT || ctrl.type == NYQ_CTRL_TIME) &&
            ctrl.lowStr.IsSameAs(wxT("nil"), false))
   {
      ctrl.low = -(FLT_MAX);
   }
   else
   {
      ctrl.low = GetCtrlValue(ctrl.lowStr);
   }

   if (ctrl.type == NYQ_CTRL_INT_TEXT && ctrl.highStr.IsSameAs(wxT("nil"), false)) {
      ctrl.high = INT_MAX;
   }
   else if ((ctrl.type == NYQ_CTRL_FLOAT_TEXT || ctrl.type == NYQ_CTRL_TIME) &&
            ctrl.highStr.IsSameAs(wxT("nil"), false))
   {
      ctrl.high = FLT_MAX;
   }
   else
   {
      ctrl.high = GetCtrlValue(ctrl.highStr);
   }

   if (ctrl.high < ctrl.low)
   {
      ctrl.high = ctrl.low + 1;
   }

   ctrl.ticks = 1000;
   if (ctrl.type == NYQ_CTRL_INT &&
       (ctrl.high - ctrl.low < ctrl.ticks))
   {
      ctrl.ticks = (int)(ctrl.high - ctrl.low);
   }
}
