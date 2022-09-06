/**********************************************************************

  Audacity: A Digital Audio Editor

  @file NyquistControls.cpp

  Dominic Mazzoni

  Paul Licameli split from Nyquist.cpp

******************************************************************//**

\class NyqControl
\brief A control on a NyquistDialog.

*//*******************************************************************/


#include "NyquistControls.h"

#include <unordered_map>
#include "../../ShuttleAutomation.h"
#include "wxFileNameWrapper.h"
#include <float.h>

void NyquistControls::Visit(
   const Bindings &bindings, ConstSettingsVisitor &visitor) const
{
   auto pBinding = bindings.cbegin();
   for (const auto &ctrl : mControls) {
      auto &binding = *pBinding++;
      double d = binding.val;

      if (d == UNINITIALIZED_CONTROL && ctrl.type != NYQ_CTRL_STRING)
         d = NyquistFormatting::GetCtrlValue(binding.valStr);

      if (ctrl.type == NYQ_CTRL_FLOAT || ctrl.type == NYQ_CTRL_FLOAT_TEXT ||
          ctrl.type == NYQ_CTRL_TIME)
         visitor.Define( d, static_cast<const wxChar*>( ctrl.var.c_str() ),
            (double)0.0, ctrl.low, ctrl.high, 1.0);
      else if (ctrl.type == NYQ_CTRL_INT || ctrl.type == NYQ_CTRL_INT_TEXT) {
         int x = d;
         visitor.Define( x, static_cast<const wxChar*>( ctrl.var.c_str() ), 0,
            static_cast<int>(ctrl.low), static_cast<int>(ctrl.high), 1);
         //parms.Write(ctrl.var, (int) d);
      }
      else if (ctrl.type == NYQ_CTRL_CHOICE) {
         // untranslated
         int x = d;
         //parms.WriteEnum(ctrl.var, (int) d, choices);
         visitor.DefineEnum( x, static_cast<const wxChar*>( ctrl.var.c_str() ),
            0, ctrl.choices.data(), ctrl.choices.size() );
      }
      else if (ctrl.type == NYQ_CTRL_STRING || ctrl.type == NYQ_CTRL_FILE) {
         visitor.Define( binding.valStr, ctrl.var,
            wxString{}, ctrl.lowStr, ctrl.highStr );
         //parms.Write(ctrl.var, ctrl.valStr);
      }
   }
}

bool NyquistControls::Save(
   const Bindings &bindings, CommandParameters & parms) const
{
   auto pBinding = bindings.cbegin();
   for (const auto &ctrl : mControls) {
      auto &binding = *pBinding++;
      double d = binding.val;

      if (d == UNINITIALIZED_CONTROL && ctrl.type != NYQ_CTRL_STRING)
      {
         d = NyquistFormatting::GetCtrlValue(binding.valStr);
      }

      if (ctrl.type == NYQ_CTRL_FLOAT || ctrl.type == NYQ_CTRL_FLOAT_TEXT ||
          ctrl.type == NYQ_CTRL_TIME)
      {
         parms.Write(ctrl.var, d);
      }
      else if (ctrl.type == NYQ_CTRL_INT || ctrl.type == NYQ_CTRL_INT_TEXT)
      {
         parms.Write(ctrl.var, (int) d);
      }
      else if (ctrl.type == NYQ_CTRL_CHOICE)
      {
         // untranslated
         parms.WriteEnum(ctrl.var, (int) d,
                         ctrl.choices.data(), ctrl.choices.size());
      }
      else if (ctrl.type == NYQ_CTRL_STRING)
      {
         parms.Write(ctrl.var, binding.valStr);
      }
      else if (ctrl.type == NYQ_CTRL_FILE)
      {
         // Convert the given path string to platform-dependent equivalent
         NyquistFormatting::
         resolveFilePath(const_cast<NyqValue&>(binding).valStr);
         parms.Write(ctrl.var, binding.valStr);
      }
   }

   return true;
}

int NyquistControls::Load(Bindings &bindings,
   const CommandParameters & parms, bool bTestOnly)
{
   int badCount = 0;
   // First pass verifies values
   auto pBinding = bindings.begin();
   for (const auto &ctrl : mControls) {
      auto &binding = *pBinding++;
      bool good = false;

      // This GetCtrlValue code is preserved from former code,
      // but probably is pointless.  The value d isn't used later,
      // and GetCtrlValue does not appear to have important needed
      // side effects.
      if (!bTestOnly) {
         double d = binding.val;
         if (d == UNINITIALIZED_CONTROL && ctrl.type != NYQ_CTRL_STRING)
         {
            d = NyquistFormatting::GetCtrlValue(binding.valStr);
         }
      }

      if (ctrl.type == NYQ_CTRL_FLOAT || ctrl.type == NYQ_CTRL_FLOAT_TEXT ||
         ctrl.type == NYQ_CTRL_TIME)
      {
         double val;
         good = parms.Read(ctrl.var, &val) &&
            val >= ctrl.low &&
            val <= ctrl.high;
         if (good && !bTestOnly)
            binding.val = val;
      }
      else if (ctrl.type == NYQ_CTRL_INT || ctrl.type == NYQ_CTRL_INT_TEXT)
      {
         int val;
         good = parms.Read(ctrl.var, &val) &&
            val >= ctrl.low &&
            val <= ctrl.high;
         if (good && !bTestOnly)
            binding.val = (double)val;
      }
      else if (ctrl.type == NYQ_CTRL_CHOICE)
      {
         int val;
         // untranslated
         good = parms.ReadEnum(ctrl.var, &val,
            ctrl.choices.data(), ctrl.choices.size()) &&
            val != wxNOT_FOUND;
         if (good && !bTestOnly)
            binding.val = (double)val;
      }
      else if (ctrl.type == NYQ_CTRL_STRING || ctrl.type == NYQ_CTRL_FILE)
      {
         wxString val;
         good = parms.Read(ctrl.var, &val);
         if (good && !bTestOnly)
            binding.valStr = val;
      }
      else if (ctrl.type == NYQ_CTRL_TEXT)
      {
         // This "control" is just fixed text (nothing to save or restore),
         // Does not count for good/bad counting.
         good = true;
      }
      badCount += !good ? 1 : 0;
   }
   return badCount;
}

wxString NyquistControls::Expression(const Bindings &bindings) const
{
   wxString cmd;
   auto pBinding = bindings.cbegin();
   for (const auto &ctrl : mControls) {
      auto &binding = *pBinding++;
      if (ctrl.type == NYQ_CTRL_FLOAT || ctrl.type == NYQ_CTRL_FLOAT_TEXT ||
          ctrl.type == NYQ_CTRL_TIME) {
         // We use Internat::ToString() rather than "%f" here because we
         // always have to use the dot as decimal separator when giving
         // numbers to Nyquist, whereas using "%f" will use the user's
         // decimal separator which may be a comma in some countries.
         cmd += wxString::Format(wxT("(setf %s %s)\n"),
                                 ctrl.var,
                                 Internat::ToString(binding.val, 14));
      }
      else if (ctrl.type == NYQ_CTRL_INT ||
            ctrl.type == NYQ_CTRL_INT_TEXT ||
            ctrl.type == NYQ_CTRL_CHOICE) {
         cmd += wxString::Format(wxT("(setf %s %d)\n"),
                                 ctrl.var,
                                 (int)(binding.val));
      }
      else if (ctrl.type == NYQ_CTRL_STRING || ctrl.type == NYQ_CTRL_FILE) {
         cmd += wxT("(setf ");
         // restrict variable names to 7-bit ASCII:
         cmd += ctrl.var;
         cmd += wxT(" \"");
         cmd += NyquistFormatting::EscapeString(binding.valStr);
         // unrestricted value will become quoted UTF-8
         cmd += wxT("\")\n");
      }
   }
   return cmd;
}

wxString NyquistFormatting::EscapeString(const wxString & inStr)
{
   wxString str = inStr;

   str.Replace(wxT("\\"), wxT("\\\\"));
   str.Replace(wxT("\""), wxT("\\\""));

   return str;
}

double NyquistFormatting::GetCtrlValue(const wxString &s)
{
   /* For this to work correctly requires that the plug-in header is
    * parsed on each run so that the correct value for "half-srate" may
    * be determined.
    *
   auto project = FindProject();
   if (project && s.IsSameAs(wxT("half-srate"), false)) {
      auto rate =
         TrackList::Get( *project ).Selected< const WaveTrack >()
            .min( &WaveTrack::GetRate );
      return (rate / 2.0);
   }
   */

   return Internat::CompatibleToDouble(s);
}

#if 0
bool NyquistEffect::Tokenizer::Tokenize(
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

bool NyquistEffect::Parse(
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
         mEnablePreview = true;
         SetLinearEffectFlag(false);
      }
      else if (tokens[1] == wxT("linear")) {
         mEnablePreview = true;
         SetLinearEffectFlag(true);
      }
      else if (tokens[1] == wxT("selection")) {
         mEnablePreview = true;
         SetPreviewFullSelectionFlag(true);
      }
      else if (tokens[1] == wxT("disabled") || tokens[1] == wxT("false")) {
         mEnablePreview = false;
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

      if( ! make_iterator_range( mPresetNames ).contains( ctrl.var ) )
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

bool NyquistEffect::ParseProgram(wxInputStream & stream)
{
   if (!stream.IsOk())
   {
      mInitError = XO("Could not open file");
      return false;
   }

   wxTextInputStream pgm(stream, wxT(" \t"), wxConvAuto());

   mCmd = wxT("");
   mCmd.Alloc(10000);
   mIsSal = false;
   mControls.clear();
   mBindings.clear();
   mCategories.clear();
   mIsSpectral = false;
   mManPage = wxEmptyString; // If not wxEmptyString, must be a page in the Audacity manual.
   mHelpFile = wxEmptyString; // If not wxEmptyString, must be a valid HTML help file.
   mHelpFileExists = false;
   mDebug = false;
   mTrace = false;
   mDebugButton = true;    // Debug button enabled by default.
   mEnablePreview = true;  // Preview button enabled by default.

   // Bug 1934.
   // All Nyquist plug-ins should have a ';type' field, but if they don't we default to
   // being an Effect.
   mType = EffectTypeProcess;

   mFoundType = false;
   while (!stream.Eof() && stream.IsOk())
   {
      wxString line = pgm.ReadLine();
      if (line.length() > 1 &&
          // New in 2.3.0:  allow magic comment lines to start with $
          // The trick is that xgettext will not consider such lines comments
          // and will extract the strings they contain
          (line[0] == wxT(';') || line[0] == wxT('$')) )
      {
         Tokenizer tzer;
         unsigned nLines = 1;
         bool done;
         // Allow continuations within control lines.
         bool control =
            line[0] == wxT('$') || line.StartsWith( wxT(";control") );
         do
            done = Parse(tzer, line, !control || stream.Eof(), nLines == 1);
         while(!done &&
            (line = pgm.ReadLine(), ++nLines, true));

         // Don't pass these lines to the interpreter, so it doesn't get confused
         // by $, but pass blanks,
         // so that SAL effects compile with proper line numbers
         while (nLines --)
            mCmd += wxT('\n');
      }
      else
      {
         if(!mFoundType && line.length() > 0) {
            if (line[0] == wxT('(') ||
                (line[0] == wxT('#') && line.length() > 1 && line[1] == wxT('|')))
            {
               mIsSal = false;
               mFoundType = true;
            }
            else if (line.Upper().Find(wxT("RETURN")) != wxNOT_FOUND)
            {
               mIsSal = true;
               mFoundType = true;
            }
         }
         mCmd += line + wxT("\n");
      }
   }
   if (!mFoundType && !RecoverParseTypeFailed())
      return false;

   const auto helpStuff = CheckHelpPage();
   mHelpFileExists = helpStuff.first;
   mHelpPage       = helpStuff.second;

   return true;
}

bool NyquistEffect::RecoverParseTypeFailed()
{
   // Just throw it at Nyquist to see what happens
   return true;
}

void NyquistEffect::ParseFile()
{
   wxFileInputStream rawStream(mFileName.GetFullPath());
   wxBufferedInputStream stream(rawStream, 10000);

   ParseProgram(stream);
}

bool NyquistEffect::ParseCommand(const wxString & cmd)
{
   wxStringInputStream stream(cmd + wxT(" "));

   return ParseProgram(stream);
}

int NyquistEffect::StaticGetCallback(float *buffer, int channel,
                                     int64_t start, int64_t len, int64_t totlen,
                                     void *userdata)
{
   NyquistEffect *This = (NyquistEffect *)userdata;
   return This->GetCallback(buffer, channel, start, len, totlen);
}

int NyquistEffect::GetCallback(float *buffer, int ch,
                               int64_t start, int64_t len, int64_t WXUNUSED(totlen))
{
   if (mCurBuffer[ch]) {
      if ((mCurStart[ch] + start) < mCurBufferStart[ch] ||
          (mCurStart[ch] + start)+len >
          mCurBufferStart[ch]+mCurBufferLen[ch]) {
         mCurBuffer[ch].reset();
      }
   }

   if (!mCurBuffer[ch]) {
      mCurBufferStart[ch] = (mCurStart[ch] + start);
      mCurBufferLen[ch] = mCurTrack[ch]->GetBestBlockSize(mCurBufferStart[ch]);

      if (mCurBufferLen[ch] < (size_t) len) {
         mCurBufferLen[ch] = mCurTrack[ch]->GetIdealBlockSize();
      }

      mCurBufferLen[ch] =
         limitSampleBufferSize( mCurBufferLen[ch],
                                mCurStart[ch] + mCurLen - mCurBufferStart[ch] );

      // C++20
      // mCurBuffer[ch] = std::make_unique_for_overwrite(mCurBufferLen[ch]);
      mCurBuffer[ch] = Buffer{ safenew float[ mCurBufferLen[ch] ] };
      try {
         mCurTrack[ch]->GetFloats( mCurBuffer[ch].get(),
            mCurBufferStart[ch], mCurBufferLen[ch]);
      }
      catch ( ... ) {
         // Save the exception object for re-throw when out of the library
         mpException = std::current_exception();
         return -1;
      }
   }

   // We have guaranteed above that this is nonnegative and bounded by
   // mCurBufferLen[ch]:
   auto offset = ( mCurStart[ch] + start - mCurBufferStart[ch] ).as_size_t();
   const void *src = &mCurBuffer[ch][offset];
   std::memcpy(buffer, src, len * sizeof(float));

   if (ch == 0) {
      double progress = mScale *
         ( (start+len)/ mCurLen.as_double() );

      if (progress > mProgressIn) {
         mProgressIn = progress;
      }

      if (TotalProgress(mProgressIn+mProgressOut+mProgressTot)) {
         return -1;
      }
   }

   return 0;
}

int NyquistEffect::StaticPutCallback(float *buffer, int channel,
                                     int64_t start, int64_t len, int64_t totlen,
                                     void *userdata)
{
   NyquistEffect *This = (NyquistEffect *)userdata;
   return This->PutCallback(buffer, channel, start, len, totlen);
}

int NyquistEffect::PutCallback(float *buffer, int channel,
                               int64_t start, int64_t len, int64_t totlen)
{
   // Don't let C++ exceptions propagate through the Nyquist library
   return GuardedCall<int>( [&] {
      if (channel == 0) {
         double progress = mScale*((float)(start+len)/totlen);

         if (progress > mProgressOut) {
            mProgressOut = progress;
         }

         if (TotalProgress(mProgressIn+mProgressOut+mProgressTot)) {
            return -1;
         }
      }

      mOutputTrack[channel]->Append((samplePtr)buffer, floatSample, len);

      return 0; // success
   }, MakeSimpleGuard( -1 ) ); // translate all exceptions into failure
}

void NyquistEffect::StaticOutputCallback(int c, void *This)
{
   ((NyquistEffect *)This)->OutputCallback(c);
}

void NyquistEffect::OutputCallback(int c)
{
   // Always collect Nyquist error messages for normal plug-ins
   if (!mRedirectOutput) {
      mDebugOutputStr += (wxChar)c;
      return;
   }

   std::cout << (char)c;
}

void NyquistEffect::StaticOSCallback(void *This)
{
   ((NyquistEffect *)This)->OSCallback();
}

void NyquistEffect::OSCallback()
{
   if (mStop) {
      mStop = false;
      nyx_stop();
   }
   else if (mBreak) {
      mBreak = false;
      nyx_break();
   }
   else if (mCont) {
      mCont = false;
      nyx_continue();
   }

   // LLL:  STF figured out that yielding while the effect is being applied
   //       produces an EXTREME slowdown.  It appears that yielding is not
   //       really necessary on Linux and Windows.
   //
   //       However, on the Mac, the spinning cursor appears during longer
   //       Nyquist processing and that may cause the user to think Audacity
   //       has crashed or hung.  In addition, yielding or not on the Mac
   //       doesn't seem to make much of a difference in execution time.
   //
   //       So, yielding on the Mac only...
#if defined(__WXMAC__)
   wxYieldIfNeeded();
#endif
}

FilePaths NyquistEffect::GetNyquistSearchPath()
{
   const auto &audacityPathList = FileNames::AudacityPathList();
   FilePaths pathList;

   for (size_t i = 0; i < audacityPathList.size(); i++)
   {
      wxString prefix = audacityPathList[i] + wxFILE_SEP_PATH;
      FileNames::AddUniquePathToPathList(prefix + wxT("nyquist"), pathList);
      FileNames::AddUniquePathToPathList(prefix + wxT("plugins"), pathList);
      FileNames::AddUniquePathToPathList(prefix + wxT("plug-ins"), pathList);
   }
   pathList.push_back(FileNames::PlugInDir());

   return pathList;
}

bool NyquistEffect::TransferDataToWindow(const EffectSettings &)
{
   mUIParent->TransferDataToWindow();
   auto beginBinding = mBindings.cbegin(), pBinding = beginBinding;
   for (const auto &ctrl : mControls) {
      size_t i = pBinding - beginBinding;
      auto &binding = *pBinding++;

      if (ctrl.type == NYQ_CTRL_CHOICE)
      {
         const auto count = ctrl.choices.size();

         int val = (int)binding.val;
         if (val < 0 || val >= (int)count)
         {
            val = 0;
         }

         wxChoice *c = (wxChoice *) mUIParent->FindWindow(ID_Choice + i);
         c->SetSelection(val);
      }
      else if (ctrl.type == NYQ_CTRL_INT || ctrl.type == NYQ_CTRL_FLOAT)
      {
         // wxTextCtrls are handled by the validators
         double range = ctrl.high - ctrl.low;
         int val = (int)(0.5 + ctrl.ticks * (binding.val - ctrl.low) / range);
         wxSlider *s = (wxSlider *) mUIParent->FindWindow(ID_Slider + i);
         s->SetValue(val);
      }
      else if (ctrl.type == NYQ_CTRL_TIME)
      {
         NumericTextCtrl *n = (NumericTextCtrl *) mUIParent->FindWindow(ID_Time + i);
         n->SetValue(binding.val);
      }
   }

   EnablePreview(mEnablePreview);
   return true;
}

bool NyquistEffect::TransferDataFromWindow(EffectSettings &)
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
      return false;

   if (mControls.size() == 0)
   {
      return true;
   }

   using namespace NyquistFormatting;
   
   auto beginBinding = mBindings.begin(), pBinding = beginBinding;
   for (const auto &ctrl : mControls) {
      size_t i = pBinding - beginBinding;
      auto &binding = *pBinding++;

      if (ctrl.type == NYQ_CTRL_STRING || ctrl.type == NYQ_CTRL_TEXT)
      {
         continue;
      }

      if (binding.val == UNINITIALIZED_CONTROL)
      {
         binding.val = GetCtrlValue(binding.valStr);
      }

      if (ctrl.type == NYQ_CTRL_CHOICE)
      {
         continue;
      }

      if (ctrl.type == NYQ_CTRL_FILE)
      {
         resolveFilePath(binding.valStr);

         wxString path;
         if (binding.valStr.StartsWith("\"", &path))
         {
            // Validate if a list of quoted paths.
            if (path.EndsWith("\"", &path))
            {
               path.Replace("\"\"", "\"");
               wxStringTokenizer tokenizer(path, "\"");
               while (tokenizer.HasMoreTokens())
               {
                  wxString token = tokenizer.GetNextToken();
                  if(!validatePath(token))
                  {
                     const auto message =
                        XO("\"%s\" is not a valid file path.").Format( token );
                     Effect::MessageBox(
                        message,
                        wxOK | wxICON_EXCLAMATION | wxCENTRE,
                        XO("Error") );
                     return false;
                  }
               }
               continue;
            }
            else
            {
               const auto message =
                  /* i18n-hint: Warning that there is one quotation mark rather than a pair.*/
                  XO("Mismatched quotes in\n%s").Format( binding.valStr );
               Effect::MessageBox(
                  message,
                  wxOK | wxICON_EXCLAMATION | wxCENTRE,
                  XO("Error") );
               return false;
            }
         }
         // Validate a single path.
         else if (validatePath(binding.valStr))
         {
            continue;
         }

         // Validation failed
         const auto message =
            XO("\"%s\" is not a valid file path.").Format( binding.valStr );
         Effect::MessageBox(
            message,
            wxOK | wxICON_EXCLAMATION | wxCENTRE,
            XO("Error") );
         return false;
      }

      if (ctrl.type == NYQ_CTRL_TIME)
      {
         NumericTextCtrl *n = (NumericTextCtrl *) mUIParent->FindWindow(ID_Time + i);
         binding.val = n->GetValue();
      }

      binding.val = std::clamp(binding.val, ctrl.low, ctrl.high);
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

std::unique_ptr<EffectUIValidator> NyquistEffect::PopulateOrExchange(
   ShuttleGui & S, EffectInstance &, EffectSettingsAccess &)
{
   wxScrolledWindow *scroller = S.Style(wxVSCROLL | wxTAB_TRAVERSAL)
      .StartScroller(2);
   {
      S.StartMultiColumn(4);
      {
         auto beginBinding = mBindings.begin(), pBinding = beginBinding;
         for (const auto &ctrl : mControls) {
            size_t i = pBinding - beginBinding;
            auto &binding = *pBinding++;

            if (ctrl.type == NYQ_CTRL_TEXT)
            {
               S.EndMultiColumn();
               S.StartHorizontalLay(wxALIGN_LEFT, 0);
               {
                  S.AddSpace(0, 10);
                  S.AddFixedText( Verbatim( ctrl.label ), false );
               }
               S.EndHorizontalLay();
               S.StartMultiColumn(4);
            }
            else
            {
               auto prompt = XXO("%s:").Format( ctrl.name );
               S.AddPrompt( prompt );

               if (ctrl.type == NYQ_CTRL_STRING)
               {
                  S.AddSpace(10, 10);

                  auto item = S.Id(ID_Text + i)
                     .Validator<wxGenericValidator>(&binding.valStr)
                     .Name( prompt )
                     .AddTextBox( {}, wxT(""), 50);
               }
               else if (ctrl.type == NYQ_CTRL_CHOICE)
               {
                  S.AddSpace(10, 10);

                  S.Id(ID_Choice + i).AddChoice( {},
                     Msgids( ctrl.choices.data(), ctrl.choices.size() ) );
               }
               else if (ctrl.type == NYQ_CTRL_TIME)
               {
                  S.AddSpace(10, 10);

                  const auto options = NumericTextCtrl::Options{}
                                          .AutoPos(true)
                                          .MenuEnabled(true)
                                          .ReadOnly(false);

                  NumericTextCtrl *time = safenew
                     NumericTextCtrl(S.GetParent(), (ID_Time + i),
                                     NumericConverter::TIME,
                                     GetSelectionFormat(),
                                     binding.val,
                                     mProjectRate,
                                     options);
                  S
                     .Name( prompt )
                     .Position(wxALIGN_LEFT | wxALL)
                     .AddWindow(time);
               }
               else if (ctrl.type == NYQ_CTRL_FILE)
               {
                  S.AddSpace(10, 10);

                  // Get default file extension if specified in wildcards
                  FileExtension defaultExtension;
                  if (!ctrl.fileTypes.empty()) {
                     const auto &type = ctrl.fileTypes[0];
                     if ( !type.extensions.empty() )
                        defaultExtension = type.extensions[0];
                  }
                  NyquistFormatting::
                  resolveFilePath(binding.valStr, defaultExtension);

                  wxTextCtrl *item = S.Id(ID_Text+i)
                     .Name( prompt )
                     .AddTextBox( {}, wxT(""), 40);
                  item->SetValidator(wxGenericValidator(&binding.valStr));

                  S.Id(ID_FILE + i).AddButton(
                     Verbatim( ctrl.label.empty ()
                        // We'd expect wxFileSelectorPromptStr to already be
                        // translated, but apparently not.
                        ? wxGetTranslation( wxFileSelectorPromptStr )
                        : ctrl.label ),
                     wxALIGN_LEFT);
               }
               else
               {
                  // Integer or Real
                  if (ctrl.type == NYQ_CTRL_INT_TEXT || ctrl.type == NYQ_CTRL_FLOAT_TEXT)
                  {
                     S.AddSpace(10, 10);
                  }

                  S.Id(ID_Text+i);
                  if (ctrl.type == NYQ_CTRL_FLOAT || ctrl.type == NYQ_CTRL_FLOAT_TEXT)
                  {
                     double range = ctrl.high - ctrl.low;
                     S.Validator<FloatingPointValidator<double>>(
                        // > 12 decimal places can cause rounding errors in display.
                        12, &binding.val,
                        // Set number of decimal places
                        (range < 10
                           ? NumValidatorStyle::THREE_TRAILING_ZEROES
                           : range < 100
                              ? NumValidatorStyle::TWO_TRAILING_ZEROES
                              : NumValidatorStyle::ONE_TRAILING_ZERO),
                        ctrl.low, ctrl.high
                     );
                  }
                  else
                  {
                     S.Validator<IntegerValidator<double>>(
                        &binding.val, NumValidatorStyle::DEFAULT,
                        (int) ctrl.low, (int) ctrl.high);
                  }
                  wxTextCtrl *item = S
                     .Name( prompt )
                     .AddTextBox( {}, wxT(""),
                        (ctrl.type == NYQ_CTRL_INT_TEXT ||
                         ctrl.type == NYQ_CTRL_FLOAT_TEXT) ? 25 : 12);

                  if (ctrl.type == NYQ_CTRL_INT || ctrl.type == NYQ_CTRL_FLOAT)
                  {
                     S.Id(ID_Slider + i)
                        .Style(wxSL_HORIZONTAL)
                        .MinSize( { 150, -1 } )
                        .AddSlider( {}, 0, ctrl.ticks, 0);
                  }
               }

               if (ctrl.type != NYQ_CTRL_FILE)
               {
                  if (ctrl.type == NYQ_CTRL_CHOICE || ctrl.label.empty())
                  {
                     S.AddSpace(10, 10);
                  }
                  else
                  {
                     S.AddUnits( Verbatim( ctrl.label ) );
                  }
               }
            }
         }
      }
      S.EndMultiColumn();
   }
   S.EndScroller();

   scroller->SetScrollRate(0, 20);

   // This fools NVDA into not saying "Panel" when the dialog gets focus
   scroller->SetName(wxT("\a"));
   scroller->SetLabel(wxT("\a"));
   return nullptr;
}

// NyquistEffect implementation

bool NyquistEffect::IsOk()
{
   return mOK;
}

void NyquistEffect::OnSlider(wxCommandEvent & evt)
{
   int i = evt.GetId() - ID_Slider;
   const auto & ctrl = mControls[i];
   auto &binding = mBindings[i];

   int val = evt.GetInt();
   double range = ctrl.high - ctrl.low;
   double newVal = (val / (double)ctrl.ticks) * range + ctrl.low;

   // Determine precision for displayed number
   int precision = range < 1.0 ? 3 :
                   range < 10.0 ? 2 :
                   range < 100.0 ? 1 :
                   0;

   // If the value is at least one tick different from the current value
   // change it (this prevents changes from manually entered values unless
   // the slider actually moved)
   if (fabs(newVal - binding.val) >= (1 / (double)ctrl.ticks) * range &&
       fabs(newVal - binding.val) >= pow(0.1, precision) / 2)
   {
      // First round to the appropriate precision
      newVal *= pow(10.0, precision);
      newVal = floor(newVal + 0.5);
      newVal /= pow(10.0, precision);

      binding.val = newVal;

      mUIParent->FindWindow(ID_Text + i)->GetValidator()->TransferToWindow();
   }
}

void NyquistEffect::OnChoice(wxCommandEvent & evt)
{
   mBindings[evt.GetId() - ID_Choice].val = (double) evt.GetInt();
}

void NyquistEffect::OnTime(wxCommandEvent& evt)
{
   int i = evt.GetId() - ID_Time;
   static double value = 0.0;
   const auto & ctrl = mControls[i];

   NumericTextCtrl *n = (NumericTextCtrl *) mUIParent->FindWindow(ID_Time + i);
   double val = n->GetValue();

   // Observed that two events transmitted on each control change (Linux)
   // so skip if value has not changed.
   if (val != value) {
      if (val < ctrl.low || val > ctrl.high) {
         const auto message = XO("Value range:\n%s to %s")
            .Format( ToTimeFormat(ctrl.low), ToTimeFormat(ctrl.high) );
         Effect::MessageBox(
            message,
            wxOK | wxCENTRE,
            XO("Value Error") );
      }

      if (val < ctrl.low)
         val = ctrl.low;
      else if (val > ctrl.high)
         val = ctrl.high;

      n->SetValue(val);
      value = val;
   }
}
#endif

void NyquistFormatting::resolveFilePath(
   wxString& path, FileExtension extension /* empty string */)
{
#if defined(__WXMSW__)
   path.Replace("/", wxFileName::GetPathSeparator());
#endif

   path.Trim(true).Trim(false);

   typedef std::unordered_map<wxString, FilePath> map;
   map pathKeys = {
      {"*home*", wxGetHomeDir()},
      {"~", wxGetHomeDir()},
      {"*default*", FileNames::DefaultToDocumentsFolder("").GetPath()},
      {"*export*", FileNames::FindDefaultPath(FileNames::Operation::Export)},
      {"*save*", FileNames::FindDefaultPath(FileNames::Operation::Save)},
      {"*config*", FileNames::DataDir()}
   };

   int characters = path.Find(wxFileName::GetPathSeparator());
   if(characters == wxNOT_FOUND) // Just a path or just a file name
   {
      if (path.empty())
         path = "*default*";

      if (pathKeys.find(path) != pathKeys.end())
      {
         // Keyword found, so assume this is the intended directory.
         path = pathKeys[path] + wxFileName::GetPathSeparator();
      }
      else  // Just a file name
      {
         path = pathKeys["*default*"] + wxFileName::GetPathSeparator() + path;
      }
   }
   else  // path + file name
   {
      wxString firstDir = path.Left(characters);
      wxString rest = path.Mid(characters);

      if (pathKeys.find(firstDir) != pathKeys.end())
      {
         path = pathKeys[firstDir] + rest;
      }
   }

   wxFileName fname = path;

   // If the directory is invalid, better to leave it as is (invalid) so that
   // the user sees the error rather than an unexpected file path.
   if (fname.wxFileName::IsOk() && fname.GetFullName().empty())
   {
      path = fname.GetPathWithSep() + _("untitled");
      if (!extension.empty())
         path = path + '.' + extension;
   }
}
