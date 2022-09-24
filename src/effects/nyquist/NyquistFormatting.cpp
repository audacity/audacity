/**********************************************************************

  Audacity: A Digital Audio Editor

  @file NyquistFormatting.cpp

  Dominic Mazzoni

  Paul Licameli split from NyquistControls.cpp

**********************************************************************/
#include "NyquistFormatting.h"

#include "Internat.h"
#include "wxFileNameWrapper.h"
#include "MemoryX.h"
#include <wx/string.h>
#include <wx/utils.h>
#include <unordered_map>


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

namespace {
wxString symbolName(wxString name)
{
   // process name so it can be given to the Lisp reader
   // TODO:  might escape characters in name, so it could include spaces
   return name;
}
}

NyquistFormatting::Symbol::Symbol(const wxString &name)
   : mName{ symbolName(name) }
{
}

namespace NyquistFormatting{ namespace {
struct ValueFormatter {
   wxString operator()(std::monostate) const {
      // Special right-hand-side for XLisp assignment unbinds a symbol
      return "'*unbound*";
   }
   wxString operator()(nullptr_t) const {
      return "nil";
   }
   wxString operator()(bool b) const {
      return b ? "t" : "nil";
   }
   wxString operator()(char c) const {
      // Syntax for character constant like 'x' in C, is #\x in Lisp
      return wxString::Format("#\\%c", c);
   }
   // Be careful about the printf formatting of FIXTYPE
   wxString operator()(FIXTYPE value) const {
      static_assert(std::is_same_v<FIXTYPE, long>,
         "ValueFormatter needs another overload of FormatIntegral");
      return FormatIntegral(value);
   }
   wxString FormatIntegral(long value) const {
      return wxString::Format("%ld", value);
   }
   // FLOTYPE can just promote to double if it isn't already
   wxString operator()(double value) const {
      // We use Internat::ToString() rather than "%f" here because we
      // always have to use the dot as decimal separator when giving
      // numbers to Nyquist, whereas using "%f" will use the user's
      // decimal separator which may be a comma in some countries.
      // Note: always format a double with positive precision, so the Lisp
      // reader will construct a floating point number, and not a fixed.
      return Internat::ToString(value, 14);
   }
   wxString operator()(const char *value) const {
      // unrestricted value will become quoted UTF-8
      return '"' + EscapeString(value) + '"';
   }
   wxString operator()(Multiple multiple, const char *function) const {
      auto result = wxString{ '(' } + function + ' ';
      for (auto &&value : multiple)
         result += Visit(value) + ' ';
      // overwrite last space
      *result.rbegin() = ')';
      return result;
   }
   wxString operator()(List list) const {
      if (begin(list) == end(list))
         return "nil";
      return (*this)(list, "list");
   }
   wxString operator()(Vector vector) const {
      // May format as the empty vector value, "(vector)"
      return (*this)(vector, "vector");
   }
   wxString operator()(const Eval &eval) const {
      return eval.form;
   }

   wxString Visit (const Value &value) const {
      const ValueBase &base = value;
      return ::Visit(*this, base);
   }
};
} }

wxString NyquistFormatting::Value::Format() const
{
   return ValueFormatter{}.Visit(*this);
}

NyquistFormatting::Assignments::Assignments(Arguments list)
{
   for (auto &argument : list)
      Append(argument);
}

void NyquistFormatting::Assignments::Append(Assignment assignment)
{
   const auto valueString = assignment.value.Format();
   if (assignment.property)
      mCommand += wxString::Format(wxT("(putprop '%s %s '%s)\n"),
         assignment.symbol->mName, valueString, assignment.property->mName);
   else
      mCommand += wxString::Format(wxT("(setf %s %s)\n"),
         assignment.symbol->mName, valueString);
}
