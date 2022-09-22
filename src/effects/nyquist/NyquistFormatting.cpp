/**********************************************************************

  Audacity: A Digital Audio Editor

  @file NyquistFormatting.cpp

  Dominic Mazzoni

  Paul Licameli split from NyquistControls.cpp

**********************************************************************/
#include "NyquistFormatting.h"

#include "Internat.h"
#include "wxFileNameWrapper.h"
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
