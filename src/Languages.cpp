/**********************************************************************

  Audacity: A Digital Audio Editor

  Languages.cpp

  Dominic Mazzoni


*******************************************************************//*!

\file Languages.cpp
\brief Determine installed languages.

  Figure out what translations are installed and return a list
  of language codes (like "es", "fr", or "pt-br") and corresponding
  language names (like "Español", "Français", and "Português").
  We use our own list of translations of language names (i.e.
  "Français" instead of "French") but we fallback on the language
  name in wxWidgets if we don't have it listed.

  This code is designed to work well with all of the current
  languages, but adapt to any language that wxWidgets supports.
  Other languages will only be supported if they're added to
  the database using wxLocale::AddLanguage.

  But for the most part, this means that somebody could add a NEW
  translation and have it work immediately.

*//*******************************************************************/


#include "Audacity.h"

#include <wx/defs.h>
#include <wx/hashmap.h>
#include <wx/intl.h>
#include <wx/textfile.h>

#include "Languages.h"
#include "FileNames.h"

#include "AudacityApp.h"

WX_DECLARE_STRING_HASH_MAP(wxString, LangHash);

static bool TranslationExists(wxArrayString &audacityPathList, wxString code)
{
   wxArrayString results;
   wxGetApp().FindFilesInPathList(wxString::Format(wxT("%s/audacity.mo"),
                                                   code),
                                  audacityPathList,
                                  results);
#if defined(__WXMAC__)
   wxGetApp().FindFilesInPathList(wxString::Format(wxT("%s.lproj/audacity.mo"),
                                                   code),
                                  audacityPathList,
                                  results);
#endif

   wxGetApp().FindFilesInPathList(wxString::Format(wxT("%s/LC_MESSAGES/audacity.mo"),
                                                   code),
                                  audacityPathList,
                                  results);

   return (results.GetCount() > 0);
}

#ifdef __WXMAC__
#include <CoreFoundation/CFLocale.h>
#include "wx/osx/core/cfstring.h"
#endif

wxString GetSystemLanguageCode()
{
   wxArrayString langCodes;
   wxArrayString langNames;

   GetLanguages(langCodes, langNames);

   int sysLang = wxLocale::GetSystemLanguage();

   const wxLanguageInfo *info;

#ifdef __WXMAC__
   // PRL: Bug 1227, system language on Mac may not be right because wxW3 is
   // dependent on country code too in wxLocale::GetSystemLanguage().

   if (sysLang == wxLANGUAGE_UNKNOWN)
   {
      // wxW3 did a too-specific lookup of language and country, when
      // there is nothing for that combination; try it by language alone.

      // The following lines are cribbed from that function.
      wxCFRef<CFLocaleRef> userLocaleRef(CFLocaleCopyCurrent());
      wxCFStringRef str(wxCFRetain((CFStringRef)CFLocaleGetValue(userLocaleRef, kCFLocaleLanguageCode)));
      auto lang = str.AsString();

      // Now avoid wxLocale::GetLanguageInfo(), instead calling:
      info = wxLocale::FindLanguageInfo(lang);
   }
   else
#endif
   {
      info = wxLocale::GetLanguageInfo(sysLang);
   }

   if (info) {
      wxString fullCode = info->CanonicalName;
      if (fullCode.Length() < 2)
         return wxT("en");

      wxString code = fullCode.Left(2);
      unsigned int i;

      for(i=0; i<langCodes.GetCount(); i++) {
         if (langCodes[i] == fullCode)
            return fullCode;

         if (langCodes[i] == code)
            return code;
      }
   }

   return wxT("en");
}

void GetLanguages(wxArrayString &langCodes, wxArrayString &langNames)
{
   wxArrayString tempNames;
   wxArrayString tempCodes;
   LangHash localLanguageName;
   LangHash reverseHash;
   LangHash tempHash;

#ifdef EXPERIMENTAL_LANGUAGES_DOT_TEXT
   {
      // The list of locales and associated self-names of languages
      // is stored in an external resource file which is easier
      // to edit as Unicode than C++ source code.
      auto dir = FileNames::ResourcesDir();
      wxTextFile file{dir + wxFILE_SEP_PATH + "LanguageNames.txt"};
      file.Open();
      for ( auto str = file.GetFirstLine(); !file.Eof();
            str = file.GetNextLine() )
      {
         // Allow commenting-out of languages no longer supported
         if (str[0] == '#')
            continue;

         auto code = str.BeforeFirst(' ');
         auto name = str.AfterFirst(' ');
         localLanguageName[code] = name;
      }
   }
#else
//#include "../locale/LanguageNames.h"
localLanguageName["af"] = L"Afrikaans";
localLanguageName["ar"] = L"العربية";
localLanguageName["be"] = L"Беларуская";
localLanguageName["bg"] = L"Български";
localLanguageName["bn"] = L"বাংলা";
localLanguageName["bs"] = L"Bosanski";
localLanguageName["ca"] = L"Català";
localLanguageName["ca_ES@valencia"] = L"Valencià";
localLanguageName["cs"] = L"Čeština";
localLanguageName["cy"] = L"Cymraeg";
localLanguageName["da"] = L"Dansk";
localLanguageName["de"] = L"Deutsch";
localLanguageName["el"] = L"Ελληνικά";
localLanguageName["en"] = L"English";
localLanguageName["es"] = L"Español";
localLanguageName["eu"] = L"Euskara";
localLanguageName["eu_ES"] = L"Euskara (Espainiako)";
localLanguageName["fa"] = L"فارسی";
localLanguageName["fi"] = L"Suomi";
localLanguageName["fr"] = L"Français";
localLanguageName["ga"] = L"Gaeilge";
localLanguageName["gl"] = L"Galego";
localLanguageName["he"] = L"עברית";
localLanguageName["hi"] = L"हिन्दी";
localLanguageName["hr"] = L"Hrvatski";
localLanguageName["hu"] = L"Magyar";
localLanguageName["hy"] = L"Հայերեն";
localLanguageName["id"] = L"Bahasa Indonesia";
localLanguageName["it"] = L"Italiano";
localLanguageName["ja"] = L"日本語";
localLanguageName["ka"] = L"ქართული";
localLanguageName["km"] = L"ខេមរភាសា";
localLanguageName["ko"] = L"한국어";
localLanguageName["lt"] = L"Lietuvių";
localLanguageName["mk"] = L"Македонски";
localLanguageName["my"] = L"မြန်မာစာ";
localLanguageName["nb"] = L"Norsk";
localLanguageName["nl"] = L"Nederlands";
localLanguageName["oc"] = L"Occitan";
localLanguageName["pl"] = L"Polski";
localLanguageName["pt"] = L"Português";
localLanguageName["pt_BR"] = L"Português (Brasil)";
localLanguageName["ro"] = L"Română";
localLanguageName["ru"] = L"Русский";
localLanguageName["sk"] = L"Slovenčina";
localLanguageName["sl"] = L"Slovenščina";
localLanguageName["sr_RS"] = L"Српски";
localLanguageName["sr_RS@latin"] = L"Srpski";
localLanguageName["sv"] = L"Svenska";
localLanguageName["ta"] = L"தமிழ்";
localLanguageName["tg"] = L"Тоҷикӣ";
localLanguageName["tr"] = L"Türkçe";
localLanguageName["uk"] = L"Українська";
localLanguageName["vi"] = L"Tiếng Việt";
localLanguageName["zh_CN"] = L"中文";
localLanguageName["zh_TW"] = L"中文";


#endif

   wxArrayString audacityPathList = wxGetApp().audacityPathList;

#if defined(__WXGTK__)
   wxGetApp().AddUniquePathToPathList(wxString::Format(wxT("%s/share/locale"),
                                                       wxT(INSTALL_PREFIX)),
                                      audacityPathList);
#endif

   // For each language in our list we look for a corresponding entry in
   // wxLocale.  
   for (LangHash::iterator i = localLanguageName.begin();
        i != localLanguageName.end();
        i++)
   {
      const wxLanguageInfo *info = wxLocale::FindLanguageInfo(i->first);

      if (!info) {
         wxASSERT(info != NULL);
         continue;
      }

      wxString fullCode = info->CanonicalName;
      wxString code = fullCode.Left(2);
      wxString name = info->Description;

      // Logic: Languages codes are sometimes hierarchical, with a
      // general language code and then a subheading.  For example,
      // zh_TW for Traditional Chinese and zh_CN for Simplified
      // Chinese - but just zh for Chinese in general.  First we look
      // for the full code, like zh_TW.  If that doesn't exist, we
      // look for a code corresponding to the first two letters.
      // Note that if the language for a fullCode exists but we only
      // have a name for the short code, we will use the short code's
      // name but associate it with the full code.  This allows someone
      // to drop in a NEW language and still get reasonable behavior.

      if (fullCode.Length() < 2)
         continue;

      if (localLanguageName[code] != wxT("")) {
         name = localLanguageName[code];
      }
      if (localLanguageName[fullCode] != wxT("")) {
         name = localLanguageName[fullCode];
      }

      if (TranslationExists(audacityPathList, fullCode)) {
         code = fullCode;
      }

      if (tempHash[code] != wxT(""))
         continue;

      if (TranslationExists(audacityPathList, code) || code==wxT("en")) {
         tempCodes.Add(code);
         tempNames.Add(name);
         tempHash[code] = name;

/*         wxLogDebug(wxT("code=%s name=%s fullCode=%s name=%s -> %s"),
                      code, localLanguageName[code],
                      fullCode, localLanguageName[fullCode],
                      name);*/
      }
   }

   // JKC: Adding language for simplified audacity.
   {
      wxString code;
      wxString name;
      code = wxT("en-simple");
      name = wxT("Simplified");
      if (TranslationExists(audacityPathList, code) ) {
         tempCodes.Add(code);
         tempNames.Add(name);
         tempHash[code] = name;
      }
   }


   // Sort
   unsigned int j;
   for(j=0; j<tempNames.GetCount(); j++){
      reverseHash[tempNames[j]] = tempCodes[j];
   }

   tempNames.Sort();

   // Add system language
   langNames.Add(wxT("System"));
   langCodes.Add(wxT(""));

   for(j=0; j<tempNames.GetCount(); j++) {
      langNames.Add(tempNames[j]);
      langCodes.Add(reverseHash[tempNames[j]]);
   }
}
