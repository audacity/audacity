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

#include "Languages.h"

#include "AudacityApp.h"

WX_DECLARE_STRING_HASH_MAP(wxString, LangHash);

static bool TranslationExists(wxArrayString &audacityPathList, wxString code)
{
   wxArrayString results;
   wxGetApp().FindFilesInPathList(wxString::Format(wxT("%s/audacity.mo"),
                                                   code.c_str()),
                                  audacityPathList,
                                  results);
#if defined(__WXMAC__)
   wxGetApp().FindFilesInPathList(wxString::Format(wxT("%s.lproj/audacity.mo"),
                                                   code.c_str()),
                                  audacityPathList,
                                  results);
#endif

   wxGetApp().FindFilesInPathList(wxString::Format(wxT("%s/LC_MESSAGES/audacity.mo"),
                                                   code.c_str()),
                                  audacityPathList,
                                  results);

   return (results.GetCount() > 0);
}

wxString GetSystemLanguageCode()
{
   wxArrayString langCodes;
   wxArrayString langNames;

   GetLanguages(langCodes, langNames);
   int sysLang = wxLocale::GetSystemLanguage();
   const wxLanguageInfo *info = wxLocale::GetLanguageInfo(sysLang);

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

   // MM: Use only ASCII characters here to avoid problems with
   //     charset conversion on Linux platforms
   localLanguageName[wxT("af")] = wxT("Afrikaans");
   localLanguageName[wxT("ar")] = wxT("Arabic");
   localLanguageName[wxT("be")] = wxT("Belarusian");
   localLanguageName[wxT("bg")] = wxT("Balgarski");
   localLanguageName[wxT("bn")] = wxT("Bengali");
   localLanguageName[wxT("bs")] = wxT("Bosnian");
   localLanguageName[wxT("ca")] = wxT("Catalan");
   localLanguageName[wxT("ca_ES@valencia")] = wxT("Valencian (southern Catalan)");
   localLanguageName[wxT("cs")] = wxT("Czech");
   localLanguageName[wxT("cy")] = wxT("Welsh");
   localLanguageName[wxT("da")] = wxT("Dansk");
   localLanguageName[wxT("de")] = wxT("Deutsch");
   localLanguageName[wxT("el")] = wxT("Ellinika");
   localLanguageName[wxT("en")] = wxT("English");
   localLanguageName[wxT("es")] = wxT("Espanol");
   localLanguageName[wxT("eu")] = wxT("Euskara");
   localLanguageName[wxT("fa")] = wxT("Farsi");
   localLanguageName[wxT("fi")] = wxT("Suomi");
   localLanguageName[wxT("fr")] = wxT("Francais");
   localLanguageName[wxT("ga")] = wxT("Gaeilge");
   localLanguageName[wxT("gl")] = wxT("Galician");
   localLanguageName[wxT("he")] = wxT("Hebrew");
   localLanguageName[wxT("hi")] = wxT("Hindi");
   localLanguageName[wxT("hr")] = wxT("Croatian");
   localLanguageName[wxT("hu")] = wxT("Magyar");
   localLanguageName[wxT("hy")] = wxT("Armenian");
   localLanguageName[wxT("id")] = wxT("Bahasa Indonesia"); // aka Indonesian
   localLanguageName[wxT("it")] = wxT("Italiano");
   localLanguageName[wxT("ja")] = wxT("Nihongo");
   localLanguageName[wxT("ka")] = wxT("Georgian");
   localLanguageName[wxT("km")] = wxT("Khmer");
   localLanguageName[wxT("ko")] = wxT("Korean");
   localLanguageName[wxT("lt")] = wxT("Lietuviu");
   localLanguageName[wxT("mk")] = wxT("Makedonski");
   localLanguageName[wxT("my")] = wxT("Burmese");
   localLanguageName[wxT("nb")] = wxT("Norsk");
   localLanguageName[wxT("nl")] = wxT("Nederlands");
   localLanguageName[wxT("oc")] = wxT("Occitan");
   localLanguageName[wxT("pl")] = wxT("Polski");
   localLanguageName[wxT("pt")] = wxT("Portugues");
   localLanguageName[wxT("pt_BR")] = wxT("Portugues (Brasil)");
   localLanguageName[wxT("ro")] = wxT("Romanian");
   localLanguageName[wxT("ru")] = wxT("Russky");
   localLanguageName[wxT("sk")] = wxT("Slovak");
   localLanguageName[wxT("sl")] = wxT("Slovenscina");
   localLanguageName[wxT("sr_RS")] = wxT("Serbian (Cyrillic)");
   localLanguageName[wxT("sr_RS@latin")] = wxT("Serbian (Latin)");
   localLanguageName[wxT("sv")] = wxT("Svenska");
   localLanguageName[wxT("tg")] = wxT("Tajik");
   localLanguageName[wxT("ta")] = wxT("Tamil");
   localLanguageName[wxT("tr")] = wxT("Turkce");
   localLanguageName[wxT("uk")] = wxT("Ukrainska");
   localLanguageName[wxT("vi")] = wxT("Vietnamese");
   // If we look up zh in wxLocale we get zh_TW hence we MUST look
   // for zh_CN.
   localLanguageName[wxT("zh_CN")] = wxT("Chinese (Simplified)");
   localLanguageName[wxT("zh_TW")] = wxT("Chinese (Traditional)");

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
                      code.c_str(), localLanguageName[code].c_str(),
                      fullCode.c_str(), localLanguageName[fullCode].c_str(),
                      name.c_str());*/
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
