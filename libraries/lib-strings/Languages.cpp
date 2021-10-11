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



#include "Languages.h"
#include <memory>
#include "wxArrayStringEx.h"

#include "Internat.h"
#include "wxArrayStringEx.h"

#include <wx/defs.h>
#include <wx/dir.h>
#include <wx/filename.h>
#include <wx/intl.h>
#include <wx/stdpaths.h>
#include <wx/textfile.h>
#include <wx/utils.h> // for wxSetEnv

#include <clocale>
#include <unordered_map>

using LangHash = std::unordered_map<wxString, TranslatableString>;
using ReverseLangHash = std::unordered_map<TranslatableString, wxString>;

static void FindFilesInPathList(const wxString & pattern,
   const FilePaths & pathList, FilePaths & results)
{
   wxFileName ff;
   for (const auto &path : pathList) {
      ff = path + wxFILE_SEP_PATH + pattern;
      wxDir::GetAllFiles(ff.GetPath(), &results, ff.GetFullName(), wxDIR_FILES);
   }
}

static bool TranslationExists(const FilePaths &pathList, wxString code)
{
   FilePaths results;
   FindFilesInPathList(code + L"/audacity.mo", pathList, results);
#if defined(__WXMAC__)
   FindFilesInPathList(code + L".lproj/audacity.mo", pathList, results);
#endif
   FindFilesInPathList(code + L"/LC_MESSAGES/audacity.mo", pathList, results);
   return (results.size() > 0);
}

#ifdef __WXMAC__
#include <CoreFoundation/CFLocale.h>
#include <wx/osx/core/cfstring.h>
#endif

namespace Languages {

wxString GetSystemLanguageCode(const FilePaths &pathList)
{
   wxArrayString langCodes;
   TranslatableStrings langNames;

   GetLanguages(pathList, langCodes, langNames);

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
      if (fullCode.length() < 2)
         return wxT("en");

      wxString code = fullCode.Left(2);
      unsigned int i;

      for(i=0; i<langCodes.size(); i++) {
         if (langCodes[i] == fullCode)
            return fullCode;

         if (langCodes[i] == code)
            return code;
      }
   }

   return wxT("en");
}

void GetLanguages( FilePaths pathList,
   wxArrayString &langCodes, TranslatableStrings &langNames)
{
   static const char *const utf8Names[] = {
"af Afrikaans",
"ar \330\247\331\204\330\271\330\261\330\250\331\212\330\251",
"be \320\221\320\265\320\273\320\260\321\200\321\203\321\201\320\272\320\260\321\217",
"bg \320\221\321\212\320\273\320\263\320\260\321\200\321\201\320\272\320\270",
"bn \340\246\254\340\246\276\340\246\202\340\246\262\340\246\276",
"bs Bosanski",
"ca Catal\303\240",
"ca_ES@valencia Valenci\303\240",
"co Corsu",
"cs \304\214e\305\241tina",
"cy Cymraeg",
"da Dansk",
"de Deutsch",
"el \316\225\316\273\316\273\316\267\316\275\316\271\316\272\316\254",
"en English",
"es Espa\303\261ol",
"eu Euskara",
"eu_ES Euskara (Espainiako)",
"fa \331\201\330\247\330\261\330\263\333\214",
"fi Suomi",
"fr Fran\303\247ais",
"ga Gaeilge",
"gl Galego",
"he \327\242\327\221\327\250\327\231\327\252",
"hi \340\244\271\340\244\277\340\244\250\340\245\215\340\244\246\340\245\200",
"hr Hrvatski",
"hu Magyar",
"hy \325\200\325\241\325\265\325\245\326\200\325\245\325\266",
"id Bahasa Indonesia",
"it Italiano",
"ja \346\227\245\346\234\254\350\252\236",
"ka \341\203\245\341\203\220\341\203\240\341\203\227\341\203\243\341\203\232\341\203\230",
"km \341\236\201\341\237\201\341\236\230\341\236\232\341\236\227\341\236\266\341\236\237\341\236\266",
"ko \355\225\234\352\265\255\354\226\264",
"lt Lietuvi\305\263",
"mk \320\234\320\260\320\272\320\265\320\264\320\276\320\275\321\201\320\272\320\270",
"mr \340\244\256\340\244\260\340\244\276\340\244\240\340\245\200",
"my \341\200\231\341\200\274\341\200\224\341\200\272\341\200\231\341\200\254\341\200\205\341\200\254",
"nb Norsk",
"nl Nederlands",
"oc Occitan",
"pl Polski",
"pt Portugu\303\252s",
"pt_BR Portugu\303\252s (Brasil)",
"ro Rom\303\242n\304\203",
"ru \320\240\321\203\321\201\321\201\320\272\320\270\320\271",
"sk Sloven\304\215ina",
"sl Sloven\305\241\304\215ina",
"sr_RS \320\241\321\200\320\277\321\201\320\272\320\270",
"sr_RS@latin Srpski",
"sv Svenska",
"ta \340\256\244\340\256\256\340\256\277\340\256\264\340\257\215",
"tg \320\242\320\276\322\267\320\270\320\272\323\243",
"tr T\303\274rk\303\247e",
"uk \320\243\320\272\321\200\320\260\321\227\320\275\321\201\321\214\320\272\320\260",
"vi Ti\341\272\277ng Vi\341\273\207t",
"zh_CN \344\270\255\346\226\207\357\274\210\347\256\200\344\275\223\357\274\211",
"zh_TW \344\270\255\346\226\207\357\274\210\347\271\201\351\253\224\357\274\211",
   };

   TranslatableStrings tempNames;
   wxArrayString tempCodes;
   ReverseLangHash reverseHash;
   LangHash tempHash;

   const LangHash localLanguageName = []{
      LangHash localLanguageName;
      for ( auto utf8Name : utf8Names )
      {
         auto str = wxString::FromUTF8(utf8Name);
         auto code = str.BeforeFirst(' ');
         auto name = str.AfterFirst(' ');
         localLanguageName[code] = Verbatim( name );
      }
      return localLanguageName;
   }();

#if defined(__WXGTK__)
   {
      wxFileName pathNorm{ wxStandardPaths::Get().GetInstallPrefix() + L"/share/locale" };
      pathNorm.Normalize();
      const wxString newPath{ pathNorm.GetFullPath() };
      if (pathList.end() ==
          std::find(pathList.begin(), pathList.end(), newPath))
         pathList.push_back(newPath);
   }
#endif

   // For each language in our list we look for a corresponding entry in
   // wxLocale.  
   for ( auto end = localLanguageName.end(), i = localLanguageName.begin();
      i != end; ++i )
   {
      const wxLanguageInfo *info = wxLocale::FindLanguageInfo(i->first);

      if (!info) {
         wxASSERT(info != NULL);
         continue;
      }

      wxString fullCode = info->CanonicalName;
      wxString code = fullCode.Left(2);
      auto name = Verbatim( info->Description );

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

      if (fullCode.length() < 2)
         continue;

      auto found = localLanguageName.find( code );
      if ( found != end ) {
         name = found->second;
      }
      found = localLanguageName.find( fullCode );
      if ( found != end ) {
         name = found->second;
      }

      if (TranslationExists(pathList, fullCode)) {
         code = fullCode;
      }

      if (!tempHash[code].empty())
         continue;

      if (TranslationExists(pathList, code) || code==wxT("en")) {
         tempCodes.push_back(code);
         tempNames.push_back(name);
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
      code = wxT("en-simple");
      auto name = XO("Simplified");
      if (TranslationExists(pathList, code) ) {
         tempCodes.push_back(code);
         tempNames.push_back(name);
         tempHash[code] = name;
      }
   }


   // Sort
   unsigned int j;
   for(j=0; j<tempNames.size(); j++){
      reverseHash[tempNames[j]] = tempCodes[j];
   }

   std::sort( tempNames.begin(), tempNames.end(),
      []( const TranslatableString &a, const TranslatableString &b ){
         return a.Translation() < b.Translation();
      } );

   // Add system language
   langNames.push_back(XO("System"));
   langCodes.push_back(wxT("System"));

   for(j=0; j<tempNames.size(); j++) {
      langNames.push_back(tempNames[j]);
      langCodes.push_back(reverseHash[tempNames[j]]);
   }
}

static std::unique_ptr<wxLocale> sLocale;
static wxString sLocaleName;

wxString SetLang( const FilePaths &pathList, const wxString & lang )
{
   wxString result = lang;

   sLocale.reset();

#if defined(__WXMAC__)
   // This should be reviewed again during the wx3 conversion.

   // On OSX, if the LANG environment variable isn't set when
   // using a language like Japanese, an assertion will trigger
   // because conversion to Japanese from "?" doesn't return a
   // valid length, so make OSX happy by defining/overriding
   // the LANG environment variable with U.S. English for now.
   wxSetEnv(wxT("LANG"), wxT("en_US.UTF-8"));
#endif

   const wxLanguageInfo *info = NULL;
   if (!lang.empty() && lang != wxT("System")) {
      // Try to find the given language
      info = wxLocale::FindLanguageInfo(lang);
   }
   if (!info)
   {
      // Not given a language or can't find it; substitute the system language
      result = Languages::GetSystemLanguageCode(pathList);
      info = wxLocale::FindLanguageInfo(result);
      if (!info)
         // Return the substituted system language, but we can't complete setup
         // Should we try to do something better?
         return result;
   }
   sLocale = std::make_unique<wxLocale>(info->Language);

   for( const auto &path : pathList )
      sLocale->AddCatalogLookupPathPrefix( path );

   // LL:  Must add the wxWidgets catalog manually since the search
   //      paths were not set up when mLocale was created.  The
   //      catalogs are search in LIFO order, so add wxstd first.
   sLocale->AddCatalog(wxT("wxstd"));

   // Must match TranslationExists() in Languages.cpp
   sLocale->AddCatalog("audacity");

   // Initialize internationalisation (number formats etc.)
   //
   // This must go _after_ creating the wxLocale instance because
   // creating the wxLocale instance sets the application-wide locale.

   Internat::Init();

   using future1 = decltype(
      // The file of unused strings is part of the source tree scanned by
      // xgettext when compiling the catalog template audacity.pot.
      // Including it here doesn't change that but does make the C++ compiler
      // check for correct syntax, but also generate no object code for them.
#include "FutureStrings.h"
      0
   );

   sLocaleName = wxSetlocale(LC_ALL, NULL);

   return result;
}

wxString GetLocaleName()
{
   return sLocaleName;
}

wxString GetLang()
{
   if (sLocale)
      return sLocale->GetSysName();
   else
      return {};
}

wxString GetLangShort()
{
   if (sLocale)
      return sLocale->GetName();
   else
      return {};
}
}
