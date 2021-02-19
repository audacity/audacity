/**********************************************************************

  Audacity: A Digital Audio Editor

  ModulePrefs.cpp

  James Crook

*******************************************************************//**

\class ModulePrefs
\brief A PrefsPanel to enable/disable certain modules.  'Modules' are 
dynamically linked libraries that modify Audacity.  They are plug-ins 
with names like mod-script-pipe that add NEW features.

*//*******************************************************************/


#include "ModulePrefs.h"



#include <wx/defs.h>
#include <wx/filename.h>

#include "ShuttleGui.h"

#include "Prefs.h"
#include "ModuleSettings.h"

////////////////////////////////////////////////////////////////////////////////

ModulePrefs::ModulePrefs(wxWindow * parent, wxWindowID winid)
/* i18n-hint: Modules are optional extensions to Audacity that add NEW features.*/
:  PrefsPanel(parent, winid, XO("Modules"))
{
   Populate();
}

ModulePrefs::~ModulePrefs()
{
}

ComponentInterfaceSymbol ModulePrefs::GetSymbol() const
{
   return MODULE_PREFS_PLUGIN_SYMBOL;
}

TranslatableString ModulePrefs::GetDescription() const
{
   return XO("Preferences for Module");
}

ManualPageID ModulePrefs::HelpPageName()
{
   return "Modules_Preferences";
}

void ModulePrefs::GetAllModuleStatuses(){
   wxString str;
   long dummy;

   // Modules could for example be:
   //    mod-script-pipe
   //    mod-nyq-bench
   //    mod-menu-munger
   //    mod-theming

   // TODO: On an Audacity upgrade we should (?) actually untick modules.
   // The old modules might be still around, and we do not want to use them.
   mModules.clear();
   mStatuses.clear();
   mPaths.clear();


   // Iterate through all Modules listed in prefs.
   // Get their names and values.
   gPrefs->SetPath( wxT("Module/") );
   bool bCont = gPrefs->GetFirstEntry(str, dummy);
   while ( bCont ) {
      int iStatus;
      gPrefs->Read( str, &iStatus, kModuleDisabled );
      wxString fname;
      gPrefs->Read( wxString( wxT("/ModulePath/") ) + str, &fname, wxEmptyString );
      if( !fname.empty() && wxFileExists( fname ) ){
         if( iStatus > kModuleNew ){
            iStatus = kModuleNew;
            gPrefs->Write( str, iStatus );
         }
         //wxLogDebug( wxT("Entry: %s Value: %i"), str, iStatus );
         mModules.push_back( str );
         mStatuses.push_back( iStatus );
         mPaths.push_back( fname );
      }
      bCont = gPrefs->GetNextEntry(str, dummy);
   }
   gPrefs->SetPath( wxT("") );
}

void ModulePrefs::Populate()
{
   GetAllModuleStatuses();
   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void ModulePrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);
   S.StartScroller();

   S.StartStatic( {} );
   {
      S.AddFixedText(XO(
"These are experimental modules. Enable them only if you've read the Audacity Manual\nand know what you are doing.") );
      S.AddFixedText(XO(
/* i18n-hint preserve the leading spaces */
"  'Ask' means Audacity will ask if you want to load the module each time it starts.") );
      S.AddFixedText(XO(
/* i18n-hint preserve the leading spaces */
"  'Failed' means Audacity thinks the module is broken and won't run it.") );
      S.AddFixedText(XO(
/* i18n-hint preserve the leading spaces */
"  'New' means no choice has been made yet.") );
      S.AddFixedText(XO(
"Changes to these settings only take effect when Audacity starts up."));
      {
        S.StartMultiColumn( 2 );
        int i;
        for(i=0;i<(int)mModules.size();i++)
           S.TieChoice( Verbatim( mModules[i] ),
              mStatuses[i],
              {
                 XO("Disabled" ) ,
                 XO("Enabled" ) ,
                 XO("Ask" ) ,
                 XO("Failed" ) ,
                 XO("New" ) ,
              }
           );
        S.EndMultiColumn();
      }
      if( mModules.size() < 1 )
      {
        S.AddFixedText( XO("No modules were found") );
      }
   }
   S.EndStatic();
   S.EndScroller();
}

bool ModulePrefs::Commit()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);
   int i;
   for(i=0;i<(int)mPaths.size();i++)
      ModuleSettings::SetModuleStatus( mPaths[i], mStatuses[i] );
   return true;
}

#ifdef EXPERIMENTAL_MODULE_PREFS
namespace{
PrefsPanel::Registration sAttachment{ "Module",
   [](wxWindow *parent, wxWindowID winid, AudacityProject *)
   {
      wxASSERT(parent); // to justify safenew
      return safenew ModulePrefs(parent, winid);
   },
   false,
   // Register with an explicit ordering hint because this one is
   // only conditionally compiled
   { "", { Registry::OrderingHint::After, "Mouse" } }
};
}
#endif
