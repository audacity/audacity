/**********************************************************************

  Audacity: A Digital Audio Editor

  ModulePrefs.cpp

  James Crook

*******************************************************************//**

\class ModulePrefs
\brief A PrefsPanel to enable/disable certain modules.  'Modules' are 
dynamically linked libraries that modify Audacity.  They are plug-ins 
with names like mnod-script-pipe that add NEW features.

*//*******************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/filename.h>

#include "../ShuttleGui.h"

#include "ModulePrefs.h"
#include "../Prefs.h"

////////////////////////////////////////////////////////////////////////////////

/* i18n-hint: Modules are optional extensions to Audacity that add NEW features.*/
ModulePrefs::ModulePrefs(wxWindow * parent)
:  PrefsPanel(parent, _("Modules"))
{
   Populate();
}

ModulePrefs::~ModulePrefs()
{
}

void ModulePrefs::GetAllModuleStatuses(){
   wxString str;
   long dummy;

   // Modules could for example be:
   //    mod-script-pipe
   //    mod-nyq-bench
   //    mod-track-panel
   //    mod-menu-munger
   //    mod-theming

   // TODO: On an Audacity upgrade we should (?) actually untick modules.
   // The old modules might be still around, and we do not want to use them.
   mModules.Clear();
   mStatuses.Clear();
   mPaths.Clear();


   // Iterate through all Modules listed in prefs.
   // Get their names and values.
   gPrefs->SetPath( wxT("Module/") );
   bool bCont = gPrefs->GetFirstEntry(str, dummy);
   while ( bCont ) {
      int iStatus;
      gPrefs->Read( str, &iStatus, kModuleDisabled );
      wxString fname;
      gPrefs->Read( wxString( wxT("/ModulePath/") ) + str, &fname, wxEmptyString );
      if( fname != wxEmptyString && wxFileExists( fname ) ){
         if( iStatus > kModuleNew ){
            iStatus = kModuleNew;
            gPrefs->Write( str, iStatus );
         }
         //wxLogDebug( wxT("Entry: %s Value: %i"), str.c_str(), iStatus );
         mModules.Add( str );
         mStatuses.Add( iStatus );
         mPaths.Add( fname );
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
   wxArrayString StatusChoices;
   StatusChoices.Add( _("Disabled" ) );
   StatusChoices.Add( _("Enabled" ) );
   StatusChoices.Add( _("Ask" ) );
   StatusChoices.Add( _("Failed" ) );
   StatusChoices.Add( _("New" ) );
   S.SetBorder(2);

   S.StartStatic(wxT(""));
   {
      S.AddFixedText(_("These are experimental modules. Enable them only if you've read the Audacity Manual\nand know what you are doing.") );
      S.AddFixedText(wxString(wxT("  ")) + _("'Ask' means Audacity will ask if you want to load the module each time it starts.") );
      S.AddFixedText(wxString(wxT("  ")) + _("'Failed' means Audacity thinks the module is broken and won't run it.") );
      S.AddFixedText(wxString(wxT("  ")) + _("'New' means no choice has been made yet.") );
      S.AddFixedText(_("Changes to these settings only take effect when Audacity starts up."));
      S.StartScroller();
      {
        S.StartMultiColumn( 2 );
        int i;
        for(i=0;i<(int)mModules.GetCount();i++)
           S.TieChoice( mModules[i], mStatuses[i], &StatusChoices );
        S.EndMultiColumn();
      }
      if( mModules.GetCount() < 1 )
      {
        S.AddFixedText( _("No modules were found") );
      }
      S.EndScroller();
   }
   S.EndStatic();
}

bool ModulePrefs::Apply()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);
   int i;
   for(i=0;i<(int)mPaths.GetCount();i++)
      SetModuleStatus( mPaths[i], mStatuses[i] );
   return true;
}


// static function that tells us about a module.
int ModulePrefs::GetModuleStatus(const wxString &fname){
   // Default status is NEW module, and we will ask once.
   int iStatus = kModuleNew;

   wxString ShortName = wxFileName( fname ).GetName();
   wxString PrefName = wxString( wxT("/Module/") ) + ShortName.Lower();

   gPrefs->Read( PrefName, &iStatus, kModuleNew );
   // fix up a bad status.
   if( iStatus > kModuleNew )
      iStatus=kModuleNew;
   return iStatus;
}

void ModulePrefs::SetModuleStatus(const wxString &fname, int iStatus){
   wxString ShortName = wxFileName( fname ).GetName();
   wxString PrefName = wxString( wxT("/Module/") ) + ShortName.Lower();
   gPrefs->Write( PrefName, iStatus );
   PrefName = wxString( wxT("/ModulePath/") ) + ShortName.Lower();
   gPrefs->Write( PrefName, fname );
   gPrefs->Flush();
}

PrefsPanel *ModulePrefsFactory::Create(wxWindow *parent)
{
   wxASSERT(parent); // to justify safenew
   return safenew ModulePrefs(parent);
}
