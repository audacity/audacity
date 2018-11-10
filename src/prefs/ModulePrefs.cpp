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
#include "ModulePrefs.h"

#include <wx/defs.h>
#include <wx/filename.h>

#include "../ShuttleGui.h"

#include "../Prefs.h"
#include "../Internat.h"

////////////////////////////////////////////////////////////////////////////////

/* i18n-hint: Modules are optional extensions to Audacity that add NEW features.*/
ModulePrefs::ModulePrefs(wxWindow * parent, wxWindowID winid)
:  PrefsPanel(parent, winid, _("Modules"))
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
      S.AddFixedText(_("These are experimental modules. Enable them only if you've read the Audacity Manual\nand know what you are doing.") );
      S.AddFixedText(wxString(wxT("  ")) + _("'Ask' means Audacity will ask if you want to load the module each time it starts.") );
      S.AddFixedText(wxString(wxT("  ")) + _("'Failed' means Audacity thinks the module is broken and won't run it.") );
      S.AddFixedText(wxString(wxT("  ")) + _("'New' means no choice has been made yet.") );
      S.AddFixedText(_("Changes to these settings only take effect when Audacity starts up."));
      {
        S.StartMultiColumn( 2 );
        int i;
        for(i=0;i<(int)mModules.size();i++)
           S.TieChoice( mModules[i],
              mStatuses[i],
              {
                 _("Disabled" ) ,
                 _("Enabled" ) ,
                 _("Ask" ) ,
                 _("Failed" ) ,
                 _("New" ) ,
              }
           );
        S.EndMultiColumn();
      }
      if( mModules.size() < 1 )
      {
        S.AddFixedText( _("No modules were found") );
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
      SetModuleStatus( mPaths[i], mStatuses[i] );
   return true;
}


// static function that tells us about a module.
int ModulePrefs::GetModuleStatus(const FilePath &fname){
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

void ModulePrefs::SetModuleStatus(const FilePath &fname, int iStatus){
   wxString ShortName = wxFileName( fname ).GetName();
   wxString PrefName = wxString( wxT("/Module/") ) + ShortName.Lower();
   gPrefs->Write( PrefName, iStatus );
   PrefName = wxString( wxT("/ModulePath/") ) + ShortName.Lower();
   gPrefs->Write( PrefName, fname );
   gPrefs->Flush();
}

wxString ModulePrefs::HelpPageName()
{
   return "Modules_Preferences";
}

PrefsPanel *ModulePrefsFactory::operator () (wxWindow *parent, wxWindowID winid)
{
   wxASSERT(parent); // to justify safenew
   return safenew ModulePrefs(parent, winid);
}
