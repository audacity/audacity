/**********************************************************************

  Audacity: A Digital Audio Editor

  ModulePrefs.cpp

  Brian Gunlogson
  Joshua Haberman
  Dominic Mazzoni
  James Crook


*******************************************************************//**

\class ModulePrefs
\brief A PrefsPanel to enable/disable certain modules.

*//*******************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/filename.h>

#include "../ShuttleGui.h"

#include "ModulePrefs.h"
#include "../Prefs.h"

////////////////////////////////////////////////////////////////////////////////

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

   mModules.Clear();
   mStatuses.Clear();


   // Iterate through all Modules listed in prefs.
   // Get their names and values.
   gPrefs->SetPath( wxT("Module/") );
   bool bCont = gPrefs->GetFirstEntry(str, dummy);
   while ( bCont ) {
      int iStatus;
      gPrefs->Read( str, &iStatus, kModuleDisabled );
      if( iStatus > kModuleNew ){
         iStatus = kModuleNew;
         gPrefs->Write( str, iStatus );
      }
      //wxLogDebug( wxT("Entry: %s Value: %i"), str.c_str(), iStatus );
      mModules.Add( str );
      mStatuses.Add( iStatus );
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

   S.StartStatic(_(""));
   {
      S.AddFixedText(_("These are experimental Modules. Enable them only if you've read the manual\nand know what you are doing.") );
      S.AddFixedText(wxString(wxT("  ")) + _("'Ask' means Audacity will ask if you want to load the plug-each time it starts.") );
      S.AddFixedText(wxString(wxT("  ")) + _("'Failed' means Audacity thinks the plug-in is broken and won't run it.") );
      S.AddFixedText(wxString(wxT("  ")) + _("'New' is like 'Ask', but asks just once.") );
      S.StartMultiColumn( 2 );
      int i;
      for(i=0;i<(int)mModules.GetCount();i++)
         S.TieChoice( mModules[i], mStatuses[i], &StatusChoices );
      S.EndMultiColumn();

   }
   S.EndStatic();
}

bool ModulePrefs::Apply()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);
   int i;
   for(i=0;i<(int)mModules.GetCount();i++)
      SetModuleStatus( mModules[i], mStatuses[i] );
   return true;
}


// static function that tells us about a module.
int ModulePrefs::GetModuleStatus( wxString fname ){
   // Default status is new module, and we will ask once.
   int iStatus = kModuleNew;

   wxString ShortName = wxFileName( fname ).GetName();
   wxString PrefName = wxString( wxT("/Module/") ) + ShortName.Lower();

   gPrefs->Read( PrefName, &iStatus, kModuleNew );
   // fix up a bad status.
   if( iStatus > kModuleNew )
      iStatus=kModuleNew;
   return iStatus;
}

void ModulePrefs::SetModuleStatus( wxString fname, int iStatus ){
   wxString ShortName = wxFileName( fname ).GetName();
   wxString PrefName = wxString( wxT("/Module/") ) + ShortName.Lower();
   gPrefs->Write( PrefName, iStatus );
}



