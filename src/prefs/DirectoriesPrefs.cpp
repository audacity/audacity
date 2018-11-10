/**********************************************************************

  Audacity: A Digital Audio Editor

  DirectoriesPrefs.cpp

  Joshua Haberman
  James Crook


*******************************************************************//**

\class DirectoriesPrefs
\brief A PrefsPanel used to select directories.

*//*******************************************************************/

#include "../Audacity.h"
#include "DirectoriesPrefs.h"

#include <math.h>

#include <wx/defs.h>
#include <wx/intl.h>
#include <wx/log.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/button.h>
#include <wx/dirdlg.h>
#include <wx/event.h>
#include <wx/filefn.h>
#include <wx/filename.h>
#include <wx/utils.h>

#include "../Prefs.h"
#include "../AudacityApp.h"
#include "../Internat.h"
#include "../ShuttleGui.h"
#include "../widgets/ErrorDialog.h"

enum {
   TempDirID = 1000,
   ChooseButtonID
};

BEGIN_EVENT_TABLE(DirectoriesPrefs, PrefsPanel)
   EVT_TEXT(TempDirID, DirectoriesPrefs::UpdateFreeSpace)
   EVT_BUTTON(ChooseButtonID, DirectoriesPrefs::OnChooseTempDir)
END_EVENT_TABLE()

DirectoriesPrefs::DirectoriesPrefs(wxWindow * parent, wxWindowID winid)
/* i18n-hint:  Directories, also called folders, in computer file systems */
:  PrefsPanel(parent, winid, _("Directories")),
   mFreeSpace(NULL),
   mTempDir(NULL)
{
   Populate();
}

DirectoriesPrefs::~DirectoriesPrefs()
{
}

/// Creates the dialog and its contents.
void DirectoriesPrefs::Populate()
{
   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------

   wxCommandEvent e;
   UpdateFreeSpace(e);
}

void DirectoriesPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);
   S.StartScroller();

   S.StartStatic(_("Temporary files directory"));
   {
      S.StartMultiColumn(2, wxEXPAND);
      {
         S.SetStretchyCol(1);

         S.Id(TempDirID);
         mTempDir = S.TieTextBox(_("&Location:"),
                                 wxT("/Directories/TempDir"),
                                 wxT(""),
                                 30);
      }
      S.EndMultiColumn();
      S.StartHorizontalLay(wxEXPAND);
      {
         S.Prop(0).AddFixedText(_("Free Space:"));
         mFreeSpace = S.Prop(0).AddVariableText( {} );
         S.Prop(10).AddSpace( 10 );
         S.Id(ChooseButtonID).Prop(0).AddButton(_("C&hoose..."));
      }

   }
   S.EndStatic();

#ifdef DEPRECATED_AUDIO_CACHE
   // See http://bugzilla.audacityteam.org/show_bug.cgi?id=545.
   S.StartStatic(_("Audio cache"));
   {
      S.TieCheckBox(_("Play and/or record using &RAM (useful for slow drives)"),
                    wxT("/Directories/CacheBlockFiles"),
                    false);

      S.StartTwoColumn();
      {
         S.TieNumericTextBox(_("Mi&nimum Free Memory (MB):"),
                             wxT("/Directories/CacheLowMem"),
                             16,
                             9);
      }
      S.EndTwoColumn();

      S.AddVariableText(_("If the available system memory falls below this value, audio will no longer\nbe cached in memory and will be written to disk."))->Wrap(600);
   }
   S.EndStatic();
#endif // DEPRECATED_AUDIO_CACHE
   S.EndScroller();

}

void DirectoriesPrefs::OnChooseTempDir(wxCommandEvent & e)
{
   wxString oldTempDir = gPrefs->Read(wxT("/Directories/TempDir"), wxGetApp().defaultTempDir);

   // Because we went through InitTempDir() during initialisation,
   // the old temp directory name in prefs should already be OK.  Just in case there is 
   // some way we hadn't thought of for it to be not OK, 
   // we avoid prompting with it in that case and use the suggested default instead.
   if( !AudacityApp::IsTempDirectoryNameOK( oldTempDir ) )
      oldTempDir = wxGetApp().defaultTempDir;

   wxDirDialogWrapper dlog(this,
                    _("Choose a location to place the temporary directory"),
                    oldTempDir );
   int retval = dlog.ShowModal();
   if (retval != wxID_CANCEL && !dlog.GetPath().empty()) {
      wxFileName tmpDirPath;
      tmpDirPath.AssignDir(dlog.GetPath());

      // Append an "audacity_temp" directory to this path if necessary (the
      // default, the existing pref (as stored in the control), and any path
      // ending in a directory with the same name as what we'd add should be OK
      // already)
      wxString newDirName;
#if defined(__WXMAC__)
      newDirName = wxT("SessionData");
#elif defined(__WXMSW__) 
      // Clearing Bug 1271 residual issue.  Let's NOT have temp in the name.
      newDirName = wxT("SessionData");
#else
      newDirName = wxT(".audacity_temp");
#endif
      auto dirsInPath = tmpDirPath.GetDirs();

      // If the default temp dir or user's pref dir don't end in '/' they cause
      // wxFileName's == operator to construct a wxFileName representing a file
      // (that doesn't exist) -- hence the constructor calls
      if (tmpDirPath != wxFileName(wxGetApp().defaultTempDir, wxT("")) &&
            tmpDirPath != wxFileName(mTempDir->GetValue(), wxT("")) &&
            (dirsInPath.size() == 0 ||
             dirsInPath[dirsInPath.size()-1] != newDirName))
      {
         tmpDirPath.AppendDir(newDirName);
      }

      mTempDir->SetValue(tmpDirPath.GetPath(wxPATH_GET_VOLUME|wxPATH_GET_SEPARATOR));
      UpdateFreeSpace(e);
   }
}

void DirectoriesPrefs::UpdateFreeSpace(wxCommandEvent & WXUNUSED(event))
{
   wxString tempDir;
   wxString label;

   if (mTempDir != NULL) {
      tempDir = mTempDir->GetValue();
   }

   if (wxDirExists(tempDir)) {
      wxLongLong space;
      wxGetDiskSpace(tempDir, NULL, &space);
      label = Internat::FormatSize(space);
   }
   else {
      label = _("unavailable - above location doesn't exist");
   }

   if( mFreeSpace != NULL ) {
      mFreeSpace->SetLabel(label);
      mFreeSpace->SetName(label); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
   }
}

bool DirectoriesPrefs::Validate()
{
   wxFileName tempDir;
   tempDir.SetPath(mTempDir->GetValue());

   wxString path{tempDir.GetPath()};
   if( !AudacityApp::IsTempDirectoryNameOK( path ) ) {
      AudacityMessageBox(
         wxString::Format(_("Directory %s is not suitable (at risk of being cleaned out)"),
                           path),
         _("Error"),
         wxOK | wxICON_ERROR);
      return false;
   }
   if (!tempDir.DirExists()) {
      int ans = AudacityMessageBox(
         wxString::Format(_("Directory %s does not exist. Create it?"),
                          path),
         _("New Temporary Directory"),
         wxYES_NO | wxCENTRE | wxICON_EXCLAMATION);

      if (ans != wxYES) {
         return false;
      }

      if (!tempDir.Mkdir(0755, wxPATH_MKDIR_FULL)) {
         /* wxWidgets throws up a decent looking dialog */
         return false;
      }
   }
   else {
      /* If the directory already exists, make sure it is writable */
      wxLogNull logNo;
      tempDir.AppendDir(wxT("canicreate"));
      path =  tempDir.GetPath();
      if (!tempDir.Mkdir(0755)) {
         AudacityMessageBox(
            wxString::Format(_("Directory %s is not writable"),
                             path),
            _("Error"),
            wxOK | wxICON_ERROR);
         return false;
      }
      tempDir.Rmdir();
      tempDir.RemoveLastDir();
   }

   wxFileName oldDir;
   oldDir.SetPath(gPrefs->Read(wxT("/Directories/TempDir")));
   if (tempDir != oldDir) {
      AudacityMessageBox(
         _("Changes to temporary directory will not take effect until Audacity is restarted"),
         _("Temp Directory Update"),
         wxOK | wxCENTRE | wxICON_INFORMATION);
   }

   return true;
}

bool DirectoriesPrefs::Commit()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   return true;
}

wxString DirectoriesPrefs::HelpPageName()
{
   return "Directories_Preferences";
}

PrefsPanel *DirectoriesPrefsFactory::operator () (wxWindow *parent, wxWindowID winid)
{
   wxASSERT(parent); // to justify safenew
   return safenew DirectoriesPrefs(parent, winid);
}
