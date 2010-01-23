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
#include <wx/msgdlg.h>
#include <wx/utils.h>

#include "../Prefs.h"
#include "../AudacityApp.h"
#include "../Internat.h"
#include "../ShuttleGui.h"
#include "DirectoriesPrefs.h"

enum {
   TempDirID = 1000,
   ChooseButtonID
};

BEGIN_EVENT_TABLE(DirectoriesPrefs, PrefsPanel)
   EVT_TEXT(TempDirID, DirectoriesPrefs::UpdateFreeSpace)
   EVT_BUTTON(ChooseButtonID, DirectoriesPrefs::OnChooseTempDir)
END_EVENT_TABLE()

DirectoriesPrefs::DirectoriesPrefs(wxWindow * parent)
:  PrefsPanel(parent, _("Directories")),
   mTempDir(NULL),
   mFreeSpace(NULL)
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

   S.StartStatic(_("Temporary files directory"));
   {
      S.StartMultiColumn(3, wxEXPAND);
      {
         S.SetStretchyCol(1);

         S.Id(TempDirID);
         mTempDir = S.TieTextBox(_("&Location:"),
                                 wxT("/Directories/TempDir"),
                                 wxT(""),
                                 30);
         S.Id(ChooseButtonID);
         S.AddButton(_("C&hoose..."));

         S.AddFixedText(_("Free Space:"));
         mFreeSpace = S.AddVariableText(wxT(""));
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   S.StartStatic(_("Audio cache"));
   {
      S.TieCheckBox(_("Play and/or record using &RAM (useful for slow drives)"),
                    wxT("/Directories/CacheBlockFiles"),
                    false);

      S.StartTwoColumn();
      {
         S.TieTextBox(_("Mi&nimum Free Memory (MB):"),
                      wxT("/Directories/CacheLowMem"),
                      16,
                      9);
      }
      S.EndTwoColumn();

      S.AddVariableText(_("If the available system memory falls below this value, audio will no longer\nbe cached in memory and will be written to disk."))->Wrap(600);
   }
   S.EndStatic();
}

void DirectoriesPrefs::OnChooseTempDir(wxCommandEvent & e)
{
   wxDirDialog dlog(this, 
                    _("Choose a location to place the temporary directory"), 
                    gPrefs->Read(wxT("/Directories/TempDir"), 
                                 wxGetApp().defaultTempDir));
   dlog.ShowModal();
   if (dlog.GetPath() != wxT("")) {
      wxFileName tmpDirPath;
      tmpDirPath.AssignDir(dlog.GetPath());
#if defined(__WXMSW__) || defined(__WXMAC__)
      tmpDirPath.AppendDir(wxT("audacity_temp"));
#else
      tmpDirPath.AppendDir(wxT(".audacity_temp"));
#endif
      mTempDir->SetValue(tmpDirPath.GetPath(wxPATH_GET_VOLUME|wxPATH_GET_SEPARATOR));
      UpdateFreeSpace(e);
   }
}

void DirectoriesPrefs::UpdateFreeSpace(wxCommandEvent & e)
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
   }
}

bool DirectoriesPrefs::Validate()
{
   wxFileName tempDir;
   tempDir.SetPath(mTempDir->GetValue());

   if (!tempDir.DirExists()) {
      int ans = wxMessageBox(
         wxString::Format(_("Directory %s does not exist. Create it?"),
                          tempDir.GetPath().c_str()),
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
      if (!tempDir.Mkdir(0755)) {
         wxMessageBox(
            wxString::Format(_("Directory %s is not writable"),
                             tempDir.GetPath().c_str()),
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
      wxMessageBox(
         _("Changes to temporary directory will not take effect until Audacity is restarted"),
         wxT("Temp Directory Update"),
         wxOK | wxCENTRE | wxICON_INFORMATION);
   }

   return true;
}

bool DirectoriesPrefs::Apply()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   return true;
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: b152d0c9-973a-44a2-a6ce-b4f6e79be37b

