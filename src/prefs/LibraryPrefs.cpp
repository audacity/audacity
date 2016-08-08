/**********************************************************************

  Audacity: A Digital Audio Editor

  LibraryPrefs.cpp

  Joshua Haberman
  Dominic Mazzoni
  James Crook

*******************************************************************//**

\class LibraryPrefs
\brief A PrefsPanel used to select manage external libraries like the
MP3 and FFmpeg encoding libraries.

*//*******************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/button.h>

#include "../FFmpeg.h"
#include "../ShuttleGui.h"
#include "../export/ExportMP3.h"
#include "../widgets/LinkingHtmlWindow.h"
#include "../widgets/HelpSystem.h"

#include "LibraryPrefs.h"

#include "../Experimental.h"

////////////////////////////////////////////////////////////////////////////////

#define ID_MP3_FIND_BUTTON          7001
#define ID_MP3_DOWN_BUTTON          7002
#define ID_FFMPEG_FIND_BUTTON       7003
#define ID_FFMPEG_DOWN_BUTTON       7004

BEGIN_EVENT_TABLE(LibraryPrefs, PrefsPanel)
   EVT_BUTTON(ID_MP3_FIND_BUTTON, LibraryPrefs::OnMP3FindButton)
   EVT_BUTTON(ID_MP3_DOWN_BUTTON, LibraryPrefs::OnMP3DownButton)
   EVT_BUTTON(ID_FFMPEG_FIND_BUTTON, LibraryPrefs::OnFFmpegFindButton)
   EVT_BUTTON(ID_FFMPEG_DOWN_BUTTON, LibraryPrefs::OnFFmpegDownButton)
END_EVENT_TABLE()

LibraryPrefs::LibraryPrefs(wxWindow * parent)
:   PrefsPanel(parent, _("Libraries"))
{
   Populate();
}

LibraryPrefs::~LibraryPrefs()
{
}

/// Creates the dialog and its contents.
void LibraryPrefs::Populate()
{
   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------

   // Set the MP3 Version string.
   SetMP3VersionText();
   SetFFmpegVersionText();
}

/// This PopulateOrExchange function is a good example of mixing the fully
/// automatic style of reading/writing from GUI to prefs with the partial form.
///
/// You'll notice that some of the Tie functions have Prefs identifiers in them
/// and others don't.
void LibraryPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);
   S.StartStatic(_("MP3 Export Library"));
   {
      S.StartTwoColumn();
      {
         S.AddVariableText(_("MP3 Library Version:"),
                           true,
                           wxALL | wxALIGN_RIGHT | wxALIGN_CENTRE_VERTICAL);
         mMP3Version = S.AddVariableText(wxT("9.99"),
                                         true,
                                         wxALL | wxALIGN_LEFT | wxALIGN_CENTRE_VERTICAL);
         S.AddVariableText(_("MP3 Library:"),
                           true,
                           wxALL | wxALIGN_RIGHT | wxALIGN_CENTRE_VERTICAL);
         wxButton *locate_button = S.Id(ID_MP3_FIND_BUTTON).AddButton(_("&Locate..."),
                                            wxALL | wxALIGN_LEFT | wxALIGN_CENTRE_VERTICAL);
         S.AddVariableText(_("LAME MP3 Library:"),
                           true,
                           wxALL | wxALIGN_RIGHT | wxALIGN_CENTRE_VERTICAL);
         wxButton *download_button = S.Id(ID_MP3_DOWN_BUTTON).AddButton(_("&Download"),
                                            wxALL | wxALIGN_LEFT | wxALIGN_CENTRE_VERTICAL);
#ifdef DISABLE_DYNAMIC_LOADING_LAME
         locate_button->Enable(FALSE);
         download_button->Enable(FALSE);
#else
        (void)locate_button;
        (void)download_button;
#endif // DISABLE_DYNAMIC_LOADING_LAME
      }
      S.EndTwoColumn();
   }
   S.EndStatic();

   S.StartStatic(_("FFmpeg Import/Export Library"));
   {
      S.StartTwoColumn();
      {
         S.AddVariableText(_("FFmpeg Library Version:"),
                           true,
                           wxALL | wxALIGN_RIGHT | wxALIGN_CENTRE_VERTICAL);
#if defined(USE_FFMPEG)
         mFFmpegVersion = S.AddVariableText(_("No compatible FFmpeg library was found"),
                                            true,
                                            wxALL | wxALIGN_LEFT | wxALIGN_CENTRE_VERTICAL);
#else
         mFFmpegVersion = S.AddVariableText(wxT("FFmpeg support is not compiled in"),
                                            true,
                                            wxALL | wxALIGN_LEFT | wxALIGN_CENTRE_VERTICAL);
#endif
         S.AddVariableText(_("FFmpeg Library:"),
                           true,
                           wxALL | wxALIGN_RIGHT | wxALIGN_CENTRE_VERTICAL);
         S.Id(ID_FFMPEG_FIND_BUTTON);
         wxButton *bfnd = S.AddButton(_("Loca&te..."),
                                      wxALL | wxALIGN_LEFT | wxALIGN_CENTRE_VERTICAL);
         S.AddVariableText(_("FFmpeg Library:"),
                           true,
                           wxALL | wxALIGN_RIGHT | wxALIGN_CENTRE_VERTICAL);
         S.Id(ID_FFMPEG_DOWN_BUTTON);
         wxButton *bdwn = S.AddButton(_("Dow&nload"),
                                      wxALL | wxALIGN_LEFT | wxALIGN_CENTRE_VERTICAL);
#if !defined(USE_FFMPEG) || defined(DISABLE_DYNAMIC_LOADING_FFMPEG)
         bdwn->Enable(FALSE);
         bfnd->Enable(FALSE);
#else
         // fix compilation warnings about unused variables
         wxUnusedVar(bfnd);
         wxUnusedVar(bdwn);
#endif
      }
      S.EndTwoColumn();
#ifdef EXPERIMENTAL_OD_FFMPEG
      wxCheckBox* checkbox = S.TieCheckBox(_("Allow &background on-demand loading"),
                    wxT("/Library/FFmpegOnDemand"),
                    false);
#if !defined(USE_FFMPEG)
      checkbox->Enable(FALSE);
#endif
#endif
   }
   S.EndStatic();
}

/// Sets the a text area on the dialog to have the name
/// of the MP3 Library version.
void LibraryPrefs::SetMP3VersionText(bool prompt)
{
   mMP3Version->SetLabel(GetMP3Version(this, prompt));
   mMP3Version->SetName(mMP3Version->GetLabel()); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
}

/// Opens a file-finder dialog so that the user can
/// tell us where the MP3 library is.
void LibraryPrefs::OnMP3FindButton(wxCommandEvent & WXUNUSED(event))
{
   SetMP3VersionText(true);
}

/// Opens help on downloading a suitable MP3 library is.
void LibraryPrefs::OnMP3DownButton(wxCommandEvent & WXUNUSED(event))
{
   // Modal help dialogue required here
   HelpSystem::ShowHelpDialog(this, wxT("FAQ:Installation_and_Plug-Ins#lame"), true);
}

void LibraryPrefs::SetFFmpegVersionText()
{
   mFFmpegVersion->SetLabel(GetFFmpegVersion(this));
   mFFmpegVersion->SetName(mFFmpegVersion->GetLabel()); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
}

void LibraryPrefs::OnFFmpegFindButton(wxCommandEvent & WXUNUSED(event))
{
#ifdef USE_FFMPEG
   FFmpegLibs* FFmpegLibsPtr = PickFFmpegLibs();
   bool showerrs =
#if defined(__WXDEBUG__)
      true;
#else
      false;
#endif

   FFmpegLibsPtr->FreeLibs();
   // Load the libs ('true' means that all errors will be shown)
   bool locate = !LoadFFmpeg(showerrs);

   // Libs are fine, don't show "locate" dialog unless user really wants it
   if (!locate) {
      int response = wxMessageBox(_("Audacity has automatically detected valid FFmpeg libraries.\nDo you still want to locate them manually?"),
                                  wxT("Success"),
                                  wxCENTRE | wxYES_NO | wxNO_DEFAULT |wxICON_QUESTION);
      if (response == wxYES) {
        locate = true;
      }
   }

   if (locate) {
      // Show "Locate FFmpeg" dialog
      FFmpegLibsPtr->FindLibs(this);
      FFmpegLibsPtr->FreeLibs();
      LoadFFmpeg(showerrs);
   }
   SetFFmpegVersionText();

   DropFFmpegLibs();
#endif
}

void LibraryPrefs::OnFFmpegDownButton(wxCommandEvent & WXUNUSED(event))
{
   HelpSystem::ShowHelpDialog(this, wxT("FAQ:Installation_and_Plug-Ins#ffdown"), true);
}

bool LibraryPrefs::Apply()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   return true;
}

PrefsPanel *LibraryPrefsFactory::Create(wxWindow *parent)
{
   wxASSERT(parent); // to justify safenew
   return safenew LibraryPrefs(parent);
}
