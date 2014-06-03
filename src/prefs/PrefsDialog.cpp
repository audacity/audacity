/**********************************************************************

  Audacity: A Digital Audio Editor

  PrefsDialog.cpp

  Joshua Haberman
  James Crook

*******************************************************************//**

\class PrefsDialog
\brief Dialog that shows the current PrefsPanel in a tabbed divider.

*//*******************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/event.h>
#include <wx/font.h>
#include <wx/gdicmn.h>
#include <wx/intl.h>
#include <wx/listbox.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>

#include <wx/listbook.h>

#if wxCHECK_VERSION(2, 8, 4)
#include <wx/treebook.h>
#else
#include "../widgets/treebook.h"
#endif

#include "../Experimental.h"
#include "../Project.h"
#include "../Prefs.h"

#include "PrefsDialog.h"
#include "PrefsPanel.h"

#include "BatchPrefs.h"
#include "DevicePrefs.h"
#include "DirectoriesPrefs.h"
#include "EffectsPrefs.h"
#include "GUIPrefs.h"
#include "ImportExportPrefs.h"
#include "KeyConfigPrefs.h"
#include "LibraryPrefs.h"
#include "MousePrefs.h"
#ifdef EXPERIMENTAL_MODULE_PREFS
#include "ModulePrefs.h"
#endif
#include "PlaybackPrefs.h"
#include "ProjectsPrefs.h"
#include "QualityPrefs.h"
#include "RecordingPrefs.h"
#include "SpectrumPrefs.h"
#include "ThemePrefs.h"
#include "TracksPrefs.h"
#include "WarningsPrefs.h"
#include "ExtImportPrefs.h"

#ifdef EXPERIMENTAL_MIDI_OUT
#include "MidiIOPrefs.h"
#endif

BEGIN_EVENT_TABLE(PrefsDialog, wxDialog)
   EVT_BUTTON(wxID_OK, PrefsDialog::OnOK)
   EVT_BUTTON(wxID_CANCEL, PrefsDialog::OnCancel)
   EVT_TREE_KEY_DOWN(wxID_ANY, PrefsDialog::OnTreeKeyDown) // Handles key events when tree has focus
END_EVENT_TABLE()


class wxTreebookExt : public wxTreebook
{
public:
   wxTreebookExt( wxWindow *parent,
      wxWindowID id) : wxTreebook( parent, id )
   {;};
   ~wxTreebookExt(){;};
   virtual int ChangeSelection(size_t n);
   virtual int SetSelection(size_t n);
};


int wxTreebookExt::ChangeSelection(size_t n) {
   int i = wxTreebook::ChangeSelection(n);
   wxString Temp = GetPageText( n );
   ((wxDialog*)GetParent())->SetTitle( Temp );
   return i;
};

int wxTreebookExt::SetSelection(size_t n)
{
   int i = wxTreebook::SetSelection(n);
   wxString Temp = wxString(_("Preferences: ")) + GetPageText( n );
   ((wxDialog*)GetParent())->SetTitle( Temp );
   return i;
}



PrefsDialog::PrefsDialog(wxWindow * parent)
:  wxDialog(parent, wxID_ANY, wxString(_("Audacity Preferences")),
            wxDefaultPosition,
            wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
{
   ShuttleGui S(this, eIsCreating);

   S.StartVerticalLay(true);
   {
      S.StartHorizontalLay(wxALIGN_LEFT | wxEXPAND, true);
      {
         mCategories = new wxTreebookExt(this, wxID_ANY);
         S.Prop(1);
         S.AddWindow(mCategories, wxEXPAND);

         wxWindow *w;
         // Parameters are: AddPage(page, name, IsSelected, imageId).
         w = new DevicePrefs(mCategories);      mCategories->AddPage(w, w->GetName(), false, 0);
         w = new PlaybackPrefs(mCategories);    mCategories->AddPage(w, w->GetName(), false, 0);
         w = new RecordingPrefs(mCategories);   mCategories->AddPage(w, w->GetName(), false, 0);
#ifdef EXPERIMENTAL_MIDI_OUT
         w = new MidiIOPrefs(mCategories);      mCategories->AddPage(w, w->GetName(), false, 0);
#endif
         w = new QualityPrefs(mCategories);     mCategories->AddPage(w, w->GetName(), false, 0);
         w = new GUIPrefs(mCategories);         mCategories->AddPage(w, w->GetName(), false, 0);
         w = new TracksPrefs(mCategories);      mCategories->AddPage(w, w->GetName(), false, 0);
         w = new ImportExportPrefs(mCategories);mCategories->AddPage(w, w->GetName(), false, 0);
         w = new ExtImportPrefs(mCategories);   mCategories->AddPage(w, w->GetName(), false, 0);
         w = new ProjectsPrefs(mCategories);    mCategories->AddPage(w, w->GetName(), false, 0);
#if !defined(DISABLE_DYNAMIC_LOADING_FFMPEG) || !defined(DISABLE_DYNAMIC_LOADING_LAME)
         w = new LibraryPrefs(mCategories);     mCategories->AddPage(w, w->GetName(), false, 0);
#endif
         w = new SpectrumPrefs(mCategories);    mCategories->AddPage(w, w->GetName(), false, 0);
         w = new DirectoriesPrefs(mCategories); mCategories->AddPage(w, w->GetName(), false, 0);
         w = new WarningsPrefs(mCategories);    mCategories->AddPage(w, w->GetName(), false, 0);
         w = new EffectsPrefs(mCategories);     mCategories->AddPage(w, w->GetName(), false, 0);

#ifdef EXPERIMENTAL_THEME_PREFS
         w = new ThemePrefs(mCategories);       mCategories->AddPage(w, w->GetName(), false, 0);
#endif

//       w = new BatchPrefs(mCategories);       mCategories->AddPage(w, w->GetName(), false, 0);
         w = new KeyConfigPrefs(mCategories);   mCategories->AddPage(w, w->GetName(), false, 0);
         w = new MousePrefs(mCategories);       mCategories->AddPage(w, w->GetName(), false, 0);
#ifdef EXPERIMENTAL_MODULE_PREFS
         w = new ModulePrefs(mCategories);      mCategories->AddPage(w, w->GetName(), false, 0);
#endif
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

   S.AddStandardButtons(eOkButton | eCancelButton);

   /* long is signed, size_t is unsigned. On some platforms they are different
    * lengths as well. So we must check that the stored category is both > 0
    * and within the possible range of categories, making the first check on the
    * _signed_ value to avoid issues when converting an unsigned one.
    */
   size_t selected;
   long prefscat = gPrefs->Read(wxT("/Prefs/PrefsCategory"), 0L);
   if (prefscat > 0L )
      selected = prefscat; // only assign if number will fit
   else
      selected = 0;  // use 0 if value can't be assigned

   if (selected >= mCategories->GetPageCount())
      selected = 0;  // clamp to available range of tabs

   mCategories->SetSelection(selected);

#if defined(__WXGTK__)
   mCategories->GetTreeCtrl()->EnsureVisible(mCategories->GetTreeCtrl()->GetRootItem());
#endif

//   mCategories->SetSizeHints(-1, -1, 790, 600);  // 790 = 800 - (border * 2)
   Layout();
   Fit();
   wxSize sz = GetSize();

   wxASSERT_MSG(sz.x <= 800 && sz.y <= 600, wxT("Preferences dialog exceeds max size"));

   if (sz.x > 800) {
      sz.x = 800;
   }

   if (sz.y > 600) {
      sz.y = 600;
   }

   // Set the minimum height to be slightly bigger than default, as fix for bug 161.
   // The magic number 7 was determined by Ed's experimentation.
   // Frankly, this is a hack to work around a bug in wxTreebook, and
   // will have to be revisited if we add another category to mCategories.
   // JKC later added a category and 20 onto the 7.
   SetSizeHints(sz.x, sz.y + 7 + 20, 800, 600);

   // Center after all that resizing, but make sure it doesn't end up
   // off-screen
   CentreOnParent();
}

PrefsDialog::~PrefsDialog()
{
}

void PrefsDialog::OnCancel(wxCommandEvent & WXUNUSED(event))
{
   for (size_t i = 0; i < mCategories->GetPageCount(); i++) {
      ((PrefsPanel *) mCategories->GetPage(i))->Cancel();
   }

   EndModal(false);
}

void PrefsDialog::OnTreeKeyDown(wxTreeEvent & event)
{
   if(event.GetKeyCode() == WXK_RETURN)
      OnOK(event);
   else
      event.Skip(); // Ensure standard behavior when enter is not pressed
}

void PrefsDialog::OnOK(wxCommandEvent & WXUNUSED(event))
{
   // Validate all pages first
   for (size_t i = 0; i < mCategories->GetPageCount(); i++) {
      PrefsPanel *panel = (PrefsPanel *) mCategories->GetPage(i);

      // The dialog doesn't end until all the input is valid
      if (!panel->Validate()) {
         mCategories->SetSelection(i);
         return;
      }
   }

   // Now apply the changes
   for (size_t i = 0; i < mCategories->GetPageCount(); i++) {
      PrefsPanel *panel = (PrefsPanel *) mCategories->GetPage(i);

      panel->Apply();
   }

   gPrefs->Write(wxT("/Prefs/PrefsCategory"), (long)mCategories->GetSelection());
   gPrefs->Flush();

#if USE_PORTMIXER
   if (gAudioIO) {
      // We cannot have opened this dialog if gAudioIO->IsAudioTokenActive(),
      // per the setting of AudioIONotBusyFlag and AudioIOBusyFlag in
      // AudacityProject::GetUpdateFlags().
      // However, we can have an invalid audio token (so IsAudioTokenActive()
      // is false), but be monitoring.
      // If monitoring, have to stop the stream, so HandleDeviceChange() can work.
      // We could disable the Preferences command while monitoring, i.e.,
      // set AudioIONotBusyFlag/AudioIOBusyFlag according to monitoring, as well.
      // Instead allow it because unlike recording, for example, monitoring
      // is not clearly something that should prohibit opening prefs.
      // TODO: We *could* be smarter in this method and call HandleDeviceChange()
      // only when the device choices actually changed. True of lots of prefs!
      // As is, we always stop monitoring before handling the device change.
      if (gAudioIO->IsMonitoring())
      {
         gAudioIO->StopStream();
         while (gAudioIO->IsBusy())
            wxMilliSleep(100);
      }
      gAudioIO->HandleDeviceChange();
   }
#endif

   // LL:  wxMac can't handle recreating the menus when this dialog is still active,
   //      so AudacityProject::UpdatePrefs() or any of the routines it calls must
   //      not cause AudacityProject::RebuildMenuBar() to be executed.
   for (size_t i = 0; i < gAudacityProjects.GetCount(); i++) {
      gAudacityProjects[i]->UpdatePrefs();
   }

   gPrefs->Flush();
   EndModal(true);
}

void PrefsDialog::SelectPageByName(wxString pageName)
{
   size_t n = mCategories->GetPageCount();

   for (size_t i = 0; i < n; i++) {
      if (mCategories->GetPageText(i) == pageName) {
         mCategories->SetSelection(i);
         return;
      }
   }
}

void PrefsDialog::ShowTempDirPage()
{
   SelectPageByName(_("Directories"));
}
