/**********************************************************************

  Audacity: A Digital Audio Editor

  PrefsDialog.cpp

  Joshua Haberman
  James Crook

*******************************************************************//**

\class PrefsDialog
\brief Dialog that shows the current PrefsPanel in a tabbed divider.

*//*******************************************************************/

#include "../Audacity.h" // for USE_* macros
#include "PrefsDialog.h"

#include "../Experimental.h"

#include <wx/setup.h> // for wxUSE_* macros
#include <wx/defs.h>
#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/event.h>
#include <wx/font.h>
#include <wx/gdicmn.h>
#include <wx/intl.h>
#include <wx/listbox.h>
#include <wx/sizer.h>

#include <wx/listbook.h>

#include <wx/treebook.h>

#include "../AudioIO.h"
#include "../Project.h"
#include "../Prefs.h"
#include "../ShuttleGui.h"

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
#include "TracksBehaviorsPrefs.h"
#include "WarningsPrefs.h"
// #include "WaveformPrefs.h"
#include "WaveformSettings.h"
#include "ExtImportPrefs.h"

#ifdef EXPERIMENTAL_MIDI_OUT
#include "MidiIOPrefs.h"
#endif

#include "../Theme.h"
#include "../widgets/HelpSystem.h"

#if wxUSE_ACCESSIBILITY
#include "../widgets/WindowAccessible.h"
#endif

BEGIN_EVENT_TABLE(PrefsDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, PrefsDialog::OnOK)
   EVT_BUTTON(wxID_CANCEL, PrefsDialog::OnCancel)
   EVT_BUTTON(wxID_PREVIEW, PrefsDialog::OnPreview)
   EVT_BUTTON(wxID_HELP, PrefsDialog::OnHelp)
   EVT_TREE_KEY_DOWN(wxID_ANY, PrefsDialog::OnTreeKeyDown) // Handles key events when tree has focus
END_EVENT_TABLE()


class wxTreebookExt final : public wxTreebook
{
public:
   wxTreebookExt( wxWindow *parent,
      wxWindowID id, const wxString &titlePrefix)
      : wxTreebook( parent, id )
      , mTitlePrefix(titlePrefix)
   {;};
   ~wxTreebookExt(){;};
   int ChangeSelection(size_t n) override;
   int SetSelection(size_t n) override;
   const wxString mTitlePrefix;
};


int wxTreebookExt::ChangeSelection(size_t n) {
   int i = wxTreebook::ChangeSelection(n);
   wxString Temp = GetPageText( n );
   static_cast<wxDialog*>(GetParent())->SetTitle( Temp );
   static_cast<wxDialog*>(GetParent())->SetName( Temp );
   return i;
};

int wxTreebookExt::SetSelection(size_t n)
{
   int i = wxTreebook::SetSelection(n);
   wxString Temp = wxString(mTitlePrefix) + GetPageText( n );
   static_cast<wxDialog*>(GetParent())->SetTitle( Temp );
   static_cast<wxDialog*>(GetParent())->SetName( Temp );

   PrefsPanel *const panel = static_cast<PrefsPanel *>(GetPage(n));
   const bool showHelp = (!panel->HelpPageName().empty());
   const bool showPreview = panel->ShowsPreviewButton();
   wxWindow *const helpButton = wxWindow::FindWindowById(wxID_HELP, GetParent());
   wxWindow *const previewButton = wxWindow::FindWindowById(wxID_PREVIEW, GetParent());

   if (helpButton) {
      if (showHelp) {
         wxAcceleratorEntry entries[1];
#if defined(__WXMAC__)
         // Is there a standard shortcut on Mac?
#else
         entries[0].Set(wxACCEL_NORMAL, (int) WXK_F1, wxID_HELP);
#endif
         wxAcceleratorTable accel(1, entries);
         this->SetAcceleratorTable(accel);
      }
      else {
         this->SetAcceleratorTable(wxNullAcceleratorTable);
      }

      const bool changed = helpButton->Show(showHelp);
      if (changed)
         GetParent()->Layout();
   }

   if (previewButton) { // might still be NULL during population
      const bool changed = previewButton->Show(showPreview);
      if (changed)
         GetParent()->Layout();
   }

   return i;
}



PrefsDialog::Factories
&PrefsDialog::DefaultFactories()
{
   // To do, perhaps:  create this table by registration, without including each PrefsPanel
   // class... and thus allowing a plug-in protocol
   static DevicePrefsFactory devicePrefsFactory;
   static PlaybackPrefsFactory playbackPrefsFactory;
   static RecordingPrefsFactory recordingPrefsFactory;
#ifdef EXPERIMENTAL_MIDI_OUT
   static MidiIOPrefsFactory midiIOPrefsFactory;
#endif
   static QualityPrefsFactory qualityPrefsFactory;
   static GUIPrefsFactory guiPrefsFactory;
   static TracksPrefsFactory tracksPrefsFactory;
   static ImportExportPrefsFactory importExportPrefsFactory;
   static ExtImportPrefsFactory extImportPrefsFactory;
   static ProjectsPrefsFactory projectsPrefsFactory;
#if !defined(DISABLE_DYNAMIC_LOADING_FFMPEG) || !defined(DISABLE_DYNAMIC_LOADING_LAME)
   static LibraryPrefsFactory libraryPrefsFactory;
#endif
   // static WaveformPrefsFactory waveformPrefsFactory;
   static TracksBehaviorsPrefsFactory tracksBehaviorsPrefsFactory;
   static SpectrumPrefsFactory spectrumPrefsFactory;
   static DirectoriesPrefsFactory directoriesPrefsFactory;
   static WarningsPrefsFactory warningsPrefsFactory;
   static EffectsPrefsFactory effectsPrefsFactory;
#ifdef EXPERIMENTAL_THEME_PREFS
   static ThemePrefsFactory themePrefsFactory;
#endif
   // static BatchPrefsFactory batchPrefsFactory;
   static KeyConfigPrefsFactory keyConfigPrefsFactory;
   static MousePrefsFactory mousePrefsFactory;
#ifdef EXPERIMENTAL_MODULE_PREFS
   static ModulePrefsFactory modulePrefsFactory;
#endif

   static PrefsNode nodes[] = {
      &devicePrefsFactory,
      &playbackPrefsFactory,
      &recordingPrefsFactory,
#ifdef EXPERIMENTAL_MIDI_OUT
      &midiIOPrefsFactory,
#endif
      &qualityPrefsFactory,
      &guiPrefsFactory,

      // Group other page(s)
      PrefsNode(&tracksPrefsFactory, 2),
      // &waveformPrefsFactory,
      &tracksBehaviorsPrefsFactory,
      &spectrumPrefsFactory,

      // Group one other page
      PrefsNode(&importExportPrefsFactory, 1),
      &extImportPrefsFactory,

      &projectsPrefsFactory,
#if !defined(DISABLE_DYNAMIC_LOADING_FFMPEG) || !defined(DISABLE_DYNAMIC_LOADING_LAME)
      &libraryPrefsFactory,
#endif
      &directoriesPrefsFactory,
      &warningsPrefsFactory,
      &effectsPrefsFactory,
#ifdef EXPERIMENTAL_THEME_PREFS
      &themePrefsFactory,
#endif
      // &batchPrefsFactory,
      &keyConfigPrefsFactory,
      &mousePrefsFactory,
#ifdef EXPERIMENTAL_MODULE_PREFS
      &modulePrefsFactory,
#endif
   };

   static Factories factories(nodes, nodes + sizeof(nodes) / sizeof(nodes[0]));
   return factories;
}


PrefsDialog::PrefsDialog
  (wxWindow * parent, const wxString &titlePrefix, Factories &factories)
:  wxDialogWrapper(parent, wxID_ANY, wxString(_("Audacity Preferences")),
            wxDefaultPosition,
            wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
, mFactories(factories)
, mTitlePrefix(titlePrefix)
{
   wxASSERT(factories.size() > 0);
   const bool uniquePage = (factories.size() == 1);
   SetLayoutDirection(wxLayout_LeftToRight);

   ShuttleGui S(this, eIsCreating);

   S.StartVerticalLay(true);
   {
      wxASSERT(factories.size() > 0);
      if (!uniquePage) {
         mCategories = safenew wxTreebookExt(this, wxID_ANY, mTitlePrefix);
#if wxUSE_ACCESSIBILITY
         // so that name can be set on a standard control
         mCategories->GetTreeCtrl()->SetAccessible(safenew WindowAccessible(mCategories->GetTreeCtrl()));
#endif
         // RJH: Prevent NVDA from reading "treeCtrl"
         mCategories->GetTreeCtrl()->SetName(_("Category"));
         S.StartHorizontalLay(wxALIGN_LEFT | wxEXPAND, true);
         {
            S.Prop(1);
            S.AddWindow(mCategories, wxEXPAND);

            {
               typedef std::pair<int, int> IntPair;
               std::vector<IntPair> stack;
               int iPage = 0;
               for (Factories::const_iterator it = factories.begin(), end = factories.end();
                  it != end; ++it, ++iPage)
               {
                  const PrefsNode &node = *it;
                  PrefsPanelFactory &factory = *node.pFactory;
                  wxWindow *const w = factory(mCategories, wxID_ANY);
                  if (stack.empty())
                     // Parameters are: AddPage(page, name, IsSelected, imageId).
                     mCategories->AddPage(w, w->GetName(), false, 0);
                  else {
                     IntPair &top = *stack.rbegin();
                     mCategories->InsertSubPage(top.first, w, w->GetName(), false, 0);
                     if (--top.second == 0) {
                        // Expand all nodes before the layout calculation
                        mCategories->ExpandNode(top.first, true);
                        stack.pop_back();
                     }
                  }
                  if (node.nChildren > 0)
                     stack.push_back(IntPair(iPage, node.nChildren));
               }
            }
         }
         S.EndHorizontalLay();
      }
      else {
         // TODO: Look into getting rid of mUniquePage and instead
         // adding into mCategories, so there is just one page in mCategories.
         // And then hiding the treebook.

         // Unique page, don't show the factory
         const PrefsNode &node = factories[0];
         PrefsPanelFactory &factory = *node.pFactory;
         mUniquePage = factory(this, wxID_ANY);
         wxWindow * uniquePageWindow = S.Prop(1).AddWindow(mUniquePage, wxEXPAND);
         // We're not in the wxTreebook, so add the accelerator here
         wxAcceleratorEntry entries[1];
#if defined(__WXMAC__)
         // Is there a standard shortcut on Mac?
#else
         entries[0].Set(wxACCEL_NORMAL, (int) WXK_F1, wxID_HELP);
#endif
         wxAcceleratorTable accel(1, entries);
         uniquePageWindow->SetAcceleratorTable(accel);
      }
   }
   S.EndVerticalLay();

   S.AddStandardButtons(eOkButton | eCancelButton | ePreviewButton | eHelpButton);
   static_cast<wxButton*>(wxWindow::FindWindowById(wxID_OK, this))->SetDefault();

   if (mUniquePage && !mUniquePage->ShowsPreviewButton()) {
      wxWindow *const previewButton =
         wxWindow::FindWindowById(wxID_PREVIEW, GetParent());
      previewButton->Show(false);
   }

#if defined(__WXGTK__)
   if (mCategories)
      mCategories->GetTreeCtrl()->EnsureVisible(mCategories->GetTreeCtrl()->GetRootItem());
#endif

//   mCategories->SetSizeHints(-1, -1, 790, 600);  // 790 = 800 - (border * 2)
   Layout();
   Fit();
   wxSize sz = GetSize();

   // Collapse nodes only after layout so the tree is wide enough
   if (mCategories)
   {
      int iPage = 0;
      for (Factories::const_iterator it = factories.begin(), end = factories.end();
         it != end; ++it, ++iPage)
         mCategories->ExpandNode(iPage, it->expanded);
   }

   // This ASSERT was originally used to limit us to 800 x 600.
   // However, the range of screen sizes and dpi of modern (2018) displays
   // makes pixel dimensions an inadequate measure of usability, so
   // now we only ASSERT that preferences will fit in the client display
   // rectangle of the developer / tester's monitor.
   // Use scrollers when necessary to ensure that preference pages will
   // be fully visible.
   wxRect screenRect(wxGetClientDisplayRect());
   wxASSERT_MSG(sz.x <= screenRect.width && sz.y <= screenRect.height, wxT("Preferences dialog exceeds max size"));

   sz.DecTo(screenRect.GetSize());

   if( !mUniquePage ){
      int prefWidth, prefHeight;
      gPrefs->Read(wxT("/Prefs/Width"), &prefWidth, sz.x);
      gPrefs->Read(wxT("/Prefs/Height"), &prefHeight, wxMax(480,sz.y));

      wxSize prefSize = wxSize(prefWidth, prefHeight);
      prefSize.DecTo(screenRect.GetSize());
      SetSize(prefSize);
      InvalidateBestSize();
      Layout();
   }
   SetMinSize(sz);

   // Center after all that resizing, but make sure it doesn't end up
   // off-screen
   CentreOnParent();
}

PrefsDialog::~PrefsDialog()
{
}

int PrefsDialog::ShowModal()
{
   if (mCategories) {
      /* long is signed, size_t is unsigned. On some platforms they are different
       * lengths as well. So we must check that the stored category is both > 0
       * and within the possible range of categories, making the first check on the
       * _signed_ value to avoid issues when converting an unsigned one.
       */
      long selected = GetPreferredPage();
      if (selected < 0 || size_t(selected) >= mCategories->GetPageCount())
         selected = 0;  // clamp to available range of tabs
      mCategories->SetSelection(selected);
   }
   else {
      wxString Temp = mTitlePrefix + mUniquePage->GetLabel();
      SetTitle(Temp);
      SetName(Temp);
   }

   return wxDialogWrapper::ShowModal();
}

void PrefsDialog::OnCancel(wxCommandEvent & WXUNUSED(event))
{
   RecordExpansionState();

   if (mCategories) {
      for (size_t i = 0; i < mCategories->GetPageCount(); i++) {
         ((PrefsPanel *)mCategories->GetPage(i))->Cancel();
      }
   }
   else
      mUniquePage->Cancel();

   // Remember modified dialog size, even if cancelling.
   if( !mUniquePage ){
      wxSize sz = GetSize();
      gPrefs->Write(wxT("/Prefs/Width"), sz.x);
      gPrefs->Write(wxT("/Prefs/Height"), sz.y);
   }
   gPrefs->Flush();

   EndModal(false);
}

PrefsPanel * PrefsDialog::GetCurrentPanel()
{
   if( mCategories) 
      return static_cast<PrefsPanel*>(mCategories->GetCurrentPage());
   else
   {
      wxASSERT( mUniquePage );
      return mUniquePage;
   }
}

void PrefsDialog::OnPreview(wxCommandEvent & WXUNUSED(event))
{
   GetCurrentPanel()->Preview();
}

void PrefsDialog::OnHelp(wxCommandEvent & WXUNUSED(event))
{
   wxString page = GetCurrentPanel()->HelpPageName();
   HelpSystem::ShowHelp(this, page, true);
}

void PrefsDialog::ShuttleAll( ShuttleGui & S)
{
   // Validate all pages first
   if (mCategories) {
      for (size_t i = 0; i < mCategories->GetPageCount(); i++) {
         S.ResetId();
         PrefsPanel *panel = (PrefsPanel *)mCategories->GetPage(i);
         panel->PopulateOrExchange( S );
      }
   }
   else
   {
      S.ResetId();
      mUniquePage->PopulateOrExchange( S );
   }
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
   RecordExpansionState();

   // Validate all pages first
   if (mCategories) {
      for (size_t i = 0; i < mCategories->GetPageCount(); i++) {
         PrefsPanel *panel = (PrefsPanel *)mCategories->GetPage(i);

         // The dialog doesn't end until all the input is valid
         if (!panel->Validate()) {
            mCategories->SetSelection(i);
            return;
         }
      }
   }
   else {
      if (!mUniquePage->Validate())
         return;
   }

   // flush now so toolbars will know their position.
   gPrefs->Flush();
   if (mCategories) {
      // Now apply the changes
      // Reverse order - so Track Name is updated before language change
      // A workaround for Bug 1661
      for (int i = (int)mCategories->GetPageCount()-1; i>= 0; i--) {
         PrefsPanel *panel = (PrefsPanel *)mCategories->GetPage(i);

         panel->Preview();
         panel->Commit();
      }
   }
   else {
      mUniquePage->Preview();
      mUniquePage->Commit();
   }

   if( !mUniquePage ){
      wxSize sz = GetSize();
      gPrefs->Write(wxT("/Prefs/Width"), sz.x);
      gPrefs->Write(wxT("/Prefs/Height"), sz.y);
   }
   gPrefs->Flush();

   // Reads preference /GUI/Theme
   theTheme.LoadPreferredTheme();
   theTheme.ApplyUpdatedImages();

   SavePreferredPage();

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
   //      not cause MenuCreator::RebuildMenuBar() to be executed.
   for (size_t i = 0; i < gAudacityProjects.size(); i++) {
      gAudacityProjects[i]->UpdatePrefs();
   }

   WaveformSettings::defaults().LoadPrefs();
   SpectrogramSettings::defaults().LoadPrefs();

   if( IsModal() )
      EndModal(true);
   else
      Destroy();
}

void PrefsDialog::SelectPageByName(const wxString &pageName)
{
   if (mCategories) {
      size_t n = mCategories->GetPageCount();

      for (size_t i = 0; i < n; i++) {
         if (mCategories->GetPageText(i) == pageName) {
            mCategories->SetSelection(i);
            return;
         }
      }
   }
}

int PrefsDialog::GetSelectedPage() const
{
   if (mCategories)
      return mCategories->GetSelection();
   else
      return 0;
}

GlobalPrefsDialog::GlobalPrefsDialog(wxWindow * parent, Factories &factories)
   : PrefsDialog(parent, _("Preferences: "), factories)
{
}

GlobalPrefsDialog::~GlobalPrefsDialog()
{
}

long GlobalPrefsDialog::GetPreferredPage()
{
   long prefscat = gPrefs->Read(wxT("/Prefs/PrefsCategory"), 0L);
   return prefscat;
}

void GlobalPrefsDialog::SavePreferredPage()
{
   gPrefs->Write(wxT("/Prefs/PrefsCategory"), (long)GetSelectedPage());
   gPrefs->Flush();
}

void PrefsDialog::RecordExpansionState()
{
   // Remember expansion state of the tree control
   if (mCategories)
   {
      int iPage = 0;
      for (Factories::iterator it = mFactories.begin(), end = mFactories.end();
         it != end; ++it, ++iPage)
         it->expanded = mCategories->IsNodeExpanded(iPage);
   }
   else
      mFactories[0].expanded = true;
}

PrefsPanel::~PrefsPanel()
{
}

void PrefsPanel::Cancel()
{
}

bool PrefsPanel::ShowsPreviewButton()
{
   return false;
}

wxString PrefsPanel::HelpPageName()
{
   return wxEmptyString;
}
