/**********************************************************************

  Audacity: A Digital Audio Editor

  PrefsDialog.cpp

  Joshua Haberman
  James Crook

*******************************************************************//**

\class PrefsDialog
\brief Dialog that shows the current PrefsPanel in a tabbed divider.

*//*******************************************************************/


#include "PrefsDialog.h"

#include <wx/app.h>
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
#include <wx/treectrl.h>

#include "../AudioIOBase.h"
#include "Prefs.h"
#include "../ShuttleGui.h"
#include "../commands/CommandManager.h"

#include "PrefsPanel.h"

#include "../widgets/HelpSystem.h"

#if wxUSE_ACCESSIBILITY
#include "../widgets/WindowAccessible.h"
#endif


#if wxUSE_ACCESSIBILITY

#ifndef __WXMAC__

// Just an alias
using TreeCtrlAx = WindowAccessible;

#else

// utility functions
namespace {
   template< typename Result, typename Fn >
   Result VisitItems( const wxTreeCtrl &ctrl, Fn fn )
   {
      // Do preorder visit of items in the tree until satisfying a test
      std::vector< wxTreeItemId > stack;
      stack.push_back( ctrl.GetRootItem() );
      unsigned position = 0;
      while ( !stack.empty() ) {
         auto itemId = stack.back();
         auto pair = fn( itemId, position );
         if ( pair.first )
            return pair.second;

         wxTreeItemIdValue cookie;
         auto childId = ctrl.GetFirstChild( itemId, cookie );
         if ( childId )
            stack.push_back( childId );
         else do {
            auto &id = stack.back();
            if ( !!( id = ctrl.GetNextSibling( id ) ) )
               break;
         } while ( stack.pop_back(), !stack.empty() );
         
         ++position;
      }
      return {};
   }
   
   unsigned FindItemPosition( const wxTreeCtrl &ctrl, wxTreeItemId id )
   {
      // Return the 1-based count of the item's position in the pre-order
      // visit of the items in the tree (not counting the root item which we
      // assume is a dummy that never matches id)
      return VisitItems<unsigned>( ctrl,
         [=]( wxTreeItemId itemId, unsigned position ){
            return std::make_pair( itemId == id, position ); } );
   }
   
   wxTreeItemId FindItem( const wxTreeCtrl &ctrl, int nn )
   {
      // The inverse of the function above
      return VisitItems<wxTreeItemId>( ctrl,
         [=]( wxTreeItemId itemId, unsigned position ){
            return std::make_pair( nn == position, itemId ); } );
   }
}

// Define a custom class
class TreeCtrlAx final
   : public WindowAccessible
{
public:
   TreeCtrlAx(wxTreeCtrl * ctrl);
   virtual ~ TreeCtrlAx();

   wxAccStatus GetChild(int childId, wxAccessible** child) override;

   wxAccStatus GetChildCount(int* childCount) override;

   wxAccStatus GetDefaultAction(int childId, wxString *actionName) override;

   // Returns the description for this object or a child.
   wxAccStatus GetDescription(int childId, wxString *description) override;

   // Gets the window with the keyboard focus.
   // If childId is 0 and child is NULL, no object in
   // this subhierarchy has the focus.
   // If this object has the focus, child should be 'this'.
   wxAccStatus GetFocus(int *childId, wxAccessible **child) override;

   // Returns help text for this object or a child, similar to tooltip text.
   wxAccStatus GetHelpText(int childId, wxString *helpText) override;

   // Returns the keyboard shortcut for this object or child.
   // Return e.g. ALT+K
   wxAccStatus GetKeyboardShortcut(int childId, wxString *shortcut) override;

   // Returns the rectangle for this object (id = 0) or a child element (id > 0).
   // rect is in screen coordinates.
   wxAccStatus GetLocation(wxRect& rect, int elementId) override;

   // Gets the name of the specified object.
   wxAccStatus GetName(int childId, wxString *name) override;

   // Returns a role constant.
   wxAccStatus GetRole(int childId, wxAccRole *role) override;

   // Gets a variant representing the selected children
   // of this object.
   // Acceptable values:
   // - a null variant (IsNull() returns TRUE)
   // - a list variant (GetType() == wxT("list"))
   // - an integer representing the selected child element,
   //   or 0 if this object is selected (GetType() == wxT("long"))
   // - a "void*" pointer to a wxAccessible child object
   //wxAccStatus GetSelections(wxVariant *selections) override;
   // leave unimplemented

   // Returns a state constant.
   wxAccStatus GetState(int childId, long* state) override;

   // Returns a localized string representing the value for the object
   // or child.
   wxAccStatus GetValue(int childId, wxString* strValue) override;

   // Navigates from fromId to toId/toObject
   // wxAccStatus Navigate(wxNavDir navDir, int fromId, int* toId, wxAccessible** toObject) override;

   // Modify focus or selection
   wxAccStatus Select(int childId, wxAccSelectionFlags selectFlags) override;

private:
   wxTreeCtrl *GetCtrl() { return static_cast<wxTreeCtrl*>( GetWindow() ); }
};

TreeCtrlAx::TreeCtrlAx( wxTreeCtrl *ctrl )
: WindowAccessible{ ctrl }
{
}

TreeCtrlAx::~TreeCtrlAx() = default;

wxAccStatus TreeCtrlAx::GetChild( int childId, wxAccessible** child )
{
   if( childId == wxACC_SELF )
   {
      *child = this;
   }
   else
   {
      *child = NULL;
   }

   return wxACC_OK;
}

wxAccStatus TreeCtrlAx::GetChildCount(int* childCount)
{
   auto ctrl = GetCtrl();
   if (!ctrl)
      return wxACC_FAIL;

   *childCount = ctrl->GetCount();
   return wxACC_OK;
}

wxAccStatus TreeCtrlAx::GetDefaultAction(int WXUNUSED(childId), wxString* actionName)
{
   actionName->clear();

   return wxACC_OK;
}

// Returns the description for this object or a child.
wxAccStatus TreeCtrlAx::GetDescription( int WXUNUSED(childId), wxString *description )
{
   description->clear();

   return wxACC_OK;
}

// This isn't really used yet by wxWidgets as patched by Audacity for
// Mac accessibility, as of Audacity 2.3.2, but here it is anyway, keeping the
// analogy with TrackPanelAx
wxAccStatus TreeCtrlAx::GetFocus( int *childId, wxAccessible **child )
{
   auto ctrl = GetCtrl();
   if (!ctrl)
      return wxACC_FAIL;

   auto item = ctrl->GetFocusedItem();
   auto id = FindItemPosition( *ctrl, item );
   *childId = id;
   *child = nullptr;
   return wxACC_OK;
}

// Returns help text for this object or a child, similar to tooltip text.
wxAccStatus TreeCtrlAx::GetHelpText( int WXUNUSED(childId), wxString *helpText )
{
   helpText->clear();

   return wxACC_OK;
}

// Returns the keyboard shortcut for this object or child.
// Return e.g. ALT+K
wxAccStatus TreeCtrlAx::GetKeyboardShortcut( int WXUNUSED(childId), wxString *shortcut )
{
   shortcut->clear();

   return wxACC_OK;
}

wxAccStatus TreeCtrlAx::GetLocation( wxRect& rect, int elementId )
{
   auto ctrl = GetCtrl();
   if (!ctrl)
      return wxACC_FAIL;

   if (elementId == wxACC_SELF)
      rect = ctrl->GetRect();
   else {
      auto item = FindItem( *ctrl, elementId );
      if ( !( item && ctrl->GetBoundingRect( item, rect ) ) )
         return wxACC_INVALID_ARG;
   }
   rect.SetPosition( ctrl->GetParent()->ClientToScreen( rect.GetPosition() ) );
   return wxACC_OK;
}

wxAccStatus TreeCtrlAx::GetName(int childId, wxString* name)
{
   if ( childId == wxACC_SELF )
      return WindowAccessible::GetName( childId, name );
   else {
      auto ctrl = GetCtrl();
      if (!ctrl)
         return wxACC_FAIL;

      auto item = FindItem( *ctrl, childId );
      if ( item ) {
         *name = ctrl->GetItemText( item );
         return wxACC_OK;
      }
      else
         return wxACC_INVALID_ARG;
   }
}

wxAccStatus TreeCtrlAx::GetRole( int childId, wxAccRole* role )
{
   // Not sure if this correct, but it is analogous with what we use in
   // TrackPanel
   *role =
      childId == wxACC_SELF ? wxROLE_SYSTEM_PANE : wxROLE_SYSTEM_STATICTEXT;
   return wxACC_OK;
}

// Returns a state constant.
wxAccStatus TreeCtrlAx::GetState(int childId, long* state)
{
   auto ctrl = GetCtrl();
   if (!ctrl)
      return wxACC_FAIL;

   *state = wxACC_STATE_SYSTEM_FOCUSABLE | wxACC_STATE_SYSTEM_SELECTABLE;

   if ( childId != wxACC_SELF ) {
      auto item = FindItem( *ctrl, childId );
      if (item) {
         if( item == ctrl->GetFocusedItem() )
            *state |= wxACC_STATE_SYSTEM_FOCUSED;

         if( item == ctrl->GetSelection() )
            *state |= wxACC_STATE_SYSTEM_SELECTED;
      }
   }

   return wxACC_OK;
}

// Returns a localized string representing the value for the object
// or child.
wxAccStatus TreeCtrlAx::GetValue(int childId, wxString* strValue)
{
   *strValue = wxString{};
   return wxACC_OK;
}

//wxAccStatus TreeCtrlAx::Navigate(
//   wxNavDir navDir, int fromId, int* toId, wxAccessible** toObject)
//{
//   to do
//}

// Modify focus or selection
wxAccStatus TreeCtrlAx::Select(int childId, wxAccSelectionFlags selectFlags)
{
   auto ctrl = GetCtrl();
   if (!ctrl)
      return wxACC_FAIL;

   if (childId != wxACC_SELF) {
      int childCount;
      GetChildCount( &childCount );
      if (childId > childCount)
           return wxACC_FAIL;

      auto item = FindItem( *ctrl, childId );
      if ( item ) {
         if (selectFlags == wxACC_SEL_TAKEFOCUS)
            ctrl->SetFocusedItem( item );
         else if (selectFlags == wxACC_SEL_TAKESELECTION)
            ctrl->SelectItem( item );
         else
            return wxACC_NOT_IMPLEMENTED;
         return wxACC_OK;
      }
   }

   return wxACC_NOT_IMPLEMENTED;
}

#endif

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
      wxWindowID id, const TranslatableString &titlePrefix)
      : wxTreebook( parent, id )
      , mTitlePrefix(titlePrefix)
   {;};
   ~wxTreebookExt(){;};
   int ChangeSelection(size_t n) override;
   int SetSelection(size_t n) override;
   const TranslatableString mTitlePrefix;
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
   auto Temp = mTitlePrefix.Translation() + wxT(" ") + GetPageText( n );
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

PrefsDialog::PrefsDialog(
   wxWindow * parent, AudacityProject *pProject,
   const TranslatableString &titlePrefix,
   PrefsPanel::Factories &factories)
:  wxDialogWrapper(parent, wxID_ANY, XO("Audacity Preferences"),
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
         mCategories = safenew wxTreebookExt(S.GetParent(), wxID_ANY, mTitlePrefix);
#if wxUSE_ACCESSIBILITY
         // so that name can be set on a standard control
         mCategories->GetTreeCtrl()->SetAccessible(
            safenew TreeCtrlAx(mCategories->GetTreeCtrl()));
#endif
         // RJH: Prevent NVDA from reading "treeCtrl"
         mCategories->GetTreeCtrl()->SetName(_("Category"));
         S.StartHorizontalLay(wxALIGN_LEFT | wxEXPAND, true);
         {
            S.Prop(1)
               .Position(wxEXPAND)
               .AddWindow(mCategories);

            {
               typedef std::pair<int, int> IntPair;
               std::vector<IntPair> stack;
               int iPage = 0;
               for (auto it = factories.begin(), end = factories.end();
                  it != end; ++it, ++iPage)
               {
                  const auto &node = *it;
                  const auto &factory = node.factory;
                  wxWindow *const w = factory(mCategories, wxID_ANY, pProject);
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
         const auto &node = factories[0];
         const auto &factory = node.factory;
         mUniquePage = factory(S.GetParent(), wxID_ANY, pProject);
         wxWindow * uniquePageWindow = S.Prop(1)
            .Position(wxEXPAND)
            .AddWindow(mUniquePage);
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

   if (mUniquePage && !mUniquePage->ShowsPreviewButton()) {
      wxWindow *const previewButton =
         wxWindow::FindWindowById(wxID_PREVIEW, GetParent());
      previewButton->Show(false);
   }

#if defined(__WXGTK__)
   if (mCategories)
      mCategories->GetTreeCtrl()->EnsureVisible(mCategories->GetTreeCtrl()->GetRootItem());
#endif

//   mCategories->SetMaxSize({ 790, 600 });  // 790 = 800 - (border * 2)
   Layout();
   Fit();
   wxSize sz = GetSize();

   // Collapse nodes only after layout so the tree is wide enough
   if (mCategories)
   {
      int iPage = 0;
      for (auto it = factories.begin(), end = factories.end();
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
      auto Temp = mTitlePrefix;
      Temp.Join( Verbatim( mUniquePage->GetLabel() ), wxT(" ") );
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
   const auto &page = GetCurrentPanel()->HelpPageName();
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

   SavePreferredPage();

#if USE_PORTMIXER
   auto gAudioIO = AudioIOBase::Get();
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

   // PRL:  Is the following concern still valid, now that prefs update is
   //      handled instead by delayed event processing?

   // LL:  wxMac can't handle recreating the menus when this dialog is still active,
   //      so AudacityProject::UpdatePrefs() or any of the routines it calls must
   //      not cause MenuCreator::RebuildMenuBar() to be executed.

   PrefsListener::Broadcast();

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
            // This covers the case, when ShowModal is called 
            // after selecting the page.
            // ShowModal will select the page previously used by 
            // user
            SavePreferredPage();
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

GlobalPrefsDialog::GlobalPrefsDialog(
   wxWindow * parent, AudacityProject *pProject,
   PrefsPanel::Factories &factories)
   : PrefsDialog(parent, pProject, XO("Preferences:"), factories)
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
      for (auto it = mFactories.begin(), end = mFactories.end();
         it != end; ++it, ++iPage)
         it->expanded = mCategories->IsNodeExpanded(iPage);
   }
   else
      mFactories[0].expanded = true;
}

#include <wx/frame.h>
#include "../Menus.h"
#include "../Project.h"

void DoReloadPreferences( AudacityProject &project )
{
   PreferenceInitializer::ReinitializeAll();

   {
      GlobalPrefsDialog dialog(
         &GetProjectFrame( project ) /* parent */, &project );
      wxCommandEvent Evt;
      //dialog.Show();
      dialog.OnOK(Evt);
   }

   // LL:  Moved from PrefsDialog since wxWidgets on OSX can't deal with
   //      rebuilding the menus while the PrefsDialog is still in the modal
   //      state.
   for (auto p : AllProjects{}) {
      MenuManager::Get(*p).RebuildMenuBar(*p);
// TODO: The comment below suggests this workaround is obsolete.
#if defined(__WXGTK__)
      // Workaround for:
      //
      //   http://bugzilla.audacityteam.org/show_bug.cgi?id=458
      //
      // This workaround should be removed when Audacity updates to wxWidgets
      // 3.x which has a fix.
      auto &window = GetProjectFrame( *p );
      wxRect r = window.GetRect();
      window.SetSize(wxSize(1,1));
      window.SetSize(r.GetSize());
#endif
   }
}
