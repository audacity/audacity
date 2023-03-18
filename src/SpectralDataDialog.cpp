/**********************************************************************

  Audacity: A Digital Audio Editor

  SpectralDataDialog.cpp

  Edward Hui

*******************************************************************//*!

\class SpectralDataDialog
\brief Provides UI for spectral editing and parameters adjustments

*//*******************************************************************/

#ifdef EXPERIMENTAL_BRUSH_TOOL

#include <type_traits>

#include <wx/app.h>
#include <wx/defs.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/dialog.h>
#include <wx/event.h>
#include <wx/frame.h>
#include <wx/imaglist.h>
#include <wx/intl.h>
#include <wx/listctrl.h>
#include <wx/settings.h>
#include <wx/spinctrl.h>
#include <wx/statline.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/tglbtn.h>

#include "../images/Arrow.xpm"
#include "../images/Empty9x16.xpm"

#include "AllThemeResources.h"
#include "AudioIO.h"
#include "ClientData.h"
#include "Clipboard.h"
#include "commands/CommandContext.h"
#include "commands/CommandManager.h"
#include "Menus.h"
#include "UndoManager.h"
#include "Prefs.h"
#include "Project.h"
#include "ProjectHistory.h"
#include "ProjectSettings.h"
#include "ProjectWindows.h"
#include "ShuttleGui.h"
#include "SpectralDataManager.h"
#include "tracks/playabletrack/wavetrack/ui/SpectrumView.h"
#include "Theme.h"
#include "TrackPanel.h"
#include "WaveTrack.h"

#include "commands/CommandManagerWindowClasses.h"
#include "AudacityMessageBox.h"
#include "wxPanelWrapper.h" // to inherit

// If defined, make a checkbox for smart selection of the fundamental
// independently of overtones
#undef SMART_CHECKBOX

enum {
   ID_BRUSH_BUTTON = 10000,
#ifdef SMART_CHECKBOX
   ID_CHECKBOX_SMART,
#endif
   ID_CHECKBOX_OVERTONES,
   ID_SLIDER_BRUSH_SIZE
};

class wxButton;
class wxCheckBox;
class wxListCtrl;
class wxListEvent;
class wxSpinCtrl;
class wxTextCtrl;
class AudacityProject;
class ShuttleGui;
class UndoManager;

class SpectralDataDialog final : public wxDialogWrapper,
      public PrefsListener,
      public ClientData::Base,
      public TopLevelKeystrokeHandlingWindow
{

      public:
         static SpectralDataDialog &Get(AudacityProject &project);
         static SpectralDataDialog *Find(AudacityProject *pProject);

         explicit SpectralDataDialog(AudacityProject &parent);

         void UpdateDisplayForClipboard(ClipboardChangeMessage);
         void UpdateDisplay(UndoRedoMessage);
         void DoUpdateDisplay();
         void UpdateControls( bool active );

         bool Show( bool show = true ) override;

      private:
         void Populate(ShuttleGui & S);

         void OnAudioIO(AudioIOEvent ev);
         void DoUpdate();

         void OnCloseWindow(wxCloseEvent &event);
         void OnApply(wxCommandEvent &event);
         void OnBrushButton(wxCommandEvent &event);
         void OnBrushSizeSlider(wxCommandEvent &event);
         void OnCheckSmartSelection(wxCommandEvent &event);
         void OnCheckOvertones(wxCommandEvent &event);

         // PrefsListener implementation
         void UpdatePrefs() override;

         Observer::Subscription mAudioIOSubscription
            , mUndoSubscription
            , mClipboardSubscription
         ;
         AudacityProject   &mProject;
         wxToggleButton *mBrushButton = nullptr;
         bool              mAudioIOBusy { false };

      public:
         void DoToolChanged();

         DECLARE_EVENT_TABLE()
};

class AUDACITY_DLL_API SpectralDataDialogWorker final
      : public ClientData::Base{
public:
   explicit SpectralDataDialogWorker( AudacityProject &project );
   ~SpectralDataDialogWorker();

   void OnToolChanged(ProjectSettingsEvent);
   void OnIdle(wxIdleEvent &evt);
private:
   Observer::Subscription mSettingsSubscription;
   AudacityProject &mProject;
   unsigned mPrevNViews = 0;
};

BEGIN_EVENT_TABLE(SpectralDataDialog, wxDialogWrapper)
   EVT_TOGGLEBUTTON(ID_BRUSH_BUTTON, SpectralDataDialog::OnBrushButton)
 #ifdef SMART_CHECKBOX
   EVT_CHECKBOX(ID_CHECKBOX_SMART, SpectralDataDialog::OnCheckSmartSelection)
#endif
   EVT_CHECKBOX(ID_CHECKBOX_OVERTONES, SpectralDataDialog::OnCheckOvertones)
   EVT_CLOSE(SpectralDataDialog::OnCloseWindow)
   EVT_SLIDER(ID_SLIDER_BRUSH_SIZE, SpectralDataDialog::OnBrushSizeSlider)
END_EVENT_TABLE()

#define Title XO("Spectral Data Control Panel")

SpectralDataDialog::SpectralDataDialog(AudacityProject &parent)
   : mProject(parent)
   , wxDialogWrapper(FindProjectFrame( &parent ), wxID_ANY, Title,
                         wxDefaultPosition, wxDefaultSize,
                         wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER )

{
   SetName();
   //------------------------- Main section --------------------
   // Construct the GUI.
   ShuttleGui S(this, eIsCreating);
   Populate(S);
   CentreOnParent();

   mAudioIOSubscription = AudioIO::Get()
      ->Subscribe(*this, &SpectralDataDialog::OnAudioIO);

   mClipboardSubscription = Clipboard::Get()
      .Subscribe(*this, &::SpectralDataDialog::UpdateDisplayForClipboard);

   mUndoSubscription = UndoManager::Get(parent)
      .Subscribe(*this, &SpectralDataDialog::UpdateDisplay);

   DoToolChanged();
}

static const AttachedWindows::RegisteredFactory key{
      []( AudacityProject &project ){
         return safenew SpectralDataDialog( project );
      }
};

static wxToggleButton *MakeButton(wxWindow *pParent)
{
   auto button = safenew wxBitmapToggleButton{
      pParent, ID_BRUSH_BUTTON, theTheme.Bitmap(bmpSpectralBrush) };
   // Name isn't shown but may be pronounced by a screen reader
   button->SetName(XO("Brush Tool").Translation());
   return button;
}

void SpectralDataDialog::Populate(ShuttleGui & S)
{
   mBrushButton = MakeButton(this);

   S.StartVerticalLay(true);
   S.SetBorder(10);
   {
      S.AddVariableText(XO("Spectral Brush"));
      S.AddWindow(mBrushButton, wxALIGN_LEFT);

      S.AddVariableText(XO("Brush radius"));
      S.Id(ID_SLIDER_BRUSH_SIZE)
            .Style(wxSL_HORIZONTAL)
            .Name(XO("Custom brush size"))
            .AddSlider( {}, 5, 10, 1);

      S.AddWindow(safenew wxStaticLine{ S.GetParent() });
      
      S.Id(ID_CHECKBOX_OVERTONES)
         .AddCheckBox(
            XXO("Auto-select overtones (beta)"),
            false);

#ifdef SMART_CHECKBOX
      S.Id(ID_CHECKBOX_SMART)
            .AddCheckBox(XXO("Enable smart selection"), false);
#endif

      S.AddVariableText(
         XO("Select the fundamental frequency\n"
            "and release the mouse"));
   }
   S.EndVerticalLay();
   // ----------------------- End of main section --------------

   Layout();
   Fit();
   SetMinSize(GetSize());
}

void SpectralDataDialog::OnAudioIO(AudioIOEvent ev)
{
   if (ev.type != AudioIOEvent::MONITOR)
      mAudioIOBusy = ev.on;
}

void SpectralDataDialog::UpdateDisplayForClipboard(ClipboardChangeMessage)
{
   DoUpdateDisplay();
}

void SpectralDataDialog::UpdateDisplay(UndoRedoMessage message)
{
   switch (message.type) {
   case UndoRedoMessage::Pushed:
   case UndoRedoMessage::Modified:
   case UndoRedoMessage::UndoOrRedo:
   case UndoRedoMessage::Reset:
   case UndoRedoMessage::Purge:
      break;
   default:
      return;
   }
   DoUpdateDisplay();
}

void SpectralDataDialog::DoUpdateDisplay()
{
   if(IsShown())
      DoUpdate();
}

void SpectralDataDialog::UpdateControls( bool active )
{
   if (mBrushButton)
      mBrushButton->SetValue(active);
}

static bool IsBrushToolActive(AudacityProject &project)
{
   return ProjectSettings::Get(project).GetTool() == ToolCodes::brushTool;
}

bool SpectralDataDialog::Show( bool show )
{
   if ( show && !IsShown())
      DoUpdate();
   if ( IsShown() && !show && IsBrushToolActive(mProject) )
      ProjectSettings::Get(mProject).SetTool(ToolCodes::selectTool);
   auto result = wxDialogWrapper::Show( show );
   CommandManager::Get( mProject ).UpdateCheckmarks( mProject );
   return result;
}

void SpectralDataDialog::DoUpdate()
{
}

void SpectralDataDialog::OnCloseWindow(wxCloseEvent & WXUNUSED(event))
{
   this->Show(false);
}

// PrefsListener implementation
void SpectralDataDialog::UpdatePrefs()
{
   bool shown = IsShown();
   if (shown) {
      Show(false);
   }

   SetSizer(nullptr);
   DestroyChildren();

   SetTitle(Title);
   ShuttleGui S(this, eIsCreating);
   Populate(S);
   DoToolChanged();

   if (shown) {
      Show(true);
   }
}

void SpectralDataDialog::OnBrushButton(wxCommandEvent &event) {
   if (mBrushButton->GetValue())
      ProjectSettings::Get(mProject).SetTool(ToolCodes::brushTool);
   else
      // Don't stay up
      mBrushButton->SetValue(true);
}

static const AudacityProject::AttachedObjects::RegisteredFactory sSpectralWorkerKey{
   []( AudacityProject &project ){
      return std::make_shared< SpectralDataDialogWorker >( project );
   }
};

AttachedWindows::RegisteredFactory sSpectralDataDialogKey{
      []( AudacityProject &parent ) -> wxWeakRef< wxWindow > {
         return safenew SpectralDataDialog( parent );
      }
};

SpectralDataDialog &SpectralDataDialog::Get( AudacityProject &project )
{
   // Ensure existence of the dialog
   return static_cast<SpectralDataDialog&>(
      GetAttachedWindows(project).Get( sSpectralDataDialogKey ) );
}

SpectralDataDialog *SpectralDataDialog::Find( AudacityProject *pProject )
{
   // Return a pointer to the dialog only if it exists
   return pProject
      ? static_cast<SpectralDataDialog*>(
         GetAttachedWindows(*pProject).Find( sSpectralDataDialogKey ) )
      : nullptr;
}

// Current workflow for spectral editing dialog:
// 1. ProjectSettings change event listened by Worker::OnToolChanged()
// 2. In Worker::OnToolChanged(), get Dialog from AttachedWindows and invoke Show()
// 3. Dialog::OnApply() listens to the apply button in Dialog, which calculates and applies the effect
SpectralDataDialogWorker::SpectralDataDialogWorker(AudacityProject &project)
   : mProject{ project }
{
   mSettingsSubscription = ProjectSettings::Get(project)
      .Subscribe(*this, &SpectralDataDialogWorker::OnToolChanged);
   wxTheApp->Bind(wxEVT_IDLE, &SpectralDataDialogWorker::OnIdle, this);
}

SpectralDataDialogWorker::~SpectralDataDialogWorker()
{
   wxTheApp->Unbind(wxEVT_IDLE, &SpectralDataDialogWorker::OnIdle, this);
}

void SpectralDataDialogWorker::OnToolChanged(ProjectSettingsEvent evt)
{
   if (evt.type == ProjectSettingsEvent::ChangedTool) {
      // Find not Get to avoid creating the dialog if not yet done
      if (auto pDialog = SpectralDataDialog::Find(&mProject);
          pDialog && pDialog->IsShown())
         pDialog->DoToolChanged();
      else {
         // Dialog is hidden, or not yet constructed
         using Type = std::underlying_type_t<decltype(ToolCodes::brushTool)>;
         constexpr auto value = static_cast<Type>(ToolCodes::brushTool);

         auto &projectSettings = ProjectSettings::Get( mProject );
         if (projectSettings.GetTool() == value) {
            auto oldValue = evt.oldValue;
            if (oldValue + 1 == value)
               // continue tool rotation
               wxTheApp->CallAfter([&]{ projectSettings.SetTool(
                  (value + 1) % ToolCodes::numTools); });
            else if ((oldValue + ToolCodes::numTools - 1 ) % ToolCodes::numTools
               == value)
               // continue backwards tool rotation
               wxTheApp->CallAfter([&]{ projectSettings.SetTool(
                  (value + ToolCodes::numTools - 1 ) % ToolCodes::numTools); });
            else
               // restore old tool value
               wxTheApp->CallAfter([&]{ projectSettings.SetTool(oldValue); });
         }
      }
   }
}

static bool HasVisibleSpectralView(WaveTrack *wt)
{
   auto &trackView = TrackView::Get(*wt);
   if ( auto waveTrackViewPtr = dynamic_cast<WaveTrackView*>(&trackView) ) {
      const auto range = waveTrackViewPtr->GetSubViews();
      return std::any_of( range.begin(), range.end(),
         [](const auto &pair){
            return dynamic_cast<SpectrumView*>(pair.second.get()); } );
   }
   return false;
}

static unsigned CountVisibleSpectralViews( AudacityProject &project )
{
   const auto range = TrackList::Get(project).Any< WaveTrack >();
   return std::count_if( range.begin(), range.end(), HasVisibleSpectralView );
}

void SpectralDataDialogWorker::OnIdle(wxIdleEvent &evt)
{
   evt.Skip();
   auto nViews = CountVisibleSpectralViews(mProject);
   if (nViews > mPrevNViews) {
      // Some track transitioned its view to spectral or multi.
      // Show the dialog.
      auto &dialog = SpectralDataDialog::Get(mProject);
      dialog.Show();
      dialog.Raise();
   }
   else if (nViews == 0 && mPrevNViews > 0) {
      // The last spectrum view was closed.
      // Hide the dialog.
      if (auto pDialog = SpectralDataDialog::Find(&mProject))
         pDialog->Hide();
   }
   mPrevNViews = nViews;
}

void SpectralDataDialog::DoToolChanged()
{
   UpdateControls( IsBrushToolActive(mProject) );
}

void SpectralDataDialog::OnBrushSizeSlider(wxCommandEvent &event) {
   auto &projectSettings = ProjectSettings::Get( mProject );
   projectSettings.SetBrushRadius(event.GetInt());
}

void SpectralDataDialog::OnCheckSmartSelection(wxCommandEvent &event){
   int isSelected = event.GetInt();
   wxASSERT(isSelected == 0 || isSelected == 1);
   auto &projectSettings = ProjectSettings::Get( mProject );
   projectSettings.SetSmartSelection(isSelected);
}

void SpectralDataDialog::OnCheckOvertones(wxCommandEvent &event){
   int isSelected = event.GetInt();
   wxASSERT(isSelected == 0 || isSelected == 1);
   auto &projectSettings = ProjectSettings::Get( mProject );
   projectSettings.SetOvertones(isSelected);
#ifndef SMART_CHECKBOX
   // One checkbox controls both things
   OnCheckSmartSelection(event);
#endif
}

namespace {
void OnSpectralEditingPanel(const CommandContext &context)
{
   auto &project = context.project;
   auto &dialog = SpectralDataDialog::Get(project);
   dialog.Show( !dialog.IsShown() );
}

using namespace MenuTable;
MenuTable::AttachedItem sAttachment{
   wxT("View/Other/Toolbars/Toolbars/Other"),
   Command( wxT("ShowSpectralSelectionPanel"),
      XXO("Spectra&l Selection Panel"),
      OnSpectralEditingPanel,
      AlwaysEnabledFlag,
      CommandManager::Options{}
         .CheckTest( [](AudacityProject &project) {
            // Find not Get to avoid creating the dialog if not yet done
            auto pDialog = SpectralDataDialog::Find(&project);
            return pDialog && pDialog->IsShown();
         } ) )
};

}

#endif
