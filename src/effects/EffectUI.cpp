/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectUI.cpp

  Leland Lucius

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

**********************************************************************/


#include "EffectUI.h"

#include "widgets/BasicMenu.h"
#include "Effect.h"
#include "EffectManager.h"
#include "../ProjectHistory.h"
#include "../ProjectWindowBase.h"
#include "../TrackPanelAx.h"
#include "RealtimeEffectManager.h"
#include "widgets/wxWidgetsWindowPlacement.h"

#if defined(EXPERIMENTAL_EFFECTS_RACK)

#include "../UndoManager.h"

#include <wx/dcmemory.h>
#include <wx/defs.h>
#include <wx/bmpbuttn.h>
#include <wx/button.h>
#include <wx/frame.h>
#include <wx/image.h>
#include <wx/imaglist.h>
#include <wx/settings.h>
#include <wx/sizer.h>
#include <wx/statline.h>
#include <wx/stattext.h>
#include <wx/timer.h>
#include <wx/tglbtn.h>

#include "../commands/CommandContext.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../widgets/wxPanelWrapper.h"

#include "../../images/EffectRack/EffectRack.h"

#define COL_POWER    0
#define COL_EDITOR   1
#define COL_UP       2
#define COL_DOWN     3
#define COL_FAV      4
#define COL_REMOVE   5
#define COL_NAME     6
#define NUMCOLS      7

#define ID_BASE      20000
#define ID_RANGE     100
#define ID_POWER     (ID_BASE + (COL_POWER * ID_RANGE))
#define ID_EDITOR    (ID_BASE + (COL_EDITOR * ID_RANGE))
#define ID_UP        (ID_BASE + (COL_UP * ID_RANGE))
#define ID_DOWN      (ID_BASE + (COL_DOWN * ID_RANGE))
#define ID_FAV       (ID_BASE + (COL_FAV * ID_RANGE))
#define ID_REMOVE    (ID_BASE + (COL_REMOVE * ID_RANGE))
#define ID_NAME      (ID_BASE + (COL_NAME * ID_RANGE))

BEGIN_EVENT_TABLE(EffectRack, wxFrame)
   EVT_CLOSE(EffectRack::OnClose)
   EVT_TIMER(wxID_ANY, EffectRack::OnTimer)

   EVT_BUTTON(wxID_APPLY, EffectRack::OnApply)
   EVT_TOGGLEBUTTON(wxID_CLEAR, EffectRack::OnBypass)

   EVT_COMMAND_RANGE(ID_REMOVE, ID_REMOVE + 99, wxEVT_COMMAND_BUTTON_CLICKED, EffectRack::OnRemove)
   EVT_COMMAND_RANGE(ID_POWER,  ID_POWER + 99,  wxEVT_COMMAND_BUTTON_CLICKED, EffectRack::OnPower)
   EVT_COMMAND_RANGE(ID_EDITOR, ID_EDITOR + 99, wxEVT_COMMAND_BUTTON_CLICKED, EffectRack::OnEditor)
   EVT_COMMAND_RANGE(ID_UP,     ID_UP + 99,     wxEVT_COMMAND_BUTTON_CLICKED, EffectRack::OnUp)
   EVT_COMMAND_RANGE(ID_DOWN,   ID_DOWN + 99,   wxEVT_COMMAND_BUTTON_CLICKED, EffectRack::OnDown)
   EVT_COMMAND_RANGE(ID_FAV,    ID_FAV + 99,    wxEVT_COMMAND_BUTTON_CLICKED, EffectRack::OnFav)
END_EVENT_TABLE()

EffectRack::EffectRack( AudacityProject &project )
:  wxFrame( &GetProjectFrame( project ),
      wxID_ANY,
      _("Effects Rack"),
      wxDefaultPosition,
      wxDefaultSize,
      wxSYSTEM_MENU |
      wxCLOSE_BOX |
      wxCAPTION |
      wxFRAME_NO_TASKBAR |
      wxFRAME_FLOAT_ON_PARENT)
, mProject{ project }
{
   mBypassing = false;
   mNumEffects = 0;
   mLastLatency = 0;
   mTimer.SetOwner(this);

   mPowerPushed = CreateBitmap(power_on_16x16_xpm, false, false);
   mPowerRaised = CreateBitmap(power_off_16x16_xpm, true, false);
   mSettingsPushed = CreateBitmap(settings_up_16x16_xpm, false, true);
   mSettingsRaised = CreateBitmap(settings_down_16x16_xpm, true, true);
   mUpDisabled = CreateBitmap(up_9x16_xpm, true, true);
   mUpPushed = CreateBitmap(up_9x16_xpm, false, true);
   mUpRaised = CreateBitmap(up_9x16_xpm, true, true);
   mDownDisabled = CreateBitmap(down_9x16_xpm, true, true);
   mDownPushed = CreateBitmap(down_9x16_xpm, false, true);
   mDownRaised = CreateBitmap(down_9x16_xpm, true, true);
   mFavPushed = CreateBitmap(fav_down_16x16_xpm, false, false);
   mFavRaised = CreateBitmap(fav_up_16x16_xpm, true, false);
   mRemovePushed = CreateBitmap(remove_16x16_xpm, false, true);
   mRemoveRaised = CreateBitmap(remove_16x16_xpm, true, true);

   {
      auto bs = std::make_unique<wxBoxSizer>(wxVERTICAL);
      mPanel = safenew wxPanelWrapper(this, wxID_ANY);
      bs->Add(mPanel, 1, wxEXPAND);
      SetSizer(bs.release());
   }

   {
      auto bs = std::make_unique<wxBoxSizer>(wxVERTICAL);
      {
         auto hs = std::make_unique<wxBoxSizer>(wxHORIZONTAL);
         wxASSERT(mPanel); // To justify safenew
         hs->Add(safenew wxButton(mPanel, wxID_APPLY, _("&Apply")), 0, wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);
         hs->AddStretchSpacer();
         mLatency = safenew wxStaticText(mPanel, wxID_ANY, _("Latency: 0"));
         hs->Add(mLatency, 0, wxALIGN_CENTER);
         hs->AddStretchSpacer();
         hs->Add(safenew wxToggleButton(mPanel, wxID_CLEAR, _("&Bypass")), 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         bs->Add(hs.release(), 0, wxEXPAND);
      }
      bs->Add(safenew wxStaticLine(mPanel, wxID_ANY), 0, wxEXPAND);

      {
         auto uMainSizer = std::make_unique<wxFlexGridSizer>(7);
         uMainSizer->AddGrowableCol(6);
         uMainSizer->SetHGap(0);
         uMainSizer->SetVGap(0);
         bs->Add((mMainSizer = uMainSizer.release()), 1, wxEXPAND);
      }

      mPanel->SetSizer(bs.release());
   }

   wxString oldPath = gPrefs->GetPath();
   gPrefs->SetPath(wxT("/EffectsRack"));
   size_t cnt = gPrefs->GetNumberOfEntries();
   gPrefs->SetPath(oldPath);

   EffectManager & em = EffectManager::Get();
   for (size_t i = 0; i < cnt; i++)
   {
      wxString slot;
      gPrefs->Read(wxString::Format(wxT("/EffectsRack/Slot%02d"), i), &slot);

      Effect *effect = em.GetEffect(slot.AfterFirst(wxT(',')));
      if (effect)
      {
         Add(effect, slot.BeforeFirst(wxT(',')) == wxT("1"), true);
      }
   }

   Fit();
}

EffectRack::~EffectRack()
{
   gPrefs->DeleteGroup(wxT("/EffectsRack"));

   for (size_t i = 0, cnt = mEffects.size(); i < cnt; i++)
   {
      if (mFavState[i])
      {
         Effect *effect = mEffects[i];
         gPrefs->Write(wxString::Format(wxT("/EffectsRack/Slot%02d"), i),
                       wxString::Format(wxT("%d,%s"),
                                        mPowerState[i],
                                        effect->GetID()));
      }
   }
}

void EffectRack::Add(Effect *effect, bool active, bool favorite)
{
   if (mEffects.end() != std::find(mEffects.begin(), mEffects.end(), effect))
   {
      return;
   }

   wxBitmapButton *bb;
 
   wxASSERT(mPanel); // To justify safenew
   bb = safenew wxBitmapButton(mPanel, ID_POWER + mNumEffects, mPowerRaised);
   bb->SetBitmapSelected(mPowerRaised);
   bb->SetName(_("Active State"));
   bb->SetToolTip(_("Set effect active state"));
   mPowerState.push_back(active);
   if (active)
   {
      bb->SetBitmapLabel(mPowerPushed);
      bb->SetBitmapSelected(mPowerPushed);
   }
   else
   {
      bb->SetBitmapLabel(mPowerRaised);
      bb->SetBitmapSelected(mPowerRaised);
   }
   mMainSizer->Add(bb, 0, wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

   bb = safenew wxBitmapButton(mPanel, ID_EDITOR + mNumEffects, mSettingsRaised);
   bb->SetBitmapSelected(mSettingsPushed);
   bb->SetName(_("Show/Hide Editor"));
   bb->SetToolTip(_("Open/close effect editor"));
   mMainSizer->Add(bb, 0, wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

   bb = safenew wxBitmapButton(mPanel, ID_UP + mNumEffects, mUpRaised);
   bb->SetBitmapSelected(mUpPushed);
   bb->SetBitmapDisabled(mUpDisabled);
   bb->SetName(_("Move Up"));
   bb->SetToolTip(_("Move effect up in the rack"));
   mMainSizer->Add(bb, 0, wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

   bb = safenew wxBitmapButton(mPanel, ID_DOWN + mNumEffects, mDownRaised);
   bb->SetBitmapSelected(mDownPushed);
   bb->SetBitmapDisabled(mDownDisabled);
   bb->SetName(_("Move Down"));
   bb->SetToolTip(_("Move effect down in the rack"));
   mMainSizer->Add(bb, 0, wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

   bb = safenew wxBitmapButton(mPanel, ID_FAV + mNumEffects, mFavRaised);
   bb->SetBitmapSelected(mFavPushed);
   bb->SetName(_("Favorite"));
   bb->SetToolTip(_("Mark effect as a favorite"));
   mFavState.push_back(favorite);
   if (favorite)
   {
      bb->SetBitmapLabel(mFavPushed);
      bb->SetBitmapSelected(mFavPushed);
   }
   else
   {
      bb->SetBitmapLabel(mFavRaised);
      bb->SetBitmapSelected(mFavRaised);
   }
   mMainSizer->Add(bb, 0, wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

   bb = safenew wxBitmapButton(mPanel, ID_REMOVE + mNumEffects, mRemoveRaised);
   bb->SetBitmapSelected(mRemovePushed);
   bb->SetName(_("Remove"));
   bb->SetToolTip(_("Remove effect from the rack"));
   mMainSizer->Add(bb, 0, wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

   wxStaticText *text = safenew wxStaticText(mPanel, ID_NAME + mNumEffects,
      effect->GetName().Translation() );
   text->SetToolTip(_("Name of the effect"));
   mMainSizer->Add(text, 0, wxEXPAND | wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL | wxALL, 5);

   mMainSizer->Layout();
   SetSize(GetMinSize());
   Fit();
   Update();

   mEffects.push_back(effect);
   mNumEffects++;

   if (!mTimer.IsRunning())
   {
      mTimer.Start(1000);
   }

   if (active)
   {
      UpdateActive();
   }
}

void EffectRack::OnClose(wxCloseEvent & evt)
{
   Show(false);
   evt.Veto();
}

void EffectRack::OnTimer(wxTimerEvent & WXUNUSED(evt))
{
   int latency = RealtimeEffectManager::Get().GetRealtimeLatency();
   if (latency != mLastLatency)
   {
      mLatency->SetLabel(wxString::Format(_("Latency: %4d"), latency));
      mLatency->Refresh();
      mLastLatency = latency;
   }
}

void EffectRack::OnApply(wxCommandEvent & WXUNUSED(evt))
{
   AudacityProject *project = &mProject;

   bool success = false;
   auto state = UndoManager::Get( *project ).GetCurrentState();
   auto cleanup = finally( [&] {
      if(!success)
         // This is like a rollback of state
         ProjectHistory::Get( *project ).SetStateTo( state, false );
   } );

   for (size_t i = 0, cnt = mEffects.size(); i < cnt; i++)
   {
      if (mPowerState[i])
      {
         if (!EffectUI::DoEffect(mEffects[i]->GetID(),
                           *project,
                           EffectManager::kConfigured))
            // If any effect fails (or throws), then stop.
            return;
      }
   }

   success = true;

   // Only after all succeed, do the following.
   for (size_t i = 0, cnt = mEffects.size(); i < cnt; i++)
   {
      if (mPowerState[i])
      {
         mPowerState[i] = false;

         wxBitmapButton *btn =
            static_cast<wxBitmapButton *>(FindWindowById(ID_POWER + i));
         btn->SetBitmapLabel(mPowerRaised);
         btn->SetBitmapSelected(mPowerRaised);
      }
   }

   UpdateActive();
}

void EffectRack::OnBypass(wxCommandEvent & evt)
{
   mBypassing = evt.GetInt() != 0;
   UpdateActive();
}

void EffectRack::OnPower(wxCommandEvent & evt)
{
   wxBitmapButton *btn =  static_cast<wxBitmapButton *>(evt.GetEventObject());

   int index = GetEffectIndex(btn);
   mPowerState[index] = !mPowerState[index];
   if (mPowerState[index])
   {
      btn->SetBitmapLabel(mPowerPushed);
      btn->SetBitmapSelected(mPowerPushed);
   }
   else
   {
      btn->SetBitmapLabel(mPowerRaised);
      btn->SetBitmapSelected(mPowerRaised);
   }

   UpdateActive();
}

void EffectRack::OnEditor(wxCommandEvent & evt)
{
   wxBitmapButton *btn =  static_cast<wxBitmapButton *>(evt.GetEventObject());

   evt.Skip();

   int index = GetEffectIndex(btn);
   if (index < 0)
   {
      return;
   }

   auto pEffect = mEffects[index];
   pEffect->ShowInterface( *GetParent(), EffectUI::DialogFactory,
      pEffect->IsBatchProcessing() );
}

void EffectRack::OnUp(wxCommandEvent & evt)
{
   wxBitmapButton *btn =  static_cast<wxBitmapButton *>(evt.GetEventObject());

   evt.Skip();

   int index = GetEffectIndex(btn);
   if (index <= 0)
   {
      return;
   }

   MoveRowUp(index);
}

void EffectRack::OnDown(wxCommandEvent & evt)
{
   wxBitmapButton *btn =  static_cast<wxBitmapButton *>(evt.GetEventObject());

   evt.Skip();

   int index = GetEffectIndex(btn);
   if (index < 0 || index == (mMainSizer->GetChildren().GetCount() / NUMCOLS) - 1)
   {
      return;
   }

   MoveRowUp(index + 1);
}

void EffectRack::OnFav(wxCommandEvent & evt)
{
   wxBitmapButton *btn =  static_cast<wxBitmapButton *>(evt.GetEventObject());

   int index = GetEffectIndex(btn);
   mFavState[index] = !mFavState[index];
   if (mFavState[index])
   {
      btn->SetBitmapLabel(mFavPushed);
      btn->SetBitmapSelected(mFavPushed);
   }
   else
   {
      btn->SetBitmapLabel(mFavRaised);
      btn->SetBitmapSelected(mFavRaised);
   }
}

void EffectRack::OnRemove(wxCommandEvent & evt)
{
   wxBitmapButton *btn =  static_cast<wxBitmapButton *>(evt.GetEventObject());

   evt.Skip();

   int index = GetEffectIndex(btn);
   if (index < 0)
   {
      return;
   }

   mEffects.erase(mEffects.begin() + index);
   mPowerState.erase(mPowerState.begin() + index);
   mFavState.erase(mFavState.begin() + index);

   if (mEffects.size() == 0)
   {
      if (mTimer.IsRunning())
      {
         mTimer.Stop();
      }
   }

   index *= NUMCOLS;

   for (int i = 0; i < NUMCOLS; i++)
   {
      std::unique_ptr<wxWindow> w {mMainSizer->GetItem(index)->GetWindow()};
      mMainSizer->Detach(index);
   }

   mMainSizer->Layout();
   Fit();

   UpdateActive();
}

wxBitmap EffectRack::CreateBitmap(const char *const xpm[], bool up, bool pusher)
{
   wxMemoryDC dc;
   wxBitmap pic(xpm);

   wxBitmap mod(pic.GetWidth() + 6, pic.GetHeight() + 6);
   dc.SelectObject(mod);
#if defined( __WXGTK__ )
   wxColour newColour = wxSystemSettings::GetColour( wxSYS_COLOUR_BACKGROUND );
#else
   wxColour newColour = wxSystemSettings::GetColour( wxSYS_COLOUR_3DFACE );
#endif
   dc.SetBackground(wxBrush(newColour));
   dc.Clear();

   int offset = 3;
   if (pusher)
   {
      if (!up)
      {
         offset += 1;
      }
   }
   dc.DrawBitmap(pic, offset, offset, true);

   dc.SelectObject(wxNullBitmap);

   return mod;
}

int EffectRack::GetEffectIndex(wxWindow *win)
{
   int col = (win->GetId() - ID_BASE) / ID_RANGE;
   int row;
   int cnt = mMainSizer->GetChildren().GetCount() / NUMCOLS;
   for (row = 0; row < cnt; row++)
   {
      wxSizerItem *si = mMainSizer->GetItem((row * NUMCOLS) + col);
      if (si->GetWindow() == win)
      {
         break;
      }
   }

   if (row == cnt)
   {
      return -1;
   }

   return row;
}

void EffectRack::MoveRowUp(int row)
{
   Effect *effect = mEffects[row];
   mEffects.erase(mEffects.begin() + row);
   mEffects.insert(mEffects.begin() + row - 1, effect);

   int state = mPowerState[row];
   mPowerState.erase(mPowerState.begin() + row);
   mPowerState.insert(mPowerState.begin() + row - 1, state);

   state = mFavState[row];
   mFavState.erase(mFavState.begin() + row);
   mFavState.insert(mFavState.begin() + row - 1, state);

   row *= NUMCOLS;

   for (int i = 0; i < NUMCOLS; i++)
   {
      wxSizerItem *si = mMainSizer->GetItem(row + NUMCOLS - 1);
      wxWindow *w = si->GetWindow();
      int flags = si->GetFlag();
      int border = si->GetBorder();
      int prop = si->GetProportion();
      mMainSizer->Detach(row + NUMCOLS - 1);
      mMainSizer->Insert(row - NUMCOLS, w, prop, flags, border);
   }

   mMainSizer->Layout();
   Refresh();

   UpdateActive();
}

void EffectRack::UpdateActive()
{
   mActive.clear();

   if (!mBypassing)
   {
      for (size_t i = 0, cnt = mEffects.size(); i < cnt; i++)
      {
         if (mPowerState[i])
         {
            mActive.push_back(mEffects[i]);
         }
      }
   }

   RealtimeEffectManager::Get().RealtimeSetEffects(
      { mActive.begin(), mActive.end() }
   );
}

namespace
{
AudacityProject::AttachedWindows::RegisteredFactory sKey{
   []( AudacityProject &parent ) -> wxWeakRef< wxWindow > {
      auto result = safenew EffectRack( parent );
      result->CenterOnParent();
      return result;
   }
};
}

EffectRack &EffectRack::Get( AudacityProject &project )
{
   return project.AttachedWindows::Get< EffectRack >( sKey );
}

#endif

///////////////////////////////////////////////////////////////////////////////
//
// EffectPanel
//
///////////////////////////////////////////////////////////////////////////////

class EffectPanel final : public wxPanelWrapper
{
public:
   EffectPanel(wxWindow *parent)
   :  wxPanelWrapper(parent)
   {
      // This fools NVDA into not saying "Panel" when the dialog gets focus
      SetName(TranslatableString::Inaudible);
      SetLabel(TranslatableString::Inaudible);

      mAcceptsFocus = true;
   }

   virtual ~EffectPanel()
   {
   }

   // ============================================================================
   // wxWindow implementation
   // ============================================================================

   bool AcceptsFocus() const override
   {
      return mAcceptsFocus;
   }

   // So that wxPanel is not included in Tab traversal, when required - see wxWidgets bug 15581
   bool AcceptsFocusFromKeyboard() const override
   {
      return mAcceptsFocus;
   }

   // ============================================================================
   // EffectPanel implementation
   // ============================================================================
   void SetAccept(bool accept)
   {
      mAcceptsFocus = accept;
   }

private:
   bool mAcceptsFocus;
};

///////////////////////////////////////////////////////////////////////////////
//
// EffectUIHost
//
///////////////////////////////////////////////////////////////////////////////

#include "../../images/Effect.h"
#include "../AudioIO.h"
#include "../CommonCommandFlags.h"
#include "../Menus.h"
#include "../prefs/GUISettings.h" // for RTL_WORKAROUND
#include "Project.h"
#include "../ProjectAudioManager.h"
#include "../ShuttleGui.h"
#include "ViewInfo.h"
#include "../commands/AudacityCommand.h"
#include "../commands/CommandContext.h"
#include "../widgets/AudacityMessageBox.h"
#include "../widgets/HelpSystem.h"

#include <wx/app.h>
#include <wx/bmpbuttn.h>
#include <wx/checkbox.h>
#include <wx/dcclient.h>
#include <wx/dcmemory.h>
#include <wx/menu.h>
#include <wx/settings.h>
#include <wx/sizer.h>
#include <wx/textctrl.h>

#if defined(__WXMAC__)
#include <Cocoa/Cocoa.h>
#endif

static const int kDummyID = 20000;
static const int kSaveAsID = 20001;
static const int kImportID = 20002;
static const int kExportID = 20003;
static const int kDefaultsID = 20004;
static const int kOptionsID = 20005;
static const int kUserPresetsDummyID = 20006;
static const int kDeletePresetDummyID = 20007;
static const int kMenuID = 20100;
static const int kEnableID = 20101;
static const int kPlayID = 20102;
static const int kRewindID = 20103;
static const int kFFwdID = 20104;
static const int kPlaybackID = 20105;
static const int kCaptureID = 20106;
static const int kUserPresetsID = 21000;
static const int kDeletePresetID = 22000;
static const int kFactoryPresetsID = 23000;

BEGIN_EVENT_TABLE(EffectUIHost, wxDialogWrapper)
EVT_INIT_DIALOG(EffectUIHost::OnInitDialog)
EVT_ERASE_BACKGROUND(EffectUIHost::OnErase)
EVT_PAINT(EffectUIHost::OnPaint)
EVT_CLOSE(EffectUIHost::OnClose)
EVT_BUTTON(wxID_APPLY, EffectUIHost::OnApply)
EVT_BUTTON(wxID_CANCEL, EffectUIHost::OnCancel)
EVT_BUTTON(wxID_HELP, EffectUIHost::OnHelp)
EVT_BUTTON(eDebugID, EffectUIHost::OnDebug)
EVT_BUTTON(kMenuID, EffectUIHost::OnMenu)
EVT_CHECKBOX(kEnableID, EffectUIHost::OnEnable)
EVT_BUTTON(kPlayID, EffectUIHost::OnPlay)
EVT_BUTTON(kRewindID, EffectUIHost::OnRewind)
EVT_BUTTON(kFFwdID, EffectUIHost::OnFFwd)
EVT_MENU(kSaveAsID, EffectUIHost::OnSaveAs)
EVT_MENU(kImportID, EffectUIHost::OnImport)
EVT_MENU(kExportID, EffectUIHost::OnExport)
EVT_MENU(kOptionsID, EffectUIHost::OnOptions)
EVT_MENU(kDefaultsID, EffectUIHost::OnDefaults)
EVT_MENU_RANGE(kUserPresetsID, kUserPresetsID + 999, EffectUIHost::OnUserPreset)
EVT_MENU_RANGE(kDeletePresetID, kDeletePresetID + 999, EffectUIHost::OnDeletePreset)
EVT_MENU_RANGE(kFactoryPresetsID, kFactoryPresetsID + 999, EffectUIHost::OnFactoryPreset)
END_EVENT_TABLE()

EffectUIHost::EffectUIHost(wxWindow *parent,
                           AudacityProject &project,
                           Effect *effect,
                           EffectUIClientInterface *client)
:  wxDialogWrapper(parent, wxID_ANY, effect->GetName(),
                   wxDefaultPosition, wxDefaultSize,
                   wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER | wxMINIMIZE_BOX | wxMAXIMIZE_BOX)
{
#if defined(__WXMAC__)
   // Make sure the effect window actually floats above the main window
   [ [((NSView *)GetHandle()) window] setLevel:NSFloatingWindowLevel];
#endif
   
   SetName( effect->GetName() );
   SetExtraStyle(GetExtraStyle() | wxWS_EX_VALIDATE_RECURSIVELY);
   
   mParent = parent;
   mEffect = effect;
   mCommand = NULL;
   mClient = client;
   
   mProject = &project;
   
   mInitialized = false;
   mSupportsRealtime = false;
   
   mDisableTransport = false;
   
   mEnabled = true;
   
   mPlayPos = 0.0;
   mClient->SetHostUI(this);
}

EffectUIHost::EffectUIHost(wxWindow *parent,
                           AudacityProject &project,
                           AudacityCommand *command,
                           EffectUIClientInterface *client)
:  wxDialogWrapper(parent, wxID_ANY, XO("Some Command") /*command->GetName()*/,
                   wxDefaultPosition, wxDefaultSize,
                   wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER | wxMINIMIZE_BOX | wxMAXIMIZE_BOX)
{
#if defined(__WXMAC__)
   // Make sure the effect window actually floats above the main window
   [ [((NSView *)GetHandle()) window] setLevel:NSFloatingWindowLevel];
#endif
   
   //SetName( command->GetName() );
   SetExtraStyle(wxWS_EX_VALIDATE_RECURSIVELY);
   
   mParent = parent;
   mEffect = NULL;
   mCommand = command;
   mClient = client;
   
   mProject = &project;
   
   mInitialized = false;
   mSupportsRealtime = false;
   
   mDisableTransport = false;
   
   mEnabled = true;
   
   mPlayPos = 0.0;
   mClient->SetHostUI(this);
}




EffectUIHost::~EffectUIHost()
{
   CleanupRealtime();
   
   if (mClient)
   {
      if (mNeedsResume)
         Resume();
      
      mClient->CloseUI();
      mClient = NULL;
   }
}

// ============================================================================
// wxWindow implementation
// ============================================================================

bool EffectUIHost::TransferDataToWindow()
{
   if( mEffect )
      return mEffect->TransferDataToWindow();
   if( mCommand )
      return mCommand->TransferDataToWindow();
   return false;
}

bool EffectUIHost::TransferDataFromWindow()
{
   if( mEffect)
      return mEffect->TransferDataFromWindow();
   if( mCommand)
      return mCommand->TransferDataFromWindow();
   return false;
}

// ============================================================================
// wxDialog implementation
// ============================================================================

int EffectUIHost::ShowModal()
{
#if defined(__WXMSW__)
   // Swap the Close and Apply buttons
   wxSizer *sz = mApplyBtn->GetContainingSizer();
   wxASSERT(mApplyBtn->GetParent()); // To justify safenew
   wxButton *apply = safenew wxButton(mApplyBtn->GetParent(), wxID_APPLY);
   sz->Replace(mCloseBtn, apply);
   sz->Replace(mApplyBtn, mCloseBtn);
   sz->Layout();
   mApplyBtn->Destroy();
   mApplyBtn = apply;
   mApplyBtn->SetDefault();
   mApplyBtn->SetLabel(wxGetStockLabel(wxID_OK, 0));
   mCloseBtn->SetLabel(wxGetStockLabel(wxID_CANCEL, 0));
#else
   mApplyBtn->SetLabel(wxGetStockLabel(wxID_OK));
   mCloseBtn->SetLabel(wxGetStockLabel(wxID_CANCEL));
#endif
   
   Layout();
   
   return wxDialogWrapper::ShowModal();
}

// ============================================================================
// EffectUIHost implementation
// ============================================================================

wxPanel *EffectUIHost::BuildButtonBar(wxWindow *parent)
{
   mSupportsRealtime = mEffect && mEffect->SupportsRealtime();
   mIsGUI = mClient->IsGraphicalUI();
   mIsBatch = (mEffect && mEffect->IsBatchProcessing()) ||
      (mCommand && mCommand->IsBatchProcessing());

   int margin = 0;
#if defined(__WXMAC__)
   margin = 3; // I'm sure it's needed because of the order things are created...
#endif

   const auto bar = safenew wxPanelWrapper(parent, wxID_ANY);

   // This fools NVDA into not saying "Panel" when the dialog gets focus
   bar->SetName(TranslatableString::Inaudible);
   bar->SetLabel(TranslatableString::Inaudible);

   ShuttleGui S{ bar, eIsCreating,
      false /* horizontal */,
      { -1, -1 } /* minimum size */
   };
   {
      S.SetBorder( margin );

      if (!mIsGUI)
      {
         mMenuBtn = S.Id( kMenuID )
            .ToolTip(XO("Manage presets and options"))
            .AddButton( XXO("&Manage"), wxALIGN_CENTER | wxTOP | wxBOTTOM );
      }
      else
      {
         mMenuBtn = S.Id( kMenuID )
            .ToolTip(XO("Manage presets and options"))
            .Name(XO("&Manage"))
            .AddBitmapButton( CreateBitmap(effect_menu_xpm, true, true) );
         mMenuBtn->SetBitmapPressed(CreateBitmap(effect_menu_xpm, false, true));
      }

      S.AddSpace( 5, 5 );

      if (!mIsBatch)
      {
         if (!mIsGUI)
         {
            if (mSupportsRealtime)
            {
               mPlayToggleBtn = S.Id( kPlayID )
                  .ToolTip(XO("Start and stop playback"))
                  .AddButton( XXO("Start &Playback"),
                              wxALIGN_CENTER | wxTOP | wxBOTTOM );
            }
            else if (mEffect &&
               (mEffect->GetType() != EffectTypeAnalyze) &&
               (mEffect->GetType() != EffectTypeTool) )
            {
               mPlayToggleBtn = S.Id( kPlayID )
                  .ToolTip(XO("Preview effect"))
                  .AddButton( XXO("&Preview"),
                              wxALIGN_CENTER | wxTOP | wxBOTTOM );
            }
         }
         else
         {
            mPlayBM = CreateBitmap(effect_play_xpm, true, false);
            mPlayDisabledBM = CreateBitmap(effect_play_disabled_xpm, true, true);
            mStopBM = CreateBitmap(effect_stop_xpm, true, false);
            mStopDisabledBM = CreateBitmap(effect_stop_disabled_xpm, true, false);
            mPlayBtn = S.Id( kPlayID ).AddBitmapButton( mPlayBM );
            mPlayBtn->SetBitmapDisabled(mPlayDisabledBM);
            mPlayBtn->SetBitmapPressed(CreateBitmap(effect_play_xpm, false, true));
            if (!mSupportsRealtime)
            {
               mPlayBtn->SetToolTip(_("Preview effect"));
#if defined(__WXMAC__)
               mPlayBtn->SetName(_("Preview effect"));
#else
               mPlayBtn->SetLabel(_("&Preview effect"));
#endif
            }
         }

         if (mSupportsRealtime)
         {
            if (!mIsGUI)
            {
               mRewindBtn = S.Id( kRewindID )
                  .ToolTip(XO("Skip backward"))
                  .AddButton( XXO("Skip &Backward"),
                              wxALIGN_CENTER | wxTOP | wxBOTTOM );
            }
            else
            {
               mRewindBtn = S.Id( kRewindID )
                  .ToolTip(XO("Skip backward"))
                  .Name(XO("Skip &Backward"))
                  .AddBitmapButton( CreateBitmap(
                     effect_rewind_xpm, true, true) );
               mRewindBtn->SetBitmapDisabled(
                     CreateBitmap(effect_rewind_disabled_xpm, true, false));
               mRewindBtn->SetBitmapPressed(CreateBitmap(effect_rewind_xpm, false, true));
            }

            if (!mIsGUI)
            {
               mFFwdBtn = S.Id( kFFwdID )
                  .ToolTip(XO("Skip forward"))
                  .AddButton( XXO("Skip &Forward"),
                     wxALIGN_CENTER | wxTOP | wxBOTTOM );
            }
            else
            {
               mFFwdBtn = S.Id( kFFwdID )
                  .ToolTip(XO("Skip forward"))
                  .Name(XO("Skip &Forward"))
                  .AddBitmapButton( CreateBitmap(
                     effect_ffwd_xpm, true, true) );
               mFFwdBtn->SetBitmapDisabled(
                  CreateBitmap(effect_ffwd_disabled_xpm, true, false));
               mFFwdBtn->SetBitmapPressed(CreateBitmap(effect_ffwd_xpm, false, true));
            }

            S.AddSpace( 5, 5 );

            mEnableCb = S.Id( kEnableID )
               .Position(wxALIGN_CENTER | wxTOP | wxBOTTOM)
               .Name(XO("Enable"))
               .AddCheckBox( XXO("&Enable"), mEnabled );
            //
         }
      }
   }

   bar->GetSizer()->SetSizeHints( bar );

   return bar;
}

bool EffectUIHost::Initialize()
{
   {
      auto gAudioIO = AudioIO::Get();
      mDisableTransport = !gAudioIO->IsAvailable(*mProject);
      mPlaying = gAudioIO->IsStreamActive(); // not exactly right, but will suffice
      mCapturing = gAudioIO->IsStreamActive() && gAudioIO->GetNumCaptureChannels() > 0 && !gAudioIO->IsMonitoring();
   }

   EffectPanel *w {};
   ShuttleGui S{ this, eIsCreating };
   {
      S.StartHorizontalLay( wxEXPAND );
      {
         Destroy_ptr<EffectPanel> uw{ safenew EffectPanel( S.GetParent() ) };
         RTL_WORKAROUND(uw.get());

         // Try to give the window a sensible default/minimum size
         uw->SetMinSize(wxSize(wxMax(600, mParent->GetSize().GetWidth() * 2 / 3),
            mParent->GetSize().GetHeight() / 2));

         ShuttleGui S1{ uw.get(), eIsCreating };
         if (!mClient->PopulateUI(S1))
         {
            return false;
         }

         S.Prop( 1 )
            .Position(wxEXPAND)
            .AddWindow((w = uw.release()));
      }
      S.EndHorizontalLay();

      S.StartPanel();
      {
         const auto bar = BuildButtonBar( S.GetParent() );

         long buttons;
         if ( mEffect && mEffect->ManualPage().empty() && mEffect->HelpPage().empty()) {
            buttons = eApplyButton | eCloseButton;
            this->SetAcceleratorTable(wxNullAcceleratorTable);
         }
         else {
            buttons = eApplyButton | eCloseButton | eHelpButton;
            wxAcceleratorEntry entries[1];
#if defined(__WXMAC__)
            // Is there a standard shortcut on Mac?
#else
            entries[0].Set(wxACCEL_NORMAL, (int) WXK_F1, wxID_HELP);
#endif
            wxAcceleratorTable accel(1, entries);
            this->SetAcceleratorTable(accel);
         }

         if (mEffect && mEffect->mUIDebug) {
            buttons |= eDebugButton;
         }

         S.AddStandardButtons(buttons, bar);
      }
      S.EndPanel();
   }

   Layout();
   Fit();
   Center();

   mApplyBtn = (wxButton *) FindWindow(wxID_APPLY);
   mCloseBtn = (wxButton *) FindWindow(wxID_CANCEL);

   UpdateControls();

   w->SetAccept(!mIsGUI);
   (!mIsGUI ? w : FindWindow(wxID_APPLY))->SetFocus();

   LoadUserPresets();

   InitializeRealtime();

   SetMinSize(GetSize());
   return true;
}

void EffectUIHost::OnInitDialog(wxInitDialogEvent & evt)
{
   // Do default handling
   wxDialogWrapper::OnInitDialog(evt);
   
#if wxCHECK_VERSION(3, 0, 0)
   //#warning "check to see if this still needed in wx3"
#endif
   
   // Pure hackage coming down the pike...
   //
   // I have no idea why, but if a wxTextCtrl is the first control in the
   // panel, then its contents will not be automatically selected when the
   // dialog is displayed.
   //
   // So, we do the selection manually.
   wxTextCtrl *focused = wxDynamicCast(FindFocus(), wxTextCtrl);
   if (focused)
   {
      focused->SelectAll();
   }
}

void EffectUIHost::OnErase(wxEraseEvent & WXUNUSED(evt))
{
   // Ignore it
}

void EffectUIHost::OnPaint(wxPaintEvent & WXUNUSED(evt))
{
   wxPaintDC dc(this);
   
   dc.Clear();
}

void EffectUIHost::OnClose(wxCloseEvent & WXUNUSED(evt))
{
   DoCancel();
   
   CleanupRealtime();
   
   Hide();
   
   if (mNeedsResume)
      Resume();
   mClient->CloseUI();
   mClient = NULL;
   
   Destroy();
}

void EffectUIHost::OnApply(wxCommandEvent & evt)
{
   auto &project = *mProject;

   // On wxGTK (wx2.8.12), the default action is still executed even if
   // the button is disabled.  This appears to affect all wxDialogs, not
   // just our Effects dialogs.  So, this is a only temporary workaround
   // for legacy effects that disable the OK button.  Hopefully this has
   // been corrected in wx3.
   if (!FindWindow(wxID_APPLY)->IsEnabled())
   {
      return;
   }
   
   // Honor the "select all if none" preference...a little hackish, but whatcha gonna do...
   if (!mIsBatch &&
       mEffect &&
       mEffect->GetType() != EffectTypeGenerate &&
       mEffect->GetType() != EffectTypeTool &&
       ViewInfo::Get( project ).selectedRegion.isPoint())
   {
      auto flags = AlwaysEnabledFlag;
      bool allowed =
      MenuManager::Get( project ).ReportIfActionNotAllowed(
         mEffect->GetName(),
         flags,
         WaveTracksSelectedFlag() | TimeSelectedFlag());
      if (!allowed)
         return;
   }
   
   if (!mClient->ValidateUI())
   {
      return;
   }
   
   // This will take care of calling TransferDataFromWindow() for an effect.
   if (mEffect &&  !mEffect->SaveUserPreset(mEffect->GetCurrentSettingsGroup()))
   {
      return;
   }
   // This will take care of calling TransferDataFromWindow() for a command.
   if (mCommand ){
      wxString params;
      mCommand->GetAutomationParameters( params );
   }
   
   if( mEffect )
      mEffect->mUIResultID = evt.GetId();
   
   if (IsModal())
   {
      mDismissed = true;
      
      EndModal(true);
      
      Close();
      
      return;
   }
   
   // Progress dialog no longer yields, so this "shouldn't" be necessary (yet to be proven
   // for sure), but it is a nice visual cue that something is going on.
   mApplyBtn->Disable();
   auto cleanup = finally( [&] { mApplyBtn->Enable(); } );

   if( mEffect ) {
      CommandContext context( project );
      // This is absolute hackage...but easy and I can't think of another way just now.
      //
      // It should callback to the EffectManager to kick off the processing
      EffectUI::DoEffect(mEffect->GetID(), context,
         EffectManager::kConfigured);
   }

   if( mCommand )
      // PRL:  I don't like the global and would rather pass *mProject!
      // But I am preserving old behavior
      mCommand->Apply( CommandContext{ project } );
}

void EffectUIHost::DoCancel()
{
   if (!mDismissed) {
      if( mEffect )
         mEffect->mUIResultID = wxID_CANCEL;
      
      if (IsModal())
         EndModal(false);
      else
         Hide();
      
      mDismissed = true;
   }
}

void EffectUIHost::OnCancel(wxCommandEvent & WXUNUSED(evt))
{
   DoCancel();
   Close();
}

void EffectUIHost::OnHelp(wxCommandEvent & WXUNUSED(event))
{
   if (mEffect && mEffect->GetFamily() == NYQUISTEFFECTS_FAMILY && (mEffect->ManualPage().empty())) {
      // Old ShowHelp required when there is no on-line manual.
      // Always use default web browser to allow full-featured HTML pages.
      HelpSystem::ShowHelp(FindWindow(wxID_HELP), mEffect->HelpPage(), wxEmptyString, true, true);
   }
   else if( mEffect )
   {
      // otherwise use the NEW ShowHelp
      HelpSystem::ShowHelp(FindWindow(wxID_HELP), mEffect->ManualPage(), true);
   }
}

void EffectUIHost::OnDebug(wxCommandEvent & evt)
{
   OnApply(evt);
   if( mEffect )
      mEffect->mUIResultID = evt.GetId();
}

void EffectUIHost::OnMenu(wxCommandEvent & WXUNUSED(evt))
{
   wxMenu menu;
   menu.Bind(wxEVT_MENU, [](auto&){}, kUserPresetsDummyID);
   menu.Bind(wxEVT_MENU, [](auto&){}, kDeletePresetDummyID);
   if( !mEffect )
      return;
   
   LoadUserPresets();
   
   if (mUserPresets.size() == 0)
   {
      menu.Append(kUserPresetsDummyID, _("User Presets"))->Enable(false);
   }
   else
   {
      auto sub = std::make_unique<wxMenu>();
      for (size_t i = 0, cnt = mUserPresets.size(); i < cnt; i++)
      {
         sub->Append(kUserPresetsID + i, mUserPresets[i]);
      }
      menu.Append(0, _("User Presets"), sub.release());
   }
   
   menu.Append(kSaveAsID, _("Save Preset..."));
   
   if (mUserPresets.size() == 0)
   {
      menu.Append(kDeletePresetDummyID, _("Delete Preset"))->Enable(false);
   }
   else
   {
      auto sub = std::make_unique<wxMenu>();
      for (size_t i = 0, cnt = mUserPresets.size(); i < cnt; i++)
      {
         sub->Append(kDeletePresetID + i, mUserPresets[i]);
      }
      menu.Append(0, _("Delete Preset"), sub.release());
   }
   
   menu.AppendSeparator();
   
   auto factory = mEffect->GetFactoryPresets();
   
   {
      auto sub = std::make_unique<wxMenu>();
      sub->Append(kDefaultsID, _("Defaults"));
      if (factory.size() > 0)
      {
         sub->AppendSeparator();
         for (size_t i = 0, cnt = factory.size(); i < cnt; i++)
         {
            auto label = factory[i];
            if (label.empty())
            {
               label = _("None");
            }
            
            sub->Append(kFactoryPresetsID + i, label);
         }
      }
      menu.Append(0, _("Factory Presets"), sub.release());
   }
   
   menu.AppendSeparator();
   menu.Append(kImportID, _("Import..."))->Enable(mClient->CanExportPresets());
   menu.Append(kExportID, _("Export..."))->Enable(mClient->CanExportPresets());
   menu.AppendSeparator();
   menu.Append(kOptionsID, _("Options..."))->Enable(mClient->HasOptions());
   menu.AppendSeparator();
   
   {
      auto sub = std::make_unique<wxMenu>();
      
      sub->Append(kDummyID, wxString::Format(_("Type: %s"),
                                             ::wxGetTranslation( mEffect->GetFamily().Translation() )));
      sub->Append(kDummyID, wxString::Format(_("Name: %s"), mEffect->GetName().Translation()));
      sub->Append(kDummyID, wxString::Format(_("Version: %s"), mEffect->GetVersion()));
      sub->Append(kDummyID, wxString::Format(_("Vendor: %s"), mEffect->GetVendor().Translation()));
      sub->Append(kDummyID, wxString::Format(_("Description: %s"), mEffect->GetDescription().Translation()));
      sub->Bind(wxEVT_MENU, [](auto&){}, kDummyID);

      menu.Append(0, _("About"), sub.release());
   }
   
   wxWindow *btn = FindWindow(kMenuID);
   wxRect r = btn->GetRect();
   BasicMenu::Handle{ &menu }.Popup(
      wxWidgetsWindowPlacement{ btn },
      { r.GetLeft(), r.GetBottom() }
   );
}

void EffectUIHost::Resume()
{
   if (!mClient->ValidateUI()) {
      // If we're previewing we should still be able to stop playback
      // so don't disable transport buttons.
      //   mEffect->EnableApply(false);   // currently this would also disable transport buttons.
      // The preferred behaviour is currently undecided, so for now
      // just disallow enabling until settings are valid.
      mEnabled = false;
      mEnableCb->SetValue(mEnabled);
      return;
   }
   RealtimeEffectManager::Get().RealtimeResumeOne( *mEffect );
}

void EffectUIHost::OnEnable(wxCommandEvent & WXUNUSED(evt))
{
   mEnabled = mEnableCb->GetValue();
   
   if (mEnabled) {
      Resume();
      mNeedsResume = false;
   }
   else
   {
      RealtimeEffectManager::Get().RealtimeSuspendOne( *mEffect );
      mNeedsResume = true;
   }
   
   UpdateControls();
}

void EffectUIHost::OnPlay(wxCommandEvent & WXUNUSED(evt))
{
   if (!mSupportsRealtime)
   {
      if (!mClient->ValidateUI() || !mEffect->TransferDataFromWindow())
      {
         return;
      }
      
      mEffect->Preview(false);
      
      return;
   }
   
   if (mPlaying)
   {
      auto gAudioIO = AudioIO::Get();
      mPlayPos = gAudioIO->GetStreamTime();
      auto &projectAudioManager = ProjectAudioManager::Get( *mProject );
      projectAudioManager.Stop();
   }
   else
   {
      auto &viewInfo = ViewInfo::Get( *mProject );
      const auto &selectedRegion = viewInfo.selectedRegion;
      const auto &playRegion = viewInfo.playRegion;
      if ( playRegion.Locked() )
      {
         mRegion.setTimes(playRegion.GetStart(), playRegion.GetEnd());
         mPlayPos = mRegion.t0();
      }
      else if (selectedRegion.t0() != mRegion.t0() ||
               selectedRegion.t1() != mRegion.t1())
      {
         mRegion = selectedRegion;
         mPlayPos = mRegion.t0();
      }
      
      if (mPlayPos > mRegion.t1())
      {
         mPlayPos = mRegion.t1();
      }
      
      auto &projectAudioManager = ProjectAudioManager::Get( *mProject );
      projectAudioManager.PlayPlayRegion(
                                         SelectedRegion(mPlayPos, mRegion.t1()),
                                         DefaultPlayOptions( *mProject ),
                                         PlayMode::normalPlay );
   }
}

void EffectUIHost::OnRewind(wxCommandEvent & WXUNUSED(evt))
{
   if (mPlaying)
   {
      auto gAudioIO = AudioIO::Get();
      double seek;
      gPrefs->Read(wxT("/AudioIO/SeekShortPeriod"), &seek, 1.0);
      
      double pos = gAudioIO->GetStreamTime();
      if (pos - seek < mRegion.t0())
      {
         seek = pos - mRegion.t0();
      }
      
      gAudioIO->SeekStream(-seek);
   }
   else
   {
      mPlayPos = mRegion.t0();
   }
}

void EffectUIHost::OnFFwd(wxCommandEvent & WXUNUSED(evt))
{
   if (mPlaying)
   {
      double seek;
      gPrefs->Read(wxT("/AudioIO/SeekShortPeriod"), &seek, 1.0);
      
      auto gAudioIO = AudioIO::Get();
      double pos = gAudioIO->GetStreamTime();
      if (mRegion.t0() < mRegion.t1() && pos + seek > mRegion.t1())
      {
         seek = mRegion.t1() - pos;
      }
      
      gAudioIO->SeekStream(seek);
   }
   else
   {
      // It allows to play past end of selection...probably useless
      mPlayPos = mRegion.t1();
   }
}

void EffectUIHost::OnPlayback(wxCommandEvent & evt)
{
   evt.Skip();
   
   if (evt.GetInt() != 0)
   {
      if (evt.GetEventObject() != mProject)
      {
         mDisableTransport = true;
      }
      else
      {
         mPlaying = true;
      }
   }
   else
   {
      mDisableTransport = false;
      mPlaying = false;
   }
   
   if (mPlaying)
   {
      mRegion = ViewInfo::Get( *mProject ).selectedRegion;
      mPlayPos = mRegion.t0();
   }
   
   UpdateControls();
}

void EffectUIHost::OnCapture(wxCommandEvent & evt)
{
   evt.Skip();
   
   if (evt.GetInt() != 0)
   {
      if (evt.GetEventObject() != mProject)
      {
         mDisableTransport = true;
      }
      else
      {
         mCapturing = true;
      }
   }
   else
   {
      mDisableTransport = false;
      mCapturing = false;
   }
   
   UpdateControls();
}

void EffectUIHost::OnUserPreset(wxCommandEvent & evt)
{
   int preset = evt.GetId() - kUserPresetsID;
   
   mEffect->LoadUserPreset(mEffect->GetUserPresetsGroup(mUserPresets[preset]));
   
   return;
}

void EffectUIHost::OnFactoryPreset(wxCommandEvent & evt)
{
   mEffect->LoadFactoryPreset(evt.GetId() - kFactoryPresetsID);
   
   return;
}

void EffectUIHost::OnDeletePreset(wxCommandEvent & evt)
{
   auto preset = mUserPresets[evt.GetId() - kDeletePresetID];
   
   int res = AudacityMessageBox(
                                XO("Are you sure you want to delete \"%s\"?").Format( preset ),
                                XO("Delete Preset"),
                                wxICON_QUESTION | wxYES_NO);
   if (res == wxYES)
   {
      mEffect->RemovePrivateConfigSubgroup(mEffect->GetUserPresetsGroup(preset));
   }
   
   LoadUserPresets();
   
   return;
}

void EffectUIHost::OnSaveAs(wxCommandEvent & WXUNUSED(evt))
{
   wxTextCtrl *text;
   wxString name;
   wxDialogWrapper dlg(this, wxID_ANY, XO("Save Preset"));
   
   ShuttleGui S(&dlg, eIsCreating);
   
   S.StartPanel();
   {
      S.StartVerticalLay(1);
      {
         S.StartHorizontalLay(wxALIGN_LEFT, 0);
         {
            text = S.AddTextBox(XXO("Preset name:"), name, 30);
         }
         S.EndHorizontalLay();
         S.SetBorder(10);
         S.AddStandardButtons();
      }
      S.EndVerticalLay();
   }
   S.EndPanel();
   
   dlg.SetSize(dlg.GetSizer()->GetMinSize());
   dlg.Center();
   dlg.Fit();
   
   while (true)
   {
      int rc = dlg.ShowModal();
      
      if (rc != wxID_OK)
      {
         break;
      }
      
      name = text->GetValue();
      if (name.empty())
      {
         AudacityMessageDialog md(
                                  this,
                                  XO("You must specify a name"),
                                  XO("Save Preset") );
         md.Center();
         md.ShowModal();
         continue;
      }
      
      if ( make_iterator_range( mUserPresets ).contains( name ) )
      {
         AudacityMessageDialog md(
                                  this,
                                  XO("Preset already exists.\n\nReplace?"),
                                  XO("Save Preset"),
                                  wxYES_NO | wxCANCEL | wxICON_EXCLAMATION );
         md.Center();
         int choice = md.ShowModal();
         if (choice == wxID_CANCEL)
         {
            break;
         }
         
         if (choice == wxID_NO)
         {
            continue;
         }
      }
      
      mEffect->SaveUserPreset(mEffect->GetUserPresetsGroup(name));
      LoadUserPresets();
      
      break;
   }
   
   return;
}

void EffectUIHost::OnImport(wxCommandEvent & WXUNUSED(evt))
{
   mClient->ImportPresets();
   
   LoadUserPresets();
   
   return;
}

void EffectUIHost::OnExport(wxCommandEvent & WXUNUSED(evt))
{
   // may throw
   // exceptions are handled in AudacityApp::OnExceptionInMainLoop
   mClient->ExportPresets();
   
   return;
}

void EffectUIHost::OnOptions(wxCommandEvent & WXUNUSED(evt))
{
   mClient->ShowOptions();
   
   return;
}

void EffectUIHost::OnDefaults(wxCommandEvent & WXUNUSED(evt))
{
   mEffect->LoadFactoryDefaults();
   
   return;
}

wxBitmap EffectUIHost::CreateBitmap(const char * const xpm[], bool up, bool pusher)
{
   wxMemoryDC dc;
   wxBitmap pic(xpm);
   
   wxBitmap mod(pic.GetWidth() + 6, pic.GetHeight() + 6, 24);
   dc.SelectObject(mod);
   
#if defined(__WXGTK__)
   wxColour newColour = wxSystemSettings::GetColour(wxSYS_COLOUR_BACKGROUND);
#else
   wxColour newColour = wxSystemSettings::GetColour(wxSYS_COLOUR_BTNFACE);
#endif
   
   dc.SetBackground(wxBrush(newColour));
   dc.Clear();
   
   int offset = 3;
   if (pusher)
   {
      if (!up)
      {
         offset += 1;
      }
   }
   
   dc.DrawBitmap(pic, offset, offset, true);
   
   dc.SelectObject(wxNullBitmap);
   
   return mod;
}

void EffectUIHost::UpdateControls()
{
   if (mIsBatch)
   {
      return;
   }

   if (mCapturing || mDisableTransport)
   {
      // Don't allow focus to get trapped
      wxWindow *focus = FindFocus();
      if (focus == mRewindBtn || focus == mFFwdBtn || focus == mPlayBtn || focus == mEnableCb)
      {
         mCloseBtn->SetFocus();
      }
   }
   
   mApplyBtn->Enable(!mCapturing);
   if (mEffect && (mEffect->GetType() != EffectTypeAnalyze) && (mEffect->GetType() != EffectTypeTool) )
   {
      (!mIsGUI ? mPlayToggleBtn : mPlayBtn)->Enable(!(mCapturing || mDisableTransport));
   }
   
   if (mSupportsRealtime)
   {
      mRewindBtn->Enable(!(mCapturing || mDisableTransport));
      mFFwdBtn->Enable(!(mCapturing || mDisableTransport));
      mEnableCb->Enable(!(mCapturing || mDisableTransport));
      
      wxBitmapButton *bb;
      
      if (mPlaying)
      {
         if (!mIsGUI)
         {
            /* i18n-hint: The access key "&P" should be the same in
             "Stop &Playback" and "Start &Playback" */
            mPlayToggleBtn->SetLabel(_("Stop &Playback"));
            mPlayToggleBtn->Refresh();
         }
         else
         {
            bb = (wxBitmapButton *) mPlayBtn;
            bb->SetBitmapLabel(mStopBM);
            bb->SetBitmapDisabled(mStopDisabledBM);
            bb->SetToolTip(_("Stop"));
#if defined(__WXMAC__)
            bb->SetName(_("Stop &Playback"));
#else
            bb->SetLabel(_("Stop &Playback"));
#endif
         }
      }
      else
      {
         if (!mIsGUI)
         {
            /* i18n-hint: The access key "&P" should be the same in
             "Stop &Playback" and "Start &Playback" */
            mPlayToggleBtn->SetLabel(_("Start &Playback"));
            mPlayToggleBtn->Refresh();
         }
         else
         {
            bb = (wxBitmapButton *) mPlayBtn;
            bb->SetBitmapLabel(mPlayBM);
            bb->SetBitmapDisabled(mPlayDisabledBM);
            bb->SetToolTip(_("Play"));
#if defined(__WXMAC__)
            bb->SetName(_("Start &Playback"));
#else
            bb->SetLabel(_("Start &Playback"));
#endif
         }
      }
   }
}

void EffectUIHost::LoadUserPresets()
{
   mUserPresets.clear();
   
   if( mEffect )
      mEffect->GetPrivateConfigSubgroups(mEffect->GetUserPresetsGroup(wxEmptyString), mUserPresets);
   
   std::sort( mUserPresets.begin(), mUserPresets.end() );
   
   return;
}

void EffectUIHost::InitializeRealtime()
{
   if (mSupportsRealtime && !mInitialized)
   {
      RealtimeEffectManager::Get().RealtimeAddEffect(mEffect);
      
      wxTheApp->Bind(EVT_AUDIOIO_PLAYBACK,
                     &EffectUIHost::OnPlayback,
                     this);
      
      wxTheApp->Bind(EVT_AUDIOIO_CAPTURE,
                     &EffectUIHost::OnCapture,
                     this);
      
      mInitialized = true;
   }
}

void EffectUIHost::CleanupRealtime()
{
   if (mSupportsRealtime && mInitialized)
   {
      RealtimeEffectManager::Get().RealtimeRemoveEffect(mEffect);
      
      mInitialized = false;
   }
}

wxDialog *EffectUI::DialogFactory( wxWindow &parent, EffectHostInterface *pHost,
   EffectUIClientInterface *client)
{
   auto pEffect = dynamic_cast< Effect* >( pHost );
   if ( ! pEffect )
      return nullptr;

   // Make sure there is an associated project, whose lifetime will
   // govern the lifetime of the dialog, even when the dialog is
   // non-modal, as for realtime effects
   auto project = FindProjectFromWindow(&parent);
   if ( !project )
      return nullptr;

   Destroy_ptr<EffectUIHost> dlg{
      safenew EffectUIHost{ &parent, *project, pEffect, client} };
   
   if (dlg->Initialize())
   {
      // release() is safe because parent will own it
      return dlg.release();
   }
   
   return nullptr;
};

#include "../PluginManager.h"
#include "ProjectRate.h"
#include "../ProjectWindow.h"
#include "../SelectUtilities.h"
#include "../TrackPanel.h"
#include "../WaveTrack.h"
#include "../commands/CommandManager.h"

/// DoEffect() takes a PluginID and executes the associated effect.
///
/// At the moment flags are used only to indicate whether to prompt for
//  parameters, whether to save the state to history and whether to allow
/// 'Repeat Last Effect'.

/* static */ bool EffectUI::DoEffect(
   const PluginID & ID, const CommandContext &context, unsigned flags )
{
   AudacityProject &project = context.project;
   auto &tracks = TrackList::Get( project );
   auto &trackPanel = TrackPanel::Get( project );
   auto &trackFactory = WaveTrackFactory::Get( project );
   auto rate = ProjectRate::Get(project).GetRate();
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
   auto &commandManager = CommandManager::Get( project );
   auto &window = ProjectWindow::Get( project );

   const PluginDescriptor *plug = PluginManager::Get().GetPlugin(ID);
   if (!plug)
      return false;

   EffectType type = plug->GetEffectType();

   // Make sure there's no activity since the effect is about to be applied
   // to the project's tracks.  Mainly for Apply during RTP, but also used
   // for batch commands
   if (flags & EffectManager::kConfigured)
   {
      ProjectAudioManager::Get( project ).Stop();
      //Don't Select All if repeating Generator Effect
      if (!(flags & EffectManager::kConfigured)) {
         SelectUtilities::SelectAllIfNone(project);
      }
   }

   auto nTracksOriginally = tracks.size();
   wxWindow *focus = wxWindow::FindFocus();
   wxWindow *parent = nullptr;
   if (focus != nullptr) {
      parent = focus->GetParent();
   }

   bool success = false;
   auto cleanup = finally( [&] {

      if (!success) {
         // For now, we're limiting realtime preview to a single effect, so
         // make sure the menus reflect that fact that one may have just been
         // opened.
         MenuManager::Get(project).UpdateMenus( false );
      }

   } );

   int count = 0;
   bool clean = true;
   for (auto t : tracks.Selected< const WaveTrack >()) {
      if (t->GetEndTime() != 0.0)
         clean = false;
      count++;
   }

   EffectManager & em = EffectManager::Get();

   em.SetSkipStateFlag( false );
   if (auto effect = em.GetEffect(ID)) {
#if defined(EXPERIMENTAL_EFFECTS_RACK)
      if (effect->SupportsRealtime())
      {
         EffectRack::Get( context.project ).Add(effect);
      }
#endif
      effect->SetUIFlags(flags);
      success = effect->DoEffect(
         rate,
         &tracks,
         &trackFactory,
         selectedRegion,
         &window,
         (flags & EffectManager::kConfigured) == 0
            ? DialogFactory
            : nullptr
      );
   }
   else
      success = false;

   if (!success)
      return false;

   if (em.GetSkipStateFlag())
      flags = flags | EffectManager::kSkipState;

   if (!(flags & EffectManager::kSkipState))
   {
      auto shortDesc = em.GetCommandName(ID);
      auto longDesc = em.GetCommandDescription(ID);
      ProjectHistory::Get( project ).PushState(longDesc, shortDesc);
   }

   if (!(flags & EffectManager::kDontRepeatLast))
   {
      // Remember a successful generator, effect, analyzer, or tool Process
         auto shortDesc = em.GetCommandName(ID);
         /* i18n-hint: %s will be the name of the effect which will be
          * repeated if this menu item is chosen */
         auto lastEffectDesc = XO("Repeat %s").Format(shortDesc);
         auto& menuManager = MenuManager::Get(project);
         switch ( type ) {
         case EffectTypeGenerate:
            commandManager.Modify(wxT("RepeatLastGenerator"), lastEffectDesc);
            menuManager.mLastGenerator = ID;
            menuManager.mRepeatGeneratorFlags = EffectManager::kConfigured;
            break;
         case EffectTypeProcess:
            commandManager.Modify(wxT("RepeatLastEffect"), lastEffectDesc);
            menuManager.mLastEffect = ID;
            menuManager.mRepeatEffectFlags = EffectManager::kConfigured;
            break;
         case EffectTypeAnalyze:
            commandManager.Modify(wxT("RepeatLastAnalyzer"), lastEffectDesc);
            menuManager.mLastAnalyzer = ID;
            menuManager.mLastAnalyzerRegistration = MenuCreator::repeattypeplugin;
            menuManager.mRepeatAnalyzerFlags = EffectManager::kConfigured;
            break;
         case EffectTypeTool:
            commandManager.Modify(wxT("RepeatLastTool"), lastEffectDesc);
            menuManager.mLastTool = ID;
            menuManager.mLastToolRegistration = MenuCreator::repeattypeplugin;
            menuManager.mRepeatToolFlags = EffectManager::kConfigured;
            if (shortDesc == NYQUIST_PROMPT_NAME) {
               menuManager.mRepeatToolFlags = EffectManager::kRepeatNyquistPrompt;  //Nyquist Prompt is not configured
            }
            break;
      }
   }

   //STM:
   //The following automatically re-zooms after sound was generated.
   // IMO, it was disorienting, removing to try out without re-fitting
   //mchinen:12/14/08 reapplying for generate effects
   if (type == EffectTypeGenerate)
   {
      if (count == 0 || (clean && selectedRegion.t0() == 0.0))
         window.DoZoomFit();
         //  trackPanel->Refresh(false);
   }

   // PRL:  RedrawProject explicitly because sometimes history push is skipped
   window.RedrawProject();

   if (focus != nullptr && focus->GetParent()==parent) {
      focus->SetFocus();
   }

   // A fix for Bug 63
   // New tracks added?  Scroll them into view so that user sees them.
   // Don't care what track type.  An analyser might just have added a
   // Label track and we want to see it.
   if( tracks.size() > nTracksOriginally ){
      // 0.0 is min scroll position, 1.0 is max scroll position.
      trackPanel.VerticalScroll( 1.0 );
   }
   else {
      auto pTrack = *tracks.Selected().begin();
      if (!pTrack)
         pTrack = *tracks.Any().begin();
      if (pTrack) {
         TrackFocus::Get(project).Set(pTrack);
         pTrack->EnsureVisible();
      }
   }

   return true;
}

///////////////////////////////////////////////////////////////////////////////
BEGIN_EVENT_TABLE(EffectDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, EffectDialog::OnOk)
END_EVENT_TABLE()

EffectDialog::EffectDialog(wxWindow * parent,
                           const TranslatableString & title,
                           int type,
                           int flags,
                           int additionalButtons)
: wxDialogWrapper(parent, wxID_ANY, title, wxDefaultPosition, wxDefaultSize, flags)
{
   mType = type;
   mAdditionalButtons = additionalButtons;
}

void EffectDialog::Init()
{
   long buttons = eOkButton;
   if ((mType != EffectTypeAnalyze) && (mType != EffectTypeTool))
   {
      buttons |= eCancelButton;
      if (mType == EffectTypeProcess)
      {
         buttons |= ePreviewButton;
      }
   }

   ShuttleGui S(this, eIsCreating);

   S.SetBorder(5);
   S.StartVerticalLay(true);
   {
      PopulateOrExchange(S);
      S.AddStandardButtons(buttons|mAdditionalButtons);
   }
   S.EndVerticalLay();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();
}

/// This is a virtual function which will be overridden to
/// provide the actual parameters that we want for each
/// kind of dialog.
void EffectDialog::PopulateOrExchange(ShuttleGui & WXUNUSED(S))
{
   return;
}

bool EffectDialog::TransferDataToWindow()
{
   ShuttleGui S(this, eIsSettingToDialog);
   PopulateOrExchange(S);

   return true;
}

bool EffectDialog::TransferDataFromWindow()
{
   ShuttleGui S(this, eIsGettingFromDialog);
   PopulateOrExchange(S);

   return true;
}

bool EffectDialog::Validate()
{
   return true;
}

void EffectDialog::OnPreview(wxCommandEvent & WXUNUSED(evt))
{
   return;
}

void EffectDialog::OnOk(wxCommandEvent & WXUNUSED(evt))
{
   // On wxGTK (wx2.8.12), the default action is still executed even if
   // the button is disabled.  This appears to affect all wxDialogs, not
   // just our Effects dialogs.  So, this is a only temporary workaround
   // for legacy effects that disable the OK button.  Hopefully this has
   // been corrected in wx3.
   if (FindWindow(wxID_OK)->IsEnabled() && Validate() && TransferDataFromWindow())
   {
      EndModal(true);
   }

   return;
}
