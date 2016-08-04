/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectRack.cpp

  Leland Lucius

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

**********************************************************************/

#include "../Audacity.h"
#include "../Experimental.h"

#if defined(EXPERIMENTAL_EFFECTS_RACK)

#include <wx/access.h>
#include <wx/defs.h>
#include <wx/bmpbuttn.h>
#include <wx/button.h>
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/image.h>
#include <wx/imaglist.h>
#include <wx/settings.h>
#include <wx/sizer.h>
#include <wx/statline.h>
#include <wx/stattext.h>
#include <wx/timer.h>
#include <wx/tglbtn.h>

#include "EffectManager.h"
#include "EffectRack.h"
#include "../Prefs.h"
#include "../Project.h"

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

EffectRack::EffectRack()
:  wxFrame(GetActiveProject(),
      wxID_ANY,
      _("Effects Rack"),
      wxDefaultPosition,
      wxDefaultSize,
      wxSYSTEM_MENU |
      wxCLOSE_BOX |
      wxCAPTION |
      wxFRAME_NO_TASKBAR |
      wxFRAME_FLOAT_ON_PARENT)
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

      Effect *effect = em.GetEffect(slot.AfterFirst(wxT(',')).c_str());
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

   for (size_t i = 0, cnt = mEffects.GetCount(); i < cnt; i++)
   {
      if (mFavState[i])
      {
         Effect *effect = mEffects[i];
         gPrefs->Write(wxString::Format(wxT("/EffectsRack/Slot%02d"), i),
                       wxString::Format(wxT("%d,%s"),
                                        mPowerState[i],
                                        effect->GetID().c_str()));
      }
   }
}

void EffectRack::Add(Effect *effect, bool active, bool favorite)
{
   if (mEffects.Index(effect) != wxNOT_FOUND)
   {
      return;
   }

   wxBitmapButton *bb;
 
   wxASSERT(mPanel); // To justify safenew
   bb = safenew wxBitmapButton(mPanel, ID_POWER + mNumEffects, mPowerRaised);
   bb->SetBitmapSelected(mPowerRaised);
   bb->SetName(_("Active State"));
   bb->SetToolTip(_("Set effect active state"));
   mPowerState.Add(active);
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
   mFavState.Add(favorite);
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

   wxStaticText *text = safenew wxStaticText(mPanel, ID_NAME + mNumEffects, effect->GetName());
   text->SetToolTip(_("Name of the effect"));
   mMainSizer->Add(text, 0, wxEXPAND | wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL | wxALL, 5);

   mMainSizer->Layout();
   SetSize(GetMinSize());
   Fit();
   Update();

   mEffects.Add(effect);
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
   int latency = EffectManager::Get().GetRealtimeLatency();
   if (latency != mLastLatency)
   {
      mLatency->SetLabel(wxString::Format(_("Latency: %4d"), latency));
      mLatency->Refresh();
      mLastLatency = latency;
   }
}

void EffectRack::OnApply(wxCommandEvent & WXUNUSED(evt))
{
   AudacityProject *project = GetActiveProject();
   
   for (size_t i = 0, cnt = mEffects.GetCount(); i < cnt; i++)
   {
      if (mPowerState[i])
      {
         project->OnEffect(mEffects[i]->GetID(), true);

         mPowerState[i] = false;

         wxBitmapButton *btn = static_cast<wxBitmapButton *>(FindWindowById(ID_POWER + i));
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

   mEffects[index]->PromptUser(GetParent());
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

   size_t index = GetEffectIndex(btn);
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

   mEffects.RemoveAt(index);
   mPowerState.RemoveAt(index);
   mFavState.RemoveAt(index);

   if (mEffects.GetCount() == 0)
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

wxBitmap EffectRack::CreateBitmap(const char *xpm[], bool up, bool pusher)
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
   mEffects.RemoveAt(row);
   mEffects.Insert(effect, row - 1);

   int state = mPowerState[row];
   mPowerState.RemoveAt(row);
   mPowerState.Insert(state, row - 1);

   state = mFavState[row];
   mFavState.RemoveAt(row);
   mFavState.Insert(state, row - 1);

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
      for (size_t i = 0, cnt = mEffects.GetCount(); i < cnt; i++)
      {
         if (mPowerState[i])
         {
            mActive.Add(mEffects[i]);
         }
      }
   }

   EffectManager::Get().RealtimeSetEffects(mActive);
}

#endif
