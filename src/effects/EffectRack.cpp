/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectRack.cpp

  Leland Lucius

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

**********************************************************************/

#include "../Experimental.h"

#if defined(EXPERIMENTAL_EFFECTS_RACK)

#include <wx/access.h>
#include <wx/defs.h>
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
#include "../widgets/AButton.h"

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
//      wxSIMPLE_BORDER |
      wxFRAME_NO_TASKBAR |
      wxFRAME_FLOAT_ON_PARENT)
{
   mBypassing = false;
   mNumEffects = 0;
   mLastLatency = 0;
   mTimer.SetOwner(this);

   mRemovePushed = CreateImage(remove_16x16_xpm, false, true);
   mRemoveRaised = CreateImage(remove_16x16_xpm, true, true);
   mPowerPushed = CreateImage(power_on_16x16_xpm, false, false);
   mPowerRaised = CreateImage(power_off_16x16_xpm, true, false);
   mFavPushed = CreateImage(fav_down_16x16_xpm, false, false);
   mFavRaised = CreateImage(fav_up_16x16_xpm, true, false);
   mSettingsPushed = CreateImage(settings_up_16x16_xpm, false, true);
   mSettingsRaised = CreateImage(settings_down_16x16_xpm, true, true);
   mUpDisabled = CreateImage(up_9x16_xpm, true, true);
   mUpPushed = CreateImage(up_9x16_xpm, false, true);
   mUpRaised = CreateImage(up_9x16_xpm, true, true);
   mDownDisabled = CreateImage(down_9x16_xpm, true, true);
   mDownPushed = CreateImage(down_9x16_xpm, false, true);
   mDownRaised = CreateImage(down_9x16_xpm, true, true);

   wxBoxSizer *bs = new wxBoxSizer(wxVERTICAL);
   mPanel = new wxPanel(this, wxID_ANY);
   bs->Add(mPanel, 1, wxEXPAND);
   SetSizer(bs);

   wxBoxSizer *hs = new wxBoxSizer(wxHORIZONTAL);
   hs->Add(new wxButton(mPanel, wxID_APPLY, _("&Apply")), 0, wxALIGN_LEFT);
   hs->AddStretchSpacer();
   mLatency = new wxStaticText(mPanel, wxID_ANY, _("Latency: 0"));
   hs->Add(mLatency, 0, wxALIGN_CENTER);
   hs->AddStretchSpacer();
   hs->Add(new wxToggleButton(mPanel, wxID_CLEAR, _("&Bypass")), 0, wxALIGN_RIGHT);

   bs = new wxBoxSizer(wxVERTICAL);
   bs->Add(hs, 0, wxEXPAND);
   bs->Add(new wxStaticLine(mPanel, wxID_ANY), 0, wxEXPAND);

   mMainSizer = new wxFlexGridSizer(7);
   mMainSizer->AddGrowableCol(6);
   mMainSizer->SetHGap(0);
   mMainSizer->SetVGap(0);
   bs->Add(mMainSizer, 1, wxEXPAND);

   mPanel->SetSizer(bs);

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
      wxSizerItem *si;

      si = mMainSizer->GetItem(i * NUMCOLS + COL_FAV);
      AButton *fav = static_cast<AButton *>(si->GetWindow());

      if (fav && fav->IsDown())
      {
         si = mMainSizer->GetItem(i * NUMCOLS + COL_POWER);
         AButton *power = static_cast<AButton *>(si->GetWindow());

         Effect *effect = mEffects[i];
         gPrefs->Write(wxString::Format(wxT("/EffectsRack/Slot%02d"), i),
                       wxString::Format(wxT("%d,%s"),
                                        power->IsDown(),
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

   AButton *ab;
 
   ab = new AButton(mPanel,
                    ID_POWER + mNumEffects,
                    wxDefaultPosition,
                    wxDefaultSize,
                    mPowerRaised,
                    mPowerRaised,
                    mPowerPushed,
                    mPowerPushed,
                    true);
   ab->SetToolTip(_("Set effect active state"));
   if (active)
   {
      ab->PushDown();
   }
   else
   {
      ab->PopUp();
   }
   mMainSizer->Add(ab, 0, wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

   ab = new AButton(mPanel,
                    ID_EDITOR + mNumEffects,
                    wxDefaultPosition,
                    wxDefaultSize,
                    mSettingsRaised,
                    mSettingsRaised,
                    mSettingsPushed,
                    mSettingsPushed,
                    false);
   ab->SetToolTip(_("Open/close effect editor"));
   ab->PopUp();
   mMainSizer->Add(ab, 0, wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

   ab = new AButton(mPanel,
                    ID_UP + mNumEffects,
                    wxDefaultPosition,
                    wxDefaultSize,
                    mUpRaised,
                    mUpRaised,
                    mUpPushed,
                    mUpDisabled,
                    false);
   ab->SetToolTip(_("Move effect up in the rack"));
   ab->PopUp();
   mMainSizer->Add(ab, 0, wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

   ab = new AButton(mPanel,
                    ID_DOWN + mNumEffects,
                    wxDefaultPosition,
                    wxDefaultSize,
                    mDownRaised,
                    mDownRaised,
                    mDownPushed,
                    mDownDisabled,
                    false);
   ab->SetToolTip(_("Move effect down in the rack"));
   ab->PopUp();
   mMainSizer->Add(ab, 0, wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

   ab = new AButton(mPanel,
                    ID_FAV + mNumEffects,
                    wxDefaultPosition,
                    wxDefaultSize,
                    mFavRaised,
                    mFavRaised,
                    mFavPushed,
                    mFavPushed,
                    true);
   ab->SetToolTip(_("Mark effect as a favorite"));
   if (favorite)
   {
      ab->PushDown();
   }
   else
   {
      ab->PopUp();
   }
   mMainSizer->Add(ab, 0, wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

   ab = new AButton(mPanel,
                    ID_REMOVE + mNumEffects,
                    wxDefaultPosition,
                    wxDefaultSize,
                    mRemoveRaised,
                    mRemoveRaised,
                    mRemovePushed,
                    mRemovePushed,
                    false);
   ab->SetToolTip(_("Remove effect from the rack"));
   ab->PopUp();
   mMainSizer->Add(ab, 0, wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

   wxStaticText *text = new wxStaticText(mPanel, ID_NAME + mNumEffects, effect->GetName());
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

void EffectRack::OnTimer(wxTimerEvent & AUNUSED(evt))
{
   int latency = EffectManager::Get().GetRealtimeLatency();
   if (latency != mLastLatency)
   {
      mLatency->SetLabel(wxString::Format(_("Latency: %4d"), latency));
      mLatency->Refresh();
      mLastLatency = latency;
   }
}

void EffectRack::OnApply(wxCommandEvent & AUNUSED(evt))
{
   AudacityProject *project = GetActiveProject();
   
   for (size_t i = 0, cnt = mEffects.GetCount(); i < cnt; i++)
   {
      AButton *btn = static_cast<AButton *>(FindWindowById(ID_POWER + i));
      if (btn->IsDown())
      {
         project->OnEffect(mEffects[i]->GetID(), true);

         btn->PopUp();
         btn->Refresh();
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
   evt.Skip();

   UpdateActive();
}

void EffectRack::OnEditor(wxCommandEvent & evt)
{
   AButton *btn =  static_cast<AButton *>(evt.GetEventObject());

   btn->PopUp();
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
   AButton *btn =  static_cast<AButton *>(evt.GetEventObject());

   btn->PopUp();
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
   AButton *btn =  static_cast<AButton *>(evt.GetEventObject());

   btn->PopUp();
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
   evt.Skip();
}

void EffectRack::OnRemove(wxCommandEvent & evt)
{
   AButton *btn =  static_cast<AButton *>(evt.GetEventObject());

   btn->PopUp();
   evt.Skip();

   int index = GetEffectIndex(btn);
   if (index < 0)
   {
      return;
   }

   mEffects.RemoveAt(index);

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
      delete mMainSizer->GetItem(index)->GetWindow();
   }

   mMainSizer->Layout();
   Fit();

   UpdateActive();
}

wxImage EffectRack::CreateImage(const char *xpm[], bool up, bool pusher)
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

   return mod.ConvertToImage();
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
         wxSizerItem *si = mMainSizer->GetItem(i * NUMCOLS + COL_POWER);
         AButton *power = static_cast<AButton *>(si->GetWindow());
         if (power && power->IsDown())
         {
            mActive.Add(mEffects[i]);
         }
      }
   }

   EffectManager::Get().SetRealtime(mActive);
}

#endif
