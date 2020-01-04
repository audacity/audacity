/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectRack.h

  Leland Lucius

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

**********************************************************************/

#ifndef __AUDACITY_EFFECTRACK_H__
#define __AUDACITY_EFFECTRACK_H__

#include "../Experimental.h"

#if defined(EXPERIMENTAL_EFFECTS_RACK)

#include <vector>

#include <wx/defs.h>
#include <wx/frame.h> // to inherit
#include <wx/timer.h> // member variable

class wxFlexGridSizer;
class wxPanel;
class wxStaticText;

class AudacityProject;

class Effect;
using EffectArray = std::vector<Effect*>;

class EffectRack final : public wxFrame
{
public:
   EffectRack( AudacityProject &project );
   virtual ~EffectRack();

   void Add(Effect *effect, bool active = false, bool favorite = false);

   static EffectRack &Get( AudacityProject &project );

private:

   wxBitmap CreateBitmap(const char *const xpm[], bool up, bool pusher);
   int GetEffectIndex(wxWindow *win);
   void MoveRowUp(int row);
   void UpdateActive();

   void OnClose(wxCloseEvent & evt);
   void OnTimer(wxTimerEvent & evt);
   void OnApply(wxCommandEvent & evt);
   void OnBypass(wxCommandEvent & evt);

   void OnPower(wxCommandEvent & evt);
   void OnEditor(wxCommandEvent & evt);
   void OnUp(wxCommandEvent & evt);
   void OnDown(wxCommandEvent & evt);
   void OnFav(wxCommandEvent & evt);
   void OnRemove(wxCommandEvent & evt);

private:
   AudacityProject &mProject;

   wxStaticText *mLatency;
   int mLastLatency;

   wxBitmap mPowerPushed;
   wxBitmap mPowerRaised;
   wxBitmap mSettingsPushed;
   wxBitmap mSettingsRaised;
   wxBitmap mUpPushed;
   wxBitmap mUpRaised;
   wxBitmap mUpDisabled;
   wxBitmap mDownPushed;
   wxBitmap mDownRaised;
   wxBitmap mDownDisabled;
   wxBitmap mFavPushed;
   wxBitmap mFavRaised;
   wxBitmap mRemovePushed;
   wxBitmap mRemoveRaised;

   std::vector<int> mPowerState;
   std::vector<int> mFavState;

   int mNumEffects;

   wxTimer mTimer;

   wxPanel *mPanel;
   wxFlexGridSizer *mMainSizer;

   EffectArray mEffects;
   EffectArray mActive;
   bool mBypassing;

   DECLARE_EVENT_TABLE()
};

#endif

#endif // __AUDACITY_EFFECTRACK_H__
