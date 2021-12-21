/**********************************************************************

  Audacity: A Digital Audio Editor

  LyricsWindow.h

  Vaughan Johnson
  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_LYRICS_WINDOW__
#define __AUDACITY_LYRICS_WINDOW__

#include <wx/frame.h> // to inherit
#include <memory>

#include "Observer.h"
#include "Prefs.h"

class AudacityProject;
class LyricsPanel;

class LyricsWindow final : public wxFrame,
                           public PrefsListener
{

 public:
   LyricsWindow(AudacityProject* parent);

   LyricsPanel *GetLyricsPanel() { return mLyricsPanel; };

 private:
   void OnCloseWindow(wxCloseEvent & WXUNUSED(event));

   void OnStyle_BouncingBall(wxCommandEvent &evt);
   void OnStyle_Highlight(wxCommandEvent &evt);
   void OnTimer(Observer::Message);

   void SetWindowTitle();

   // PrefsListener implementation
   void UpdatePrefs() override;

   std::weak_ptr<AudacityProject> mProject;
   LyricsPanel *mLyricsPanel;
   Observer::Subscription mSubscription;

 public:
   DECLARE_EVENT_TABLE()
};

#endif
