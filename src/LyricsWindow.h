/**********************************************************************

  Audacity: A Digital Audio Editor

  LyricsWindow.h

  Vaughan Johnson
  Dominic Mazzoni

**********************************************************************/

#include "Experimental.h"

#ifndef __AUDACITY_LYRICS_WINDOW__
#define __AUDACITY_LYRICS_WINDOW__

#include <wx/frame.h>

class AudacityProject;
class Lyrics;

class LyricsWindow final : public wxFrame {

 public:
   LyricsWindow(AudacityProject* parent);
   virtual ~LyricsWindow();

   Lyrics *GetLyricsPanel() { return mLyricsPanel; };

 private:
   void OnCloseWindow(wxCloseEvent & WXUNUSED(event));

   void OnStyle_BouncingBall(wxCommandEvent &evt);
   void OnStyle_Highlight(wxCommandEvent &evt);
   void OnTimer(wxCommandEvent &event);

   AudacityProject *mProject;
   Lyrics *mLyricsPanel;

 public:
   DECLARE_EVENT_TABLE()
};

#endif
