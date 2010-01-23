/**********************************************************************

  Audacity: A Digital Audio Editor

  LyricsWindow.h

  Vaughan Johnson
  Dominic Mazzoni

**********************************************************************/

#include "Experimental.h"
#ifdef EXPERIMENTAL_LYRICS_WINDOW

#ifndef __AUDACITY_LYRICS_WINDOW__
#define __AUDACITY_LYRICS_WINDOW__

#include <wx/frame.h>

class AudacityProject;
class Lyrics;

class LyricsWindow : public wxFrame {

 public:
   LyricsWindow(AudacityProject* parent);
   virtual ~LyricsWindow();

   Lyrics *GetLyricsPanel() { return mLyricsPanel; };

 private:
   void OnCloseWindow(wxCloseEvent & WXUNUSED(event));

   void OnStyle_BouncingBall(wxCommandEvent &evt);
   void OnStyle_Highlight(wxCommandEvent &evt);

   AudacityProject *mProject;
   Lyrics *mLyricsPanel;

 public:
   DECLARE_EVENT_TABLE()
};

#endif
#endif // EXPERIMENTAL_LYRICS_WINDOW
