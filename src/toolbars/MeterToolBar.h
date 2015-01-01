/**********************************************************************

  Audacity: A Digital Audio Editor

  MeterToolbar.h

  Dominic Mazzoni
  Leland Lucius

  ToolBar to hold the VU Meter

**********************************************************************/

#ifndef __AUDACITY_METER_TOOLBAR__
#define __AUDACITY_METER_TOOLBAR__

#include "ToolBar.h"
#include "../Project.h"

class wxDC;
class wxGridBagSizer;
class wxSizeEvent;
class wxWindow;

class Meter;


// Constants used as bit pattern
const int kWithRecordMeter = 1;
const int kWithPlayMeter = 2;

class MeterToolBar:public ToolBar {

 public:

   MeterToolBar(AudacityProject *project, int type);
   virtual ~MeterToolBar();

   void Create(wxWindow *parent);

   virtual void Populate();
   virtual void ReCreateButtons();
   virtual void Repaint(wxDC * WXUNUSED(dc)) {};
   virtual void EnableDisableButtons() {};
   virtual void UpdatePrefs();

   virtual void OnSize(wxSizeEvent & event);
   virtual bool Expose( bool show );

   int GetInitialWidth() {return (mWhichMeters == 
      (kWithRecordMeter + kWithPlayMeter)) ? 338 : 460;} // Separate bars used to be smaller.
   int GetMinToolbarWidth() { return 50; }
   wxSize GetDockedSize();

 private:
   void RegenerateTooltips();

   AudacityProject *mProject;
   int mWhichMeters;
   wxGridBagSizer *mSizer;
   Meter *mPlayMeter;
   Meter *mRecordMeter;

 public:

   DECLARE_CLASS(MeterToolBar);
   DECLARE_EVENT_TABLE();

};

#endif

