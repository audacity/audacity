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

class wxDC;
class wxGridBagSizer;
class wxSizeEvent;
class wxWindow;

class Meter;

class MeterToolBar:public ToolBar {

 public:

   MeterToolBar();
   virtual ~MeterToolBar();

   void Create(wxWindow *parent);
   bool DestroyChildren();

   virtual void Populate();
   virtual void Repaint(wxDC * WXUNUSED(dc)) {};
   virtual void EnableDisableButtons() {};
   virtual void UpdatePrefs();

   void GetMeters(Meter **playMeter, Meter **recordMeter);
   void StartMonitoring();
   void Clear();

   virtual void OnSize(wxSizeEvent & event);

   int GetInitialWidth() {return 338;}
   int GetMinToolbarWidth() { return 255; }

 private:
   void RegenerateTooltips();

   wxGridBagSizer *mSizer;
   Meter *mPlayMeter;
   Meter *mRecordMeter;

 public:

   DECLARE_CLASS(MeterToolBar);
   DECLARE_EVENT_TABLE();

};

#endif

