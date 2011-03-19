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
   virtual void Repaint(wxDC *dc) {};
   virtual void EnableDisableButtons() {};
   virtual void UpdatePrefs();

   void GetMeters(Meter **playMeter, Meter **recordMeter);
   void StartMonitoring();
   void Clear();

   virtual void OnSize(wxSizeEvent & event);

   int GetInitialWidth() {return 255;}

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

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 5a2a21f8-6c9e-45a4-8718-c26cad5cfe65
