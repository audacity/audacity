/**********************************************************************

  Audacity: A Digital Audio Editor

  DeviceToolbar.h
 
  Dominic Mazzoni
 
**********************************************************************/

#ifndef __AUDACITY_DEVICE_TOOLBAR__
#define __AUDACITY_DEVICE_TOOLBAR__

#include "ToolBar.h"

class wxImage;
class wxSize;
class wxPoint;
class wxChoice;

class DeviceToolBar:public ToolBar {

 public:

   DeviceToolBar();
   virtual ~DeviceToolBar();

   void Create(wxWindow * parent);

   void RecreateTipWindows();
   void UpdatePrefs();

   virtual void Populate();
   virtual void Repaint(wxDC *dc) {};
   virtual void EnableDisableButtons() {};

   void OnFocus(wxFocusEvent &event);
   void OnCaptureKey(wxCommandEvent &event);

   void OnChoice(wxCommandEvent & event);

 private:
   void RegenerateTooltips();

   wxBitmap *mPlayBitmap;
   wxBitmap *mRecordBitmap;

   wxChoice *mInput;
   wxChoice *mOutput;

 public:

   DECLARE_CLASS(DeviceToolBar);
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
// arch-tag: 3acba542-52ae-44eb-b0b3-e0645587b5c0

