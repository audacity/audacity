/**********************************************************************

  Audacity: A Digital Audio Editor

  DeviceToolbar.h
 
  Dominic Mazzoni
 
**********************************************************************/

#ifndef __AUDACITY_DEVICE_TOOLBAR__
#define __AUDACITY_DEVICE_TOOLBAR__

#include <vector>
#include "ToolBar.h"

class wxImage;
class wxSize;
class wxPoint;
class wxChoice;
class wxStaticText;
struct DeviceSourceMap;

class DeviceToolBar:public ToolBar {

 public:

   DeviceToolBar();
   virtual ~DeviceToolBar();

   void Create(wxWindow * parent);

   void RecreateTipWindows();
   void UpdatePrefs();

   void DeinitChildren();
   virtual void Populate();
   virtual void Repaint(wxDC *dc) {};
   virtual void EnableDisableButtons();
   virtual bool Layout();
   void OnFocus(wxFocusEvent &event);
   void OnCaptureKey(wxCommandEvent &event);

   void OnChoice(wxCommandEvent & event);

   /// When the prefs don't exist this value is used.
   /// It should be small enough to work on tiny screens
   int GetInitialWidth() {return 520;}
   virtual int GetMinToolbarWidth() {return 200;}

   void ShowInputDialog();
   void ShowOutputDialog();
   void ShowHostDialog();
   void ShowChannelsDialog();

   void RefillCombos();

 private:
   int  ChangeHost();
   void ChangeDevice(bool isInput);
   void FillHosts();
   void FillHostDevices();
   void FillInputChannels();
   void SetDevices(DeviceSourceMap *in, DeviceSourceMap *out);
   void RepositionCombos();
   void RegenerateTooltips();

   void ShowComboDialog(wxChoice *combo, const wxString &title);

   wxBitmap *mPlayBitmap;
   wxBitmap *mRecordBitmap;

   wxChoice *mInput;
   wxChoice *mOutput;
   wxChoice *mInputChannels;
   wxChoice *mHost;

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

