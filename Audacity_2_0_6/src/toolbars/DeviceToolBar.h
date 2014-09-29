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
   virtual void Repaint(wxDC * WXUNUSED(dc)) {};
   virtual void EnableDisableButtons();
   virtual bool Layout();
   void OnFocus(wxFocusEvent &event);
   void OnCaptureKey(wxCommandEvent &event);

   void OnChoice(wxCommandEvent & event);

   /// When the prefs don't exist this value is used.
   /// It should be small enough to work on tiny screens
   int GetInitialWidth() { return 620; }
   virtual int GetMinToolbarWidth() { return 200; }

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
   void SetDevices(const DeviceSourceMap *in, const DeviceSourceMap *out);
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

