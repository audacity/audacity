/**********************************************************************

  Audacity: A Digital Audio Editor

  DeviceToolbar.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_DEVICE_TOOLBAR__
#define __AUDACITY_DEVICE_TOOLBAR__

#include "../MemoryX.h"
#include <vector>
#include "ToolBar.h"

class wxImage;
class wxSize;
class wxPoint;
class wxChoice;
class wxStaticText;
struct DeviceSourceMap;

class DeviceToolBar final : public ToolBar {

 public:

   DeviceToolBar();
   virtual ~DeviceToolBar();

   void Create(wxWindow * parent);

   void UpdatePrefs();

   void DeinitChildren();
   void Populate() override;
   void Repaint(wxDC * WXUNUSED(dc)) override {};
   void EnableDisableButtons() override;
   bool Layout() override;
   void OnFocus(wxFocusEvent &event);
   void OnCaptureKey(wxCommandEvent &event);

   void OnChoice(wxCommandEvent & event);

   /// When the prefs don't exist this value is used.
   /// It should be small enough to work on tiny screens
   int GetInitialWidth() { return 620; }
   int GetMinToolbarWidth() override { return 200; }

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
   void SetNames();
   void RegenerateTooltips() override;

   void ShowComboDialog(wxChoice *combo, const wxString &title);

   std::unique_ptr<wxBitmap> mPlayBitmap, mRecordBitmap;

   wxChoice *mInput;
   wxChoice *mOutput;
   wxChoice *mInputChannels;
   wxChoice *mHost;

 public:

   DECLARE_CLASS(DeviceToolBar)
   DECLARE_EVENT_TABLE()
};

#endif

