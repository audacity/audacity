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
struct DeviceSourceMap;

class DeviceToolBar final : public ToolBar {

 public:

   DeviceToolBar();
   virtual ~DeviceToolBar();

   void Create(wxWindow * parent) override;

   void UpdatePrefs() override;

   void DeinitChildren();
   void Populate() override;
   void Repaint(wxDC * WXUNUSED(dc)) override {};
   void EnableDisableButtons() override;
   bool Layout() override;
   void OnFocus(wxFocusEvent &event);
   void OnCaptureKey(wxCommandEvent &event);

   void OnChoice(wxCommandEvent & event);

   /// When the prefs don't exist this value is used.
   /// 883 takes a complete row in the default initial size of Audacity.
   int GetInitialWidth()  override{ return 883; }
   int GetMinToolbarWidth() override { return 350; }

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

   wxChoice *mInput;
   wxChoice *mOutput;
   wxChoice *mInputChannels;
   wxChoice *mHost;

 public:

   DECLARE_CLASS(DeviceToolBar)
   DECLARE_EVENT_TABLE()
};

#endif

