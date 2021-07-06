/**********************************************************************

  Sneedacity: A Digital Audio Editor

  DeviceToolbar.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __SNEEDACITY_DEVICE_TOOLBAR__
#define __SNEEDACITY_DEVICE_TOOLBAR__

#include <vector>
#include "ToolBar.h"

class wxSize;
class wxPoint;
class wxChoice;
struct DeviceSourceMap;

class SneedacityProject;

class DeviceToolBar final : public ToolBar {

 public:

   DeviceToolBar( SneedacityProject &project );
   virtual ~DeviceToolBar();

   static DeviceToolBar &Get( SneedacityProject &project );
   static const DeviceToolBar &Get( const SneedacityProject &project );

   void Create(wxWindow * parent) override;

   void UpdatePrefs() override;
   void UpdateSelectedPrefs( int ) override;

   void DeinitChildren();
   void Populate() override;
   void Repaint(wxDC * WXUNUSED(dc)) override {};
   void EnableDisableButtons() override;
   void OnFocus(wxFocusEvent &event);
   void OnCaptureKey(wxCommandEvent &event);

   void OnChoice(wxCommandEvent & event);

   /// When the prefs don't exist this value is used.
   /// 883 takes a complete row in the default initial size of Sneedacity.
   int GetInitialWidth()  override{ return 883; }
   int GetMinToolbarWidth() override { return 350; }

   void ShowInputDialog();
   void ShowOutputDialog();
   void ShowHostDialog();
   void ShowChannelsDialog();

 private:
   void OnRescannedDevices( wxCommandEvent& );

   int  ChangeHost();
   void ChangeDevice(bool isInput);
   void RefillCombos();
   void FillHosts();
   void FillHostDevices();
   void FillInputChannels();
   void SetDevices(const DeviceSourceMap *in, const DeviceSourceMap *out);
   void SetNames();
   void RegenerateTooltips() override;
   void ShowComboDialog(wxChoice *combo, const TranslatableString &title);

   wxChoice *mInput;
   wxChoice *mOutput;
   wxChoice *mInputChannels;
   wxChoice *mHost;

 public:

   DECLARE_CLASS(DeviceToolBar)
   DECLARE_EVENT_TABLE()
};

#endif

