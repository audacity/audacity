/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioSetupToolBar.h

  K. Soze

  **********************************************************************/

#ifndef __AUDACITY_AUDIO_SETUP_TOOLBAR__
#define __AUDACITY_AUDIO_SETUP_TOOLBAR__

#include <optional>
#include <vector>
#include "ToolBar.h"
#include "Observer.h"

enum class DeviceChangeMessage : char;

class wxMenu;
class wxString;
struct DeviceSourceMap;

class AudioSetupToolBar final : public ToolBar {

 public:

   explicit AudioSetupToolBar( AudacityProject &project );
   virtual ~AudioSetupToolBar();

   static AudioSetupToolBar &Get( AudacityProject &project );
   static const AudioSetupToolBar &Get( const AudacityProject &project );

   void Create(wxWindow * parent) override;

   void UpdatePrefs() override;
   void UpdateSelectedPrefs( int ) override;

   void DeinitChildren();
   void Populate() override;
   void Repaint(wxDC* dc) override;
   void EnableDisableButtons() override;
   void ReCreateButtons() override;
   void OnFocus(wxFocusEvent &event);
   void OnAudioSetup(wxCommandEvent &event);

 private:
   void OnRescannedDevices(DeviceChangeMessage);
   void OnMenu(wxCommandEvent& event);

   bool ChangeHost(int hostId);
   void ChangeDevice(int deviceId, bool isInput);
   void RepopulateMenus();
   void FillHosts();
   void FillHostDevices();
   void FillInputChannels();
   void SetDevices(const DeviceSourceMap *in, const DeviceSourceMap *out);
   void RegenerateTooltips() override;

   void MakeAudioSetupButton();
   void ArrangeButtons();

   std::unique_ptr<wxMenu> CloneMenu(const wxMenu& menu) const;
   void AppendSubMenu(wxMenu& menu, const std::unique_ptr<wxMenu>& submenu, const wxString& title);

   std::optional<wxString> GetSelectedRadioItemLabel(const wxMenu& menu) const;

   enum {
      ID_AUDIO_SETUP_BUTTON = 12000,
      BUTTON_COUNT,
   };

   AButton *mAudioSetup{};
   wxBoxSizer *mSizer{};

   std::unique_ptr<wxMenu> mInput;
   std::unique_ptr<wxMenu> mOutput;
   std::unique_ptr<wxMenu> mInputChannels;
   std::unique_ptr<wxMenu> mHost;

   Observer::Subscription mSubscription;

 public:

   DECLARE_CLASS(AudioSetupToolBar)
   DECLARE_EVENT_TABLE()
};

#endif

