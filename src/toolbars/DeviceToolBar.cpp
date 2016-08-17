/**********************************************************************

  Audacity: A Digital Audio Editor

  DeviceToolBar.cpp

  Dominic Mazzoni

*******************************************************************//*!

\class DeviceToolBar
\brief A toobar to allow easier changing of input and output devices .

*//*******************************************************************/


#include "../Audacity.h"
#include "DeviceToolBar.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifndef WX_PRECOMP
#include <wx/choice.h>
#include <wx/event.h>
#include <wx/intl.h>
#include <wx/settings.h>
#include <wx/sizer.h>
#include <wx/statbmp.h>
#include <wx/tooltip.h>
#endif

#include "../AudacityApp.h"

#include "ToolDock.h"
#include "../TrackPanel.h"

#include "../AColor.h"
#include "../AllThemeResources.h"
#include "../AudioIO.h"
#include "../ImageManipulation.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../ShuttleGui.h"
#include "../Theme.h"
#include "../widgets/Grabber.h"
#include "../DeviceManager.h"

IMPLEMENT_CLASS(DeviceToolBar, ToolBar);

////////////////////////////////////////////////////////////
/// Methods for DeviceToolBar
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE(DeviceToolBar, ToolBar)
   EVT_CHOICE(wxID_ANY, DeviceToolBar::OnChoice)
   EVT_COMMAND(wxID_ANY, EVT_CAPTURE_KEY, DeviceToolBar::OnCaptureKey)
END_EVENT_TABLE()

//Standard contructor
DeviceToolBar::DeviceToolBar()
: ToolBar(DeviceBarID, _("Device"), wxT("Device"), true)
{
   mPlayBitmap = NULL;
   mRecordBitmap = NULL;
}

DeviceToolBar::~DeviceToolBar()
{
}

void DeviceToolBar::Create(wxWindow *parent)
{
   ToolBar::Create(parent);

   // Simulate a size event to set initial meter placement/size
   wxSizeEvent event(GetSize(), GetId());
   event.SetEventObject(this);
   GetEventHandler()->ProcessEvent(event);
}

void DeviceToolBar::DeinitChildren()
{
   mInput         = NULL;
   mOutput        = NULL;
   mInputChannels = NULL;
   mHost          = NULL;
}

void DeviceToolBar::Populate()
{
   DeinitChildren();
   // Hosts
   mHost = safenew wxChoice(this,
                        wxID_ANY,
                        wxDefaultPosition,
                        wxDefaultSize);

   Add(mHost, 0, wxALIGN_CENTER);

   // Input device
   if( mRecordBitmap == NULL )
      mRecordBitmap = std::make_unique<wxBitmap>(theTheme.Bitmap(bmpMic));

   Add(safenew wxStaticBitmap(this,
                          wxID_ANY,
                          *mRecordBitmap), 0, wxALIGN_CENTER);

   mInput = safenew wxChoice(this,
                         wxID_ANY,
                         wxDefaultPosition,
                         wxDefaultSize);
   Add(mInput, 0, wxALIGN_CENTER);

   mInputChannels = safenew wxChoice(this,
                         wxID_ANY,
                         wxDefaultPosition,
                         wxDefaultSize);
   Add(mInputChannels, 0, wxALIGN_CENTER);

   // Output device
   if( mPlayBitmap == NULL )
      mPlayBitmap = std::make_unique<wxBitmap>(theTheme.Bitmap(bmpSpeaker));
   Add(safenew wxStaticBitmap(this,
                          wxID_ANY,
                          *mPlayBitmap), 0, wxALIGN_CENTER);

   mOutput = safenew wxChoice(this,
                               wxID_ANY,
                               wxDefaultPosition,
                               wxDefaultSize);
   Add(mOutput, 0, wxALIGN_CENTER);




   mHost->Connect(wxEVT_SET_FOCUS,
                 wxFocusEventHandler(DeviceToolBar::OnFocus),
                 NULL,
                 this);
   mHost->Connect(wxEVT_KILL_FOCUS,
                 wxFocusEventHandler(DeviceToolBar::OnFocus),
                 NULL,
                 this);
   mOutput->Connect(wxEVT_SET_FOCUS,
                 wxFocusEventHandler(DeviceToolBar::OnFocus),
                 NULL,
                 this);
   mOutput->Connect(wxEVT_KILL_FOCUS,
                 wxFocusEventHandler(DeviceToolBar::OnFocus),
                 NULL,
                 this);
   mInput->Connect(wxEVT_SET_FOCUS,
                 wxFocusEventHandler(DeviceToolBar::OnFocus),
                 NULL,
                 this);
   mInput->Connect(wxEVT_KILL_FOCUS,
                 wxFocusEventHandler(DeviceToolBar::OnFocus),
                 NULL,
                 this);
   mInputChannels->Connect(wxEVT_SET_FOCUS,
                 wxFocusEventHandler(DeviceToolBar::OnFocus),
                 NULL,
                 this);
   mInputChannels->Connect(wxEVT_KILL_FOCUS,
                 wxFocusEventHandler(DeviceToolBar::OnFocus),
                 NULL,
                 this);

   SetNames();

   RefillCombos();
}

void DeviceToolBar::RefillCombos()
{
   FillHosts();
   FillHostDevices();
   FillInputChannels();
   // make the device display selection reflect the prefs if they exist
   UpdatePrefs();
}

void DeviceToolBar::OnFocus(wxFocusEvent &event)
{
   if (event.GetEventType() == wxEVT_KILL_FOCUS) {
      AudacityProject::ReleaseKeyboard(this);
   }
   else {
      AudacityProject::CaptureKeyboard(this);
   }

   Refresh(false);

   event.Skip();
}

void DeviceToolBar::OnCaptureKey(wxCommandEvent &event)
{
   wxKeyEvent *kevent = (wxKeyEvent *)event.GetEventObject();
   int keyCode = kevent->GetKeyCode();

   // Pass UP/DOWN/LEFT/RIGHT through for input/output choice
   if (FindFocus() == mInput && (keyCode == WXK_LEFT || keyCode == WXK_RIGHT
                                 || keyCode == WXK_UP || keyCode == WXK_DOWN)) {
      return;
   }

   if (FindFocus() == mOutput && (keyCode == WXK_LEFT || keyCode == WXK_RIGHT
                                 || keyCode == WXK_UP || keyCode == WXK_DOWN)) {
      return;
   }
   event.Skip();

   return;
}

void DeviceToolBar::UpdatePrefs()
{
   wxString hostName;
   wxString devName;
   wxString sourceName;
   wxString desc;
   const std::vector<DeviceSourceMap> &inMaps  = DeviceManager::Instance()->GetInputDeviceMaps();
   const std::vector<DeviceSourceMap> &outMaps = DeviceManager::Instance()->GetOutputDeviceMaps();


   int hostSelectionIndex = mHost->GetSelection();
   wxString oldHost = hostSelectionIndex >= 0 ? mHost->GetString(hostSelectionIndex) :
                                                wxT("");
   hostName = gPrefs->Read(wxT("/AudioIO/Host"), wxT(""));

   // if the prefs host name doesn't match the one displayed, it changed
   // in another project's DeviceToolBar, so we need to repopulate everything.
   if (oldHost != hostName)
      FillHostDevices();

   devName = gPrefs->Read(wxT("/AudioIO/RecordingDevice"), wxT(""));
   sourceName = gPrefs->Read(wxT("/AudioIO/RecordingSource"), wxT(""));
   if (sourceName == wxT(""))
      desc = devName;
   else
      desc = devName + wxT(": ") + sourceName;

   if (mInput->GetStringSelection() != desc &&
       mInput->FindString(desc) != wxNOT_FOUND) {
      mInput->SetStringSelection(desc);
      FillInputChannels();
   } else if (mInput->GetStringSelection() != desc && mInput->GetCount()) {
      for (size_t i = 0; i < inMaps.size(); i++) {
         if (inMaps[i].hostString == hostName &&
             MakeDeviceSourceString(&inMaps[i]) == mInput->GetString(0)) {
            // use the default.  It should exist but check just in case, falling back on the 0 index.
            DeviceSourceMap *defaultMap = DeviceManager::Instance()->GetDefaultInputDevice(inMaps[i].hostIndex);
            if (defaultMap) {
               mInput->SetStringSelection(MakeDeviceSourceString(defaultMap));
               SetDevices(defaultMap, NULL);
            } else {
               //use the first item (0th index) if we have no familiar devices
               mInput->SetSelection(0);
               SetDevices(&inMaps[i], NULL);
            }
            break;
         }
      }
   }

   devName = gPrefs->Read(wxT("/AudioIO/PlaybackDevice"), wxT(""));
   sourceName = gPrefs->Read(wxT("/AudioIO/PlaybackSource"), wxT(""));
   if (sourceName == wxT(""))
      desc = devName;
   else
      desc = devName + wxT(": ") + sourceName;

   if (mOutput->GetStringSelection() != desc &&
       mOutput->FindString(desc) != wxNOT_FOUND) {
      mOutput->SetStringSelection(desc);
   } else if (mOutput->GetStringSelection() != desc &&
              mOutput->GetCount()) {
      for (size_t i = 0; i < outMaps.size(); i++) {
         if (outMaps[i].hostString == hostName &&
             MakeDeviceSourceString(&outMaps[i]) == mOutput->GetString(0)) {
            // use the default.  It should exist but check just in case, falling back on the 0 index.
            DeviceSourceMap *defaultMap = DeviceManager::Instance()->GetDefaultOutputDevice(outMaps[i].hostIndex);
            if (defaultMap) {
               mOutput->SetStringSelection(MakeDeviceSourceString(defaultMap));
               SetDevices(NULL, defaultMap);
            } else {
               //use the first item (0th index) if we have no familiar devices
               mOutput->SetSelection(0);
               SetDevices(NULL, &outMaps[i]);
            }
            break;
         }
      }
   }

   long oldChannels, newChannels;
   oldChannels = mInputChannels->GetSelection() + 1;
   gPrefs->Read(wxT("/AudioIO/RecordChannels"), &newChannels, 0);
   if (newChannels > 0 && oldChannels != newChannels)
      mInputChannels->SetSelection(newChannels - 1);

   if (hostName != wxT("") && mHost->GetStringSelection() != hostName)
      mHost->SetStringSelection(hostName);

   RegenerateTooltips();

   // Set label to pull in language change
   SetLabel(_("Device"));

   // Give base class a chance
   ToolBar::UpdatePrefs();

   Layout();
   Refresh();
}


void DeviceToolBar::EnableDisableButtons()
{
   if (gAudioIO) {
      // we allow changes when monitoring, but not when recording
      bool audioStreamActive = gAudioIO->IsStreamActive() && !gAudioIO->IsMonitoring();

      // Here we should relinquish focus
      if (audioStreamActive) {
         wxWindow *focus = wxWindow::FindFocus();
         if (focus == mHost || focus == mInput || focus == mOutput || focus == mInputChannels) {
            AudacityProject *activeProject = GetActiveProject();
            if (activeProject) {
               activeProject->GetTrackPanel()->SetFocus();
            }
         }
      }

      mHost->Enable(!audioStreamActive);
      mInput->Enable(!audioStreamActive);
      mOutput->Enable(!audioStreamActive);
      mInputChannels->Enable(!audioStreamActive);
   }
}

void DeviceToolBar::SetNames()
{
   /* i18n-hint: (noun) It's the device used for playback.*/
   mOutput->SetName(_("Playback Device"));
   /* i18n-hint: (noun) It's the device used for recording.*/
   mInput->SetName(_("Recording Device"));
   mHost->SetName(_("Audio Host"));
   mInputChannels->SetName(_("Recording Channels"));
}

void DeviceToolBar::RegenerateTooltips()
{
#if wxUSE_TOOLTIPS
   SetNames();
   mOutput->SetToolTip(mOutput->GetName() + wxT(" - ") + mOutput->GetStringSelection());
   mInput->SetToolTip(mInput->GetName() + wxT(" - ") + mInput->GetStringSelection());
   mHost->SetToolTip(mHost->GetName() + wxT(" - ") + mHost->GetStringSelection());
   mInputChannels->SetToolTip(mInputChannels->GetName() + wxT(" - ") + mInputChannels->GetStringSelection());
#endif
}

bool DeviceToolBar::Layout()
{
   bool ret;
   RepositionCombos();
   ret = ToolBar::Layout();
   return ret;
}

// returns true if the combo is constrained and false otherwise
// @param toolbarWidth the width of the toolbar in pixels
// @param ratio an in/out for the desired and resultant width ratio.
// @param flex the amount of extra space allowed to have past the available.
//             the amount used is subtracted.
static bool RepositionCombo(wxWindow *combo, int toolbarWidth, wxSize desiredSize,
                            float &ratio, float &flex, int marginPixels, bool changesRatio)
{
   float ratioChange;
   bool constrained = false;

   // push margin pixels
   desiredSize.x += marginPixels;

   // truncate the window size if necessary
   if (desiredSize.x > toolbarWidth * (flex + ratio)) {
      constrained = true;
      desiredSize.SetWidth(toolbarWidth * (flex + ratio));
      if (desiredSize.GetWidth() - marginPixels < 0)
         desiredSize.SetWidth(marginPixels);
   }

   // keep track of how much space gained or lost so it can be used by other combos.
   if (changesRatio) {
      ratioChange = (desiredSize.x / ((float) toolbarWidth)) - ratio;
      ratio += ratioChange;
      flex  -= ratioChange;
   }

   // pop the margin pixels
   desiredSize.x -= marginPixels;

   combo->SetMinSize(desiredSize);

   return constrained;
}

//These don't add up to 1 because there is a bit of margin that we allow
//the layout sizer to handle.
#define kHostWidthRatio 0.13f
#define kInputWidthRatio 0.32f
#define kOutputWidthRatio 0.32f
#define kChannelsWidthRatio 0.18f

void DeviceToolBar::RepositionCombos()
{
   int w, h, dockw, dockh;
   float ratioUnused;
   bool constrained = true;
   wxWindow *window;
   wxSize desiredInput, desiredOutput, desiredHost, desiredChannels;
   float hostRatio, outputRatio, inputRatio, channelsRatio;
   // if the toolbar is docked then the width we should use is the project width.
   // as the toolbar's with can extend past this.
   GetClientSize(&w, &h);

   // FIXME: Note that there's some bug in here, in that even if the prefs show the toolbar
   // docked, on initialization, this call to IsDocked() returns false.
   if (IsDocked()) {
      // If the toolbar is docked its width can be larger than what is actually viewable
      // So take the min.  We don't need to worry about having another toolbar to the left off us
      // because if we are larger than the dock size we always get our own row.
      // and if smaller then we don't use the dock size (because we take the min).
      window = GetDock();
      window->GetClientSize(&dockw, &dockh);
      if (dockw < w)
         w = dockw;
   }
   // subtract the main grabber on the left and the resizer as well
   w -= grabberWidth + GetResizeGrabberWidth();
   if (w <= 0)
      return;

   // set up initial sizes and ratios
   // Note that the y values of the desired sizes are not changed, so that the height
   // of the toolbar is not changed
   hostRatio     = kHostWidthRatio;
   inputRatio    = kInputWidthRatio;
   outputRatio   = kOutputWidthRatio;
   channelsRatio = kChannelsWidthRatio;

   desiredHost.x     = mHost->GetBestSize().x;
   desiredHost.y     = mHost->GetSize().y;
   desiredInput.x    = mInput->GetBestSize().x;
   desiredInput.y    = mInput->GetSize().y;
   desiredOutput.x   = mOutput->GetBestSize().x;
   desiredOutput.y   = mOutput->GetSize().y;
   desiredChannels.x = mInputChannels->GetBestSize().x;
   desiredChannels.y = mInputChannels->GetSize().y;

   // wxGtk (Gnome) has larger comboboxes than the other platforms.  For DeviceToolBar this prevents
   // the toolbar docking on a single height row. So we shrink it to prevent this.
#ifdef __WXGTK__
   desiredHost.SetHeight(mHost->GetBestSize().y -4);
   desiredInput.SetHeight(desiredHost.GetHeight());
   desiredOutput.SetHeight(desiredHost.GetHeight());
   desiredChannels.SetHeight(desiredHost.GetHeight());
#endif

   ratioUnused = 0.995f - (kHostWidthRatio + kInputWidthRatio + kOutputWidthRatio + kChannelsWidthRatio);
   int i = 0;
   // limit the amount of times we solve contraints to 5
   while (constrained && ratioUnused > 0.01f && i < 5) {
      i++;
      constrained = RepositionCombo(mHost,   w,   desiredHost,   hostRatio, ratioUnused, 0, true);
      constrained |= RepositionCombo(mInput,  w,  desiredInput,  inputRatio, ratioUnused, mRecordBitmap->GetWidth(), true);
      constrained |= RepositionCombo(mOutput, w, desiredOutput, outputRatio, ratioUnused, mPlayBitmap->GetWidth(), true);
      constrained |= RepositionCombo(mInputChannels, w, desiredChannels, channelsRatio, ratioUnused, 0, true);
   }

   Update();
}

void DeviceToolBar::FillHosts()
{
   wxArrayString hosts;
   size_t i;

   const std::vector<DeviceSourceMap> &inMaps  = DeviceManager::Instance()->GetInputDeviceMaps();
   const std::vector<DeviceSourceMap> &outMaps = DeviceManager::Instance()->GetOutputDeviceMaps();
   // go over our lists add the host to the list if it isn't there yet
   for (i = 0; i < inMaps.size(); i++)
      if (hosts.Index(inMaps[i].hostString) == wxNOT_FOUND)
         hosts.Add(inMaps[i].hostString);
   for (i = 0; i < outMaps.size(); i++)
      if (hosts.Index(outMaps[i].hostString) == wxNOT_FOUND)
         hosts.Add(outMaps[i].hostString);

   mHost->Clear();
   mHost->Append(hosts);

   if (hosts.GetCount() == 0)
      mHost->Enable(false);

   mHost->InvalidateBestSize();
   mHost->SetMaxSize(mHost->GetBestSize());
}

void DeviceToolBar::FillHostDevices()
{
   const std::vector<DeviceSourceMap> &inMaps  = DeviceManager::Instance()->GetInputDeviceMaps();
   const std::vector<DeviceSourceMap> &outMaps = DeviceManager::Instance()->GetOutputDeviceMaps();

   //read what is in the prefs
   wxString host = gPrefs->Read(wxT("/AudioIO/Host"), wxT(""));
   size_t i;
   int foundHostIndex = -1;

   // if the host is not in the hosts combo then we rescanned.
   // set it to blank so we search for another host.
   if (mHost->FindString(host) == wxNOT_FOUND)
      host = wxT("");

   for (i = 0; i < outMaps.size(); i++) {
      if (outMaps[i].hostString == host) {
         foundHostIndex = outMaps[i].hostIndex;
         break;
      }
   }

   if (foundHostIndex == -1) {
      for (i = 0; i < inMaps.size(); i++) {
         if (inMaps[i].hostString == host) {
            foundHostIndex = inMaps[i].hostIndex;
            break;
         }
      }
   }

   // If no host was found based on the prefs device host, load the first available one
   if (foundHostIndex == -1) {
      if (outMaps.size())
         foundHostIndex = outMaps[0].hostIndex;
      else if (inMaps.size())
         foundHostIndex = inMaps[0].hostIndex;
   }

   // Make sure in/out are clear in case no host was found
   mInput->Clear();
   mOutput->Clear();

   // If we still have no host it means no devices, in which case do nothing.
   if (foundHostIndex == -1)
      return;

   // Repopulate the Input/Output device list available to the user
   for (i = 0; i < inMaps.size(); i++) {
      if (foundHostIndex == inMaps[i].hostIndex) {
         mInput->Append(MakeDeviceSourceString(&inMaps[i]));
         if (host == wxT("")) {
            host = inMaps[i].hostString;
            gPrefs->Write(wxT("/AudioIO/Host"), host);
            mHost->SetStringSelection(host);
         }
      }
   }
   mInput->Enable(mInput->GetCount() ? true : false);

   mInput->InvalidateBestSize();
   mInput->SetMaxSize(mInput->GetBestSize());

   for (i = 0; i < outMaps.size(); i++) {
      if (foundHostIndex == outMaps[i].hostIndex) {
         mOutput->Append(MakeDeviceSourceString(&outMaps[i]));
         if (host == wxT("")) {
            host = outMaps[i].hostString;
            gPrefs->Write(wxT("/AudioIO/Host"), host);
            gPrefs->Flush();
            mHost->SetStringSelection(host);
         }
      }
   }
   mOutput->Enable(mOutput->GetCount() ? true : false);

   mOutput->InvalidateBestSize();
   mOutput->SetMaxSize(mOutput->GetBestSize());

   // The setting of the Device is left up to OnChoice
}

//return 1 if host changed, 0 otherwise.
int DeviceToolBar::ChangeHost()
{
   int hostSelectionIndex;
   hostSelectionIndex = mHost->GetSelection();

   wxString oldHost = gPrefs->Read(wxT("/AudioIO/Host"), wxT(""));
   wxString newHost = hostSelectionIndex >= 0 ? mHost->GetString(hostSelectionIndex) :
                                                oldHost;

   if (oldHost == newHost)
      return 0;

   //change the host and switch to correct devices.
   gPrefs->Write(wxT("/AudioIO/Host"), newHost);
   gPrefs->Flush();

   // populate the devices
   FillHostDevices();

   return 1;
}

void DeviceToolBar::FillInputChannels()
{
   const std::vector<DeviceSourceMap> &inMaps  = DeviceManager::Instance()->GetInputDeviceMaps();
   wxString host     = gPrefs->Read(wxT("/AudioIO/Host"), wxT(""));
   wxString device   = gPrefs->Read(wxT("/AudioIO/RecordingDevice"), wxT(""));
   wxString source   = gPrefs->Read(wxT("/AudioIO/RecordingSource"), wxT(""));
   long oldChannels = 2, newChannels;

   gPrefs->Read(wxT("/AudioIO/RecordChannels"), &oldChannels);
   int index = -1;
   size_t i, j;
   mInputChannels->Clear();
   for (i = 0; i < inMaps.size(); i++) {
      if (source == inMaps[i].sourceString &&
          device == inMaps[i].deviceString &&
          host   == inMaps[i].hostString) {

         // add one selection for each channel of this source
         for (j = 0; j < (unsigned int) inMaps[i].numChannels; j++) {
            wxString name;

            if (j == 0) {
               name = _("1 (Mono) Recording Channel");
            }
            else if (j == 1) {
               name = _("2 (Stereo) Recording Channels");
            }
            else {
               name = wxString::Format(wxT("%d"), (int) j + 1);
            }
            mInputChannels->Append(name);
         }
         newChannels = inMaps[i].numChannels;
         if (oldChannels <= newChannels && oldChannels >= 1)
            newChannels = oldChannels;
         if (newChannels >= 1)
            mInputChannels->SetSelection(newChannels - 1);
         gPrefs->Write(wxT("/AudioIO/RecordChannels"), newChannels);
         mInputChannels->Enable(mInputChannels->GetCount() ? true : false);
         index = i;
         break;
      }
   }
   if (index == -1)
      mInputChannels->Enable(false);

   mInputChannels->InvalidateBestSize();
   mInputChannels->SetMaxSize(mInputChannels->GetBestSize());
}

void DeviceToolBar::SetDevices(const DeviceSourceMap *in, const DeviceSourceMap *out)
{
   if (in) {
      gPrefs->Write(wxT("/AudioIO/RecordingDevice"), in->deviceString);
      gPrefs->Write(wxT("/AudioIO/RecordingSourceIndex"), in->sourceIndex);
      if (in->totalSources >= 1) {
         gPrefs->Write(wxT("/AudioIO/RecordingSource"), in->sourceString);
      } else {
         gPrefs->Write(wxT("/AudioIO/RecordingSource"), wxT(""));
      }
      gPrefs->Flush();

      FillInputChannels();
   }

   if (out) {
      gPrefs->Write(wxT("/AudioIO/PlaybackDevice"), out->deviceString);
      if (out->totalSources >= 1) {
         gPrefs->Write(wxT("/AudioIO/PlaybackSource"), out->sourceString);
      } else {
         gPrefs->Write(wxT("/AudioIO/PlaybackSource"), wxT(""));
      }
      gPrefs->Flush();
   }
}

void DeviceToolBar::ChangeDevice(bool isInput)
{
   int newIndex = -1;
   wxChoice *combo = isInput ? mInput :mOutput;
   size_t i;

   int selectionIndex  = mInput->GetSelection();
   wxString host       = gPrefs->Read(wxT("/AudioIO/Host"), wxT(""));
   const std::vector<DeviceSourceMap> &maps = isInput ? DeviceManager::Instance()->GetInputDeviceMaps()
                                                      : DeviceManager::Instance()->GetOutputDeviceMaps();

   // Find device indices for input and output
   if (selectionIndex >= 0 ) {
      wxString newDevice = combo->GetStringSelection();
      for (i = 0; i < maps.size(); ++i) {
         wxString name;
         name = MakeDeviceSourceString(&maps[i]);
         if (name == newDevice && maps[i].hostString == host) {
            newIndex = i;
         }
      }
   }

   if (newIndex < 0) {
      wxLogDebug(wxT("DeviceToolBar::OnChoice(): couldn't find device indices"));
      return;
   }

   SetDevices(isInput ? &maps[newIndex] : NULL,
              isInput ? NULL            : &maps[newIndex]);
}

void DeviceToolBar::OnChoice(wxCommandEvent &event)
{
   wxObject *eventObject = event.GetEventObject();
   //if we've changed hosts, we've handled the device switching already.
   if (eventObject == mHost) {
      ChangeHost();
   } else if (eventObject == mInputChannels) {
      int channelsSelectionIndex = mInputChannels->GetSelection();
      if (channelsSelectionIndex >= 0)
         gPrefs->Write(wxT("/AudioIO/RecordChannels"),channelsSelectionIndex + 1);
   } else if (eventObject == mInput) {
      ChangeDevice(true);
   }
   else if (eventObject == mOutput) {
      ChangeDevice(false);
   }

   if (gAudioIO) {
      // We cannot have gotten here if gAudioIO->IsAudioTokenActive(),
      // per the setting of AudioIONotBusyFlag and AudioIOBusyFlag in
      // AudacityProject::GetUpdateFlags().
      // However, we can have an invalid audio token (so IsAudioTokenActive()
      // is false), but be monitoring.
      // If monitoring, have to stop the stream, so HandleDeviceChange() can work.
      // We could disable the Preferences command while monitoring, i.e.,
      // set AudioIONotBusyFlag/AudioIOBusyFlag according to monitoring, as well.
      // Instead allow it because unlike recording, for example, monitoring
      // is not clearly something that should prohibit changing device.
      // TODO: We *could* be smarter in this method and call HandleDeviceChange()
      // only when the device choices actually changed. True of lots of prefs!
      // As is, we always stop monitoring before handling the device change.
      if (gAudioIO->IsMonitoring())
      {
         gAudioIO->StopStream();
         while (gAudioIO->IsBusy())
            wxMilliSleep(100);
      }
      gAudioIO->HandleDeviceChange();
   }

   // Update all projects' DeviceToolBar.
   for (size_t i = 0; i < gAudacityProjects.size(); i++) {
      gAudacityProjects[i]->GetDeviceToolBar()->UpdatePrefs();
   }
}

void DeviceToolBar::ShowInputDialog()
{
   ShowComboDialog(mInput, wxString(_("Select Recording Device")));
}
void DeviceToolBar::ShowOutputDialog()
{
   ShowComboDialog(mOutput, wxString(_("Select Playback Device")));
}
void DeviceToolBar::ShowHostDialog()
{
   ShowComboDialog(mHost, wxString(_("Select Audio Host")));
}
void DeviceToolBar::ShowChannelsDialog()
{
   ShowComboDialog(mInputChannels, wxString(_("Select Recording Channels")));
}

void DeviceToolBar::ShowComboDialog(wxChoice *combo, const wxString &title)
{
   if (!combo || combo->GetCount() == 0) {
      wxMessageBox(_("Device information is not available."));
      return;
   }

#if USE_PORTMIXER
   wxArrayString inputSources = combo->GetStrings();

   wxDialogWrapper dlg(nullptr, wxID_ANY, title);
   dlg.SetName(dlg.GetTitle());
   ShuttleGui S(&dlg, eIsCreating);
   wxChoice *c;

   S.StartVerticalLay(true);
   {
     S.StartHorizontalLay(wxCENTER, false);
      {
         c = S.AddChoice(combo->GetName(),
                         combo->GetStringSelection(),
                         &inputSources);
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();
   S.AddStandardButtons();

   dlg.GetSizer()->SetSizeHints(&dlg);
   dlg.Center();

   if (dlg.ShowModal() == wxID_OK)
   {
      wxCommandEvent dummyEvent;
      dummyEvent.SetEventObject(combo);
      // SetSelection() doesn't send an event, so we call OnChoice explicitly
      combo->SetSelection(c->GetSelection());
      OnChoice(dummyEvent);
   }
#endif
}
