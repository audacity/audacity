/**********************************************************************

  Audacity: A Digital Audio Editor

  DeviceToolBar.cpp

  Dominic Mazzoni
 
*******************************************************************//*!

\class DeviceToolBar
\brief A toobar to allow easier changing of input and output devices .

*//*******************************************************************/


#include "../Audacity.h"

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

#include "DeviceToolBar.h"

#include "../AColor.h"
#include "../AllThemeResources.h"
#include "../AudioIO.h"
#include "../ImageManipulation.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Theme.h"

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
: ToolBar(DeviceBarID, _("Device"), wxT("Device"))
{
}

DeviceToolBar::~DeviceToolBar()
{
   delete mPlayBitmap;
   delete mRecordBitmap;
}

void DeviceToolBar::Create(wxWindow *parent)
{
   ToolBar::Create(parent);
}

void DeviceToolBar::RecreateTipWindows()
{
}

static wxString MakeDeviceSourceString(DeviceSourceMap *map)
{
   wxString ret;
   ret = map->deviceString;
   if (map->totalSources > 1)
      ret += wxString(": ", wxConvLocal) + map->sourceString;
   
   return ret;
}

//Port Audio requires we open the stream with a callback or a lot of devices will fail
//as this means open in blocking mode, so we use a dummy one.
static int DummyPaStreamCallback(
    const void *input, void *output,
    unsigned long frameCount,
    const PaStreamCallbackTimeInfo* timeInfo,
    PaStreamCallbackFlags statusFlags,
    void *userData )
{
   return 0;
}

static void FillHostDeviceInfo(DeviceSourceMap *map, const PaDeviceInfo *info, int deviceIndex)
{
   wxString hostapiName(Pa_GetHostApiInfo(info->hostApi)->name, wxConvLocal);
   wxString infoName(info->name, wxConvLocal);

   map->deviceIndex  = deviceIndex;
   map->hostIndex    = info->hostApi;
   map->deviceString = infoName;
   map->hostString   = hostapiName;
   map->numChannels  = info->maxInputChannels;
}

static void AddSourcesFromStream(int deviceIndex, const PaDeviceInfo *info, std::vector<DeviceSourceMap> *maps, PaStream *stream)
{
   int i;
   PxMixer *portMixer;
   DeviceSourceMap map;

   map.sourceIndex  = -1;
   map.totalSources = 0;
   FillHostDeviceInfo(&map, info, deviceIndex);
   portMixer = Px_OpenMixer(stream, 0);
   if (!portMixer) {
      maps->push_back(map);
      return;
   }

   //if there is only one source, we don't need to concatenate the source
   //or enumerate, because it is something meaningless like 'master' 
   //(as opposed to 'mic in' or 'line in'), and the user doesn't have any choice.
   //note that some devices have no input sources at all but are still valid.
   //the behavior we do is the same for 0 and 1 source cases.
   map.totalSources = Px_GetNumInputSources(portMixer);
    
   if (map.totalSources <= 1) {
      map.sourceIndex = 0;
      maps->push_back(map);
   } else {
      //open up a stream with the device so portmixer can get the info out of it.
      for (i = 0; i < map.totalSources; i++) {
         map.sourceIndex  = i;
         map.sourceString = wxString(Px_GetInputSourceName(portMixer, i), wxConvLocal);
         maps->push_back(map);
      }
   }
   Px_CloseMixer(portMixer);
}

static void AddSources(int deviceIndex, int rate, wxArrayString *hosts, std::vector<DeviceSourceMap> *maps, int isInput)
{
   int error;
   DeviceSourceMap map;
   wxString hostDevName;
   const PaDeviceInfo *info = Pa_GetDeviceInfo(deviceIndex);

   hostDevName = DeviceName(info);
   // This tries to open the device with the samplerate worked out above, which
   // will be the highest available for play and record on the device, or
   // 44.1kHz if the info cannot be fetched.

   PaStream *stream = NULL;

   PaStreamParameters parameters;

   parameters.device = deviceIndex;
   parameters.sampleFormat = paFloat32;
   parameters.hostApiSpecificStreamInfo = NULL;
   parameters.channelCount = 1;
   if (info)
      parameters.suggestedLatency = isInput ? info->defaultLowInputLatency:
                                              info->defaultLowOutputLatency;
   else
      parameters.suggestedLatency = DEFAULT_LATENCY_CORRECTION/1000.0;
      // try opening for record and playback
   error = Pa_OpenStream(&stream,
                         isInput ? &parameters : NULL,
                         isInput ? NULL : &parameters,
                         rate, paFramesPerBufferUnspecified,
                         paClipOff | paDitherOff,
                         DummyPaStreamCallback, NULL);
   if (stream) {
      AddSourcesFromStream(deviceIndex, info, maps, stream);
      Pa_CloseStream(stream);
   } else {
      map.sourceIndex  = -1;
      map.totalSources = 0;
      FillHostDeviceInfo(&map, info, deviceIndex);
      maps->push_back(map);
   }
   //add the host to the list if it isn't there yet
   wxString hostName(Pa_GetHostApiInfo(info->hostApi)->name, wxConvLocal);
   if (hosts->Index(hostName) == wxNOT_FOUND) {
      hosts->Add(hostName);
   }
}

void DeviceToolBar::Populate()
{
   int i;
   wxArrayString inputs;
   wxArrayString outputs;
   wxArrayString hosts;
   wxArrayString channels;

   channels.Add(wxT("1 (Mono)"));

   int nDevices = Pa_GetDeviceCount();

   //The heirarchy for devices is Host/device/source.
   //Some newer systems aggregate this.
   //So we need to call port mixer for every device to get the sources
   for (i = 0; i < nDevices; i++) {
      const PaDeviceInfo *info = Pa_GetDeviceInfo(i);
      if (info->maxOutputChannels > 0) {
         AddSources(i, info->defaultSampleRate, &hosts, &mOutputDeviceSourceMaps, 0);
      }

      if (info->maxInputChannels > 0) {
         AddSources(i, info->defaultSampleRate, &hosts, &mInputDeviceSourceMaps, 1);
      }
   }

   // Hosts
   mHost = new wxChoice(this,
                        wxID_ANY,
                        wxDefaultPosition,
                        wxDefaultSize,
                        hosts);
   mHost->SetName(_("Audio Host"));

   Add(mHost, 0, wxALIGN_CENTER);
   if (hosts.GetCount() == 0)
      mHost->Enable(false);

   // Output device
   mPlayBitmap = new wxBitmap(theTheme.Bitmap(bmpSpeaker));

   Add(new wxStaticBitmap(this,
                          wxID_ANY, 
                          *mPlayBitmap), 0, wxALIGN_CENTER);

   mOutput = new wxChoice(this,
                               wxID_ANY,
                               wxDefaultPosition,
                               wxDefaultSize,
                               outputs);
   mOutput->SetName(_("Output Device"));

   Add(mOutput, 0, wxALIGN_CENTER);
   if (outputs.GetCount() == 0)
      mOutput->Enable(false);

   // Input device
   mRecordBitmap = new wxBitmap(theTheme.Bitmap(bmpMic));

   Add(new wxStaticBitmap(this,
                          wxID_ANY, 
                          *mRecordBitmap), 0, wxALIGN_CENTER);

   mInput = new wxChoice(this,
                         wxID_ANY,
                         wxDefaultPosition,
                         wxDefaultSize,
                         inputs);
   mInput->SetName(_("Input Device"));
   Add(mInput, 0, wxALIGN_CENTER);
   if (inputs.GetCount() == 0)
      mInput->Enable(false);


   wxStaticText *channelsLabel = new wxStaticText(this, wxID_ANY, wxT("# Channels:"),
                                                 wxDefaultPosition, wxDefaultSize, 
                                                 wxALIGN_LEFT);
   Add(channelsLabel, 0, wxALIGN_CENTER);

   mInputChannels = new wxChoice(this,
                         wxID_ANY,
                         wxDefaultPosition,
                         wxDefaultSize,
                         channels);
   mInputChannels->SetName(_("Input Channels"));
   Add(mInputChannels, 0, wxALIGN_CENTER);
   // hide the number of channels until we have some to display
   mInputChannels->Enable(false);

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

   FillHostDevices();
   FillInputChannels();
}

void DeviceToolBar::OnFocus(wxFocusEvent &event)
{
   wxCommandEvent e(EVT_CAPTURE_KEYBOARD);

   if (event.GetEventType() == wxEVT_KILL_FOCUS) {
      e.SetEventType(EVT_RELEASE_KEYBOARD);
   }
   e.SetEventObject(this);
   GetParent()->GetEventHandler()->ProcessEvent(e);

   Refresh(false);

   event.Skip();
}

void DeviceToolBar::OnCaptureKey(wxCommandEvent &event)
{
   wxKeyEvent *kevent = (wxKeyEvent *)event.GetEventObject();
   int keyCode = kevent->GetKeyCode();

   // Pass UP/DOWN/LEFT/RIGHT through for input/output choice
   if (FindFocus() == mOutput && (keyCode == WXK_LEFT || keyCode == WXK_RIGHT
                                 || keyCode == WXK_UP || keyCode == WXK_DOWN)) {
      return;
   }
   if (FindFocus() == mInput && (keyCode == WXK_LEFT || keyCode == WXK_RIGHT
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

   hostName = gPrefs->Read(wxT("/AudioIO/Host"), wxT(""));
   devName = gPrefs->Read(wxT("/AudioIO/RecordingDevice"), wxT(""));
   sourceName = gPrefs->Read(wxT("/AudioIO/RecordingSource"), wxT(""));
   if (sourceName == wxT(""))
      desc = devName;
   else
      desc = devName + wxString(": ", wxConvLocal) + sourceName; 

   if (mInput->FindString(desc) != wxNOT_FOUND)
      mInput->SetStringSelection(desc);
   else if (mInput->GetCount()) {
      //use the 0th index if we have no familiar devices
      mInput->SetSelection(0);
      for (size_t i = 0; i < mInputDeviceSourceMaps.size(); i++) {
         if (mInputDeviceSourceMaps[i].hostString == hostName &&
             MakeDeviceSourceString(&mInputDeviceSourceMaps[i]) == mInput->GetString(0)) {
            SetDevices(&mInputDeviceSourceMaps[i], NULL);
            break;
         }
      }
   }

   devName = gPrefs->Read(wxT("/AudioIO/PlaybackDevice"), wxT(""));
   sourceName = gPrefs->Read(wxT("/AudioIO/PlaybackSource"), wxT(""));
   if (sourceName == wxT(""))
      desc = devName;
   else
      desc = devName + wxString(": ", wxConvLocal) + sourceName; 
   mOutput->SetStringSelection(desc);

   if (mOutput->FindString(desc) != wxNOT_FOUND)
      mOutput->SetStringSelection(desc);
   else if (mOutput->GetCount()) {
      //use the 0th index if we have no familiar devices
      mOutput->SetSelection(0);
      for (size_t i = 0; i < mOutputDeviceSourceMaps.size(); i++) {
         if (mOutputDeviceSourceMaps[i].hostString == hostName &&
             MakeDeviceSourceString(&mOutputDeviceSourceMaps[i]) == mOutput->GetString(0)) {
            SetDevices(NULL, &mOutputDeviceSourceMaps[i]);
            break;
         }
      }
   }

   mHost->SetStringSelection(hostName);

   RegenerateTooltips();

   // Set label to pull in language change
   SetLabel(_("Device"));

   // Give base class a chance
   ToolBar::UpdatePrefs();
}

void DeviceToolBar::RegenerateTooltips()
{
#if wxUSE_TOOLTIPS
   mOutput->SetToolTip(_("Output Device"));
   mInput->SetToolTip(_("Input Device"));
   mHost->SetToolTip(_("Audio Host"));
#endif
}

void DeviceToolBar::FillHostDevices()
{
   //read what is in the prefs
   wxString host = gPrefs->Read(wxT("/AudioIO/Host"), wxT(""));
   size_t i;
   int foundHostIndex = -1;
   for (i = 0; i < mOutputDeviceSourceMaps.size(); i++) {
      if (mOutputDeviceSourceMaps[i].hostString == host) {
         foundHostIndex = mOutputDeviceSourceMaps[i].hostIndex;
         break;
      }
   }
   
   if (foundHostIndex == -1) {
      for (i = 0; i < mInputDeviceSourceMaps.size(); i++) {
         if (mInputDeviceSourceMaps[i].hostString == host) {
            foundHostIndex = mInputDeviceSourceMaps[i].hostIndex;
            break;
         }
      } 
   }

   // If no host was found based on the prefs device host, load the first available one
   if (foundHostIndex == -1) {
      if (mOutputDeviceSourceMaps.size())
         foundHostIndex = mOutputDeviceSourceMaps[0].hostIndex;
      else if (mInputDeviceSourceMaps.size())
         foundHostIndex = mInputDeviceSourceMaps[0].hostIndex;
   }

   // If we still have no host it means no devices, in which case do nothing.
   if (foundHostIndex == -1)
      return;

   // Repopulate the Input/Output device list available to the user
   mInput->Clear();
   for (i = 0; i < mInputDeviceSourceMaps.size(); i++) {
      if (foundHostIndex == mInputDeviceSourceMaps[i].hostIndex)
         mInput->Append(MakeDeviceSourceString(&mInputDeviceSourceMaps[i]));
   }
   mInput->Enable(mInput->GetCount() ? true : false);
   mInput->SetSize(mInput->GetBestFittingSize());
//   mInput->Layout();
   mOutput->Clear();
   for (i = 0; i < mOutputDeviceSourceMaps.size(); i++) {
      if (foundHostIndex == mOutputDeviceSourceMaps[i].hostIndex)
         mOutput->Append(MakeDeviceSourceString(&mOutputDeviceSourceMaps[i]));
   }
   mOutput->Enable(mOutput->GetCount() ? true : false);
   mOutput->SetSize(mOutput->GetBestFittingSize());
//   mOutput->Layout();

   // make the device display selection reflect the prefs if they exist
   UpdatePrefs();
   Layout();
   this->Refresh();
   Update();
   // The setting of the Device is left up to OnChoice
}

//return 1 if host changed, 0 otherwise.
int DeviceToolBar::ChangeHost()
{
   int hostSelectionIndex;
   hostSelectionIndex = mHost->GetSelection();

   wxString oldHost = gPrefs->Read(wxT("/AudioIO/Host"), wxT(""));
   wxString newHost = hostSelectionIndex >= 0 ? mHost->GetString(mHost->GetSelection()) :
                                                oldHost;
   
   if (oldHost == newHost)
      return 0;
   
   //change the host and switch to correct devices.
   gPrefs->Write(wxT("/AudioIO/Host"), newHost);
   FillHostDevices();

   return 1;
}

void DeviceToolBar::FillInputChannels()
{
   wxString host     = gPrefs->Read(wxT("/AudioIO/Host"), wxT(""));
   wxString device   = gPrefs->Read(wxT("/AudioIO/RecordingDevice"), wxT(""));
   wxString source   = gPrefs->Read(wxT("/AudioIO/RecordingSource"), wxT(""));
   long oldChannels = 1, newChannels;
   
   gPrefs->Read(wxT("/AudioIO/RecordChannels"), &oldChannels);
   int index = -1;
   size_t i, j;
   mInputChannels->Clear();
   for (i = 0; i < mInputDeviceSourceMaps.size(); i++) {
      if (source == mInputDeviceSourceMaps[i].sourceString &&
          device == mInputDeviceSourceMaps[i].deviceString &&
          host   == mInputDeviceSourceMaps[i].hostString) {

         // add one selection for each channel of this source
         for (j = 0; j < mInputDeviceSourceMaps[i].numChannels; j++) {
            wxString name;

            if (j == 0) {
               name = _("1 (Mono)");
            }
            else if (j == 1) {
               name = _("2 (Stereo)");
            }
            else {
               name = wxString::Format(wxT("%d"), j + 1);
            }
            mInputChannels->Append(name);
         }
         newChannels = mInputDeviceSourceMaps[i].numChannels;
         if (oldChannels < newChannels && oldChannels >= 1)
            newChannels = oldChannels;
         mInputChannels->SetSelection(newChannels - 1);
         gPrefs->Write(wxT("/AudioIO/RecordChannels"), newChannels);
         mInputChannels->Enable(mInputChannels->GetCount() ? true : false);
         index = i;
         break;
      }
   }
   if (index == -1)
      mInputChannels->Enable(false);
}
void DeviceToolBar::SetDevices(DeviceSourceMap *in, DeviceSourceMap *out)
{
   if (in) {
      gPrefs->Write(wxT("/AudioIO/RecordingDevice"), in->deviceString);
      gPrefs->Write(wxT("/AudioIO/RecordingSourceIndex"), in->sourceIndex);
      if (in->sourceIndex >= 0) {
         gPrefs->Write(wxT("/AudioIO/RecordingSource"),in->sourceString);
      } else
         gPrefs->Write(wxT("/AudioIO/RecordingSource"), wxT(""));
      FillInputChannels();
   }

   if (out) {
      gPrefs->Write(wxT("/AudioIO/PlaybackDevice"), out->deviceString);
      if (out->sourceIndex >= 0) {
         gPrefs->Write(wxT("/AudioIO/PlaybackSource"), out->sourceString);
      } else
         gPrefs->Write(wxT("/AudioIO/RecordingSource"), wxT(""));
   }
}

void DeviceToolBar::OnChoice(wxCommandEvent &event)
{
   int inputSelectionIndex;
   int outputSelectionIndex;
   int channelsSelectionIndex;

   //if we've changed hosts, we've handled the device switching already.
   if (!ChangeHost()) {
      inputSelectionIndex  = mInput->GetSelection();
      outputSelectionIndex = mOutput->GetSelection();
      channelsSelectionIndex = mInputChannels->GetSelection();

      if (channelsSelectionIndex >= 0) {
         gPrefs->Write(wxT("/AudioIO/RecordChannels"),
                       channelsSelectionIndex + 1);
      }

      wxString host     = gPrefs->Read(wxT("/AudioIO/Host"), wxT(""));
      wxString oldInput = gPrefs->Read(wxT("/AudioIO/RecordingDevice"), wxT(""));
      wxString newInput = inputSelectionIndex >= 0 ? 
                             MakeDeviceSourceString(&mInputDeviceSourceMaps[inputSelectionIndex]):
                             oldInput;
      wxString oldOutput = gPrefs->Read(wxT("/AudioIO/PlaybackDevice"), wxT(""));
      wxString newOutput = outputSelectionIndex >= 0 ?
                              MakeDeviceSourceString(&mOutputDeviceSourceMaps[outputSelectionIndex]):
                              oldOutput;
      int newInIndex = -1, newOutIndex = -1;
      size_t i;

      // Find device indices for input and output
      for (i = 0; i < mInputDeviceSourceMaps.size(); ++i) {
         wxString name;
         name = MakeDeviceSourceString(&mInputDeviceSourceMaps[i]);
         if (name == newInput && mInputDeviceSourceMaps[i].hostString == host)
            newInIndex = i;
      }
      for (i = 0; i < mOutputDeviceSourceMaps.size(); ++i) {
         wxString name;
         name = MakeDeviceSourceString(&mOutputDeviceSourceMaps[i]);
         if (name == newOutput && mOutputDeviceSourceMaps[i].hostString == host)
            newOutIndex = i;
      }

      // This shouldn't happen for new choices (it's OK for old ones)
      if (newInIndex < 0 || newOutIndex < 0) {
         wxLogDebug(wxT("DeviceToolBar::OnChoice(): couldn't find device indices"));
         return;
      }

      SetDevices(&mInputDeviceSourceMaps[newInIndex], &mOutputDeviceSourceMaps[newOutIndex]);
   }

   if (gAudioIO)
      gAudioIO->HandleDeviceChange();
   GetActiveProject()->UpdatePrefs();
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 6a50243e-9fc9-4f0f-b344-bd3044dc09ad

