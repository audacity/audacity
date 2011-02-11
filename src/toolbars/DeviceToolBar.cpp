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
#include "ToolDock.h"

#include "../AColor.h"
#include "../AllThemeResources.h"
#include "../AudioIO.h"
#include "../ImageManipulation.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Theme.h"
#include "../widgets/Grabber.h"

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
}

DeviceToolBar::~DeviceToolBar()
{
   delete mPlayBitmap;
   delete mRecordBitmap;
}

void DeviceToolBar::Create(wxWindow *parent)
{
   ToolBar::Create(parent);

   // Simulate a size event to set initial meter placement/size
   wxSizeEvent dummy;
   OnSize(dummy);
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

static void FillHostDeviceInfo(DeviceSourceMap *map, const PaDeviceInfo *info, int deviceIndex, int isInput)
{
   wxString hostapiName(Pa_GetHostApiInfo(info->hostApi)->name, wxConvLocal);
   wxString infoName(info->name, wxConvLocal);

   map->deviceIndex  = deviceIndex;
   map->hostIndex    = info->hostApi;
   map->deviceString = infoName;
   map->hostString   = hostapiName;
   map->numChannels  = isInput ? info->maxInputChannels : info->maxOutputChannels;
}

static void AddSourcesFromStream(int deviceIndex, const PaDeviceInfo *info, std::vector<DeviceSourceMap> *maps, PaStream *stream)
{
   int i;
   PxMixer *portMixer;
   DeviceSourceMap map;

   map.sourceIndex  = -1;
   map.totalSources = 0;
   // Only inputs have sources, so we call FillHostDeviceInfo with a 1 to indicate this
   FillHostDeviceInfo(&map, info, deviceIndex, 1);
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

static bool IsInputDeviceAMapperDevice(const PaDeviceInfo *info)
{
   // For Windows only, portaudio returns the default mapper object
   // as the first index after a new hostApi index is detected (true for MME and DS)
   // this is a bit of a hack, but there's no other way to find out which device is a mapper,
   // I've looked at string comparisons, but if the system is in a different language this breaks.
#ifdef __WXMSW__
   static int lastHostApiTypeId = -1;
   int hostApiTypeId = Pa_GetHostApiInfo(info->hostApi)->type;
   if(hostApiTypeId != lastHostApiTypeId &&
      (hostApiTypeId == paMME || hostApiTypeId == paDirectSound)) {
      lastHostApiTypeId = hostApiTypeId;
      return true;
   }
#endif

   return false;
}

static void AddSources(int deviceIndex, int rate, wxArrayString *hosts, std::vector<DeviceSourceMap> *maps, int isInput)
{
   int error = 0;
   DeviceSourceMap map;
   const PaDeviceInfo *info = Pa_GetDeviceInfo(deviceIndex);

   // This tries to open the device with the samplerate worked out above, which
   // will be the highest available for play and record on the device, or
   // 44.1kHz if the info cannot be fetched.

   PaStream *stream = NULL;

   PaStreamParameters parameters;

   parameters.device = deviceIndex;
   parameters.sampleFormat = paFloat32;
   parameters.hostApiSpecificStreamInfo = NULL;
   parameters.channelCount = 1;

   // If the device is for input, open a stream so we can use portmixer to query
   // the number of inputs.  We skip this for outputs because there are no 'sources'
   // and some platforms (e.g. XP) have the same device for input and output, (while
   // Vista/Win7 seperate these into two devices with the same names (but different
   // portaudio indecies)
   // Also, for mapper devices we don't want to keep any sources, so check for it here
   if (isInput && !IsInputDeviceAMapperDevice(info)) {
      if (info)
         parameters.suggestedLatency = info->defaultLowInputLatency;
      else
         parameters.suggestedLatency = DEFAULT_LATENCY_CORRECTION/1000.0;

      error = Pa_OpenStream(&stream,
                            &parameters,
                            NULL,
                            rate, paFramesPerBufferUnspecified,
                            paClipOff | paDitherOff,
                            DummyPaStreamCallback, NULL);
   }

   if (stream && !error) {
      AddSourcesFromStream(deviceIndex, info, maps, stream);
      Pa_CloseStream(stream);
   } else {
      map.sourceIndex  = -1;
      map.totalSources = 0;
      FillHostDeviceInfo(&map, info, deviceIndex, isInput);
      maps->push_back(map);
   }

   if(error) {
      wxLogDebug(wxT("PortAudio stream error creating device list: ") +
                 map.hostString + wxT(":") + map.deviceString + wxT(": ") +
                 wxString(Pa_GetErrorText( (PaError)error), wxConvLocal));
   }

   //add the host to the list if it isn't there yet
   wxString hostName(Pa_GetHostApiInfo(info->hostApi)->name, wxConvLocal);
   if (hosts->Index(hostName) == wxNOT_FOUND) {
      hosts->Add(hostName);
   }
}

void DeviceToolBar::DeinitChildren()
{
   mPlayBitmap    = NULL;
   mRecordBitmap  = NULL;
   
   mInput         = NULL;
   mOutput        = NULL;
   mInputChannels = NULL;
   mHost          = NULL;

   mInputDeviceSourceMaps.clear();
   mOutputDeviceSourceMaps.clear();
}

void DeviceToolBar::Populate()
{
   int i;
   wxArrayString inputs;
   wxArrayString outputs;
   wxArrayString hosts;
   wxArrayString channels;

   DeinitChildren();

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
   mInputChannels->Connect(wxEVT_SET_FOCUS,
                 wxFocusEventHandler(DeviceToolBar::OnFocus),
                 NULL,
                 this);
   mInputChannels->Connect(wxEVT_KILL_FOCUS,
                 wxFocusEventHandler(DeviceToolBar::OnFocus),
                 NULL,
                 this);

   FillHostDevices();
   FillInputChannels();
   // make the device display selection reflect the prefs if they exist
   UpdatePrefs();
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
      desc = devName + wxString(": ", wxConvLocal) + sourceName; 

   if (mInput->GetStringSelection() != desc &&
       mInput->FindString(desc) != wxNOT_FOUND) {
      mInput->SetStringSelection(desc);
      FillInputChannels();
   } else if (mInput->GetStringSelection() != desc && mInput->GetCount()) {
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

   if (mOutput->GetStringSelection() != desc &&
       mOutput->FindString(desc) != wxNOT_FOUND) {
      mOutput->SetStringSelection(desc);
   } else if (mOutput->GetStringSelection() != desc &&
              mOutput->GetCount()) {
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

   long oldChannels = 1, newChannels;
   oldChannels = mInputChannels->GetSelection() + 1;
   gPrefs->Read(wxT("/AudioIO/RecordChannels"), &newChannels, 0);
   if (newChannels > 0 && oldChannels != newChannels)
      mInputChannels->SetSelection(newChannels - 1);

   mHost->SetStringSelection(hostName);

   RegenerateTooltips();

   // Set label to pull in language change
   SetLabel(_("Device"));

   // Give base class a chance
   ToolBar::UpdatePrefs();
}


void DeviceToolBar::EnableDisableButtons()
{
   if (gAudioIO) {
      // we allow changes when monitoring, but not when recording
      bool audioStreamActive = gAudioIO->IsStreamActive() && !gAudioIO->IsMonitoring();
      mHost->Enable(!audioStreamActive);
      mInput->Enable(!audioStreamActive);
      mOutput->Enable(!audioStreamActive);
      mInputChannels->Enable(!audioStreamActive);
   }
}

void DeviceToolBar::RegenerateTooltips()
{
#if wxUSE_TOOLTIPS
   mOutput->SetToolTip(_("Output Device"));
   mInput->SetToolTip(_("Input Device"));
   mHost->SetToolTip(_("Audio Host"));
   mInputChannels->SetToolTip(_("Input Channels"));
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
   combo->SetMaxSize(desiredSize);

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
   hostRatio     = kHostWidthRatio;
   inputRatio    = kInputWidthRatio;
   outputRatio   = kOutputWidthRatio;
   channelsRatio = kChannelsWidthRatio;

   desiredHost     = mHost->GetBestSize();
   desiredInput    = mInput->GetBestSize();
   desiredOutput   = mOutput->GetBestSize();
   desiredChannels = mInputChannels->GetBestSize();

   // wxGtk has larger comboboxes than the other platforms.  For DeviceToolBar this will cause
   // the height to be double because of the discrete grid layout.  So we shrink it to prevent this.
#ifdef __WXGTK__
   desiredHost.SetHeight(desiredHost.GetHeight() -4);
   desiredInput.SetHeight(desiredHost.GetHeight());
   desiredOutput.SetHeight(desiredHost.GetHeight());
   desiredChannels.SetHeight(desiredHost.GetHeight());
#endif

   ratioUnused = 0.995f - (kHostWidthRatio + kInputWidthRatio + kOutputWidthRatio + kChannelsWidthRatio);
   int i = 0;
   // limit the amount of times we solve contraints to 5
   while (constrained && ratioUnused > 0.01f && i < 5) {
      i++;
      constrained = false;

      constrained = RepositionCombo(mHost,   w,   desiredHost,   hostRatio, ratioUnused,
				    0, true) || constrained;
      constrained = RepositionCombo(mInput,  w,  desiredInput,  inputRatio, ratioUnused,
				    mRecordBitmap->GetWidth(), true) || constrained;
      constrained = RepositionCombo(mOutput, w, desiredOutput, outputRatio, ratioUnused,
				    mPlayBitmap->GetWidth(), true) || constrained;      
      constrained = RepositionCombo(mInputChannels, w, desiredChannels, channelsRatio, ratioUnused,
				    0, true) || constrained;
   }

   Update();
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
      if (foundHostIndex == mInputDeviceSourceMaps[i].hostIndex) {
         mInput->Append(MakeDeviceSourceString(&mInputDeviceSourceMaps[i]));
         if (host == wxT("")) {
            host = mInputDeviceSourceMaps[i].hostString;
            gPrefs->Write(wxT("/AudioIO/Host"), host);
         }
      }
   }
   mInput->Enable(mInput->GetCount() ? true : false);

   mOutput->Clear();
   for (i = 0; i < mOutputDeviceSourceMaps.size(); i++) {
      if (foundHostIndex == mOutputDeviceSourceMaps[i].hostIndex) {
         mOutput->Append(MakeDeviceSourceString(&mOutputDeviceSourceMaps[i]));
         if (host == wxT("")) {
            host = mOutputDeviceSourceMaps[i].hostString;
            gPrefs->Write(wxT("/AudioIO/Host"), host);
         }
      }
   }
   mOutput->Enable(mOutput->GetCount() ? true : false);

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
   // populate the devices
   FillHostDevices();
   // make the device display selection reflect the prefs if they exist
   UpdatePrefs();
   Refresh();
   Layout();
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
         for (j = 0; j < (unsigned int) mInputDeviceSourceMaps[i].numChannels; j++) {
            wxString name;

            if (j == 0) {
               name = _("1 (Mono) Input Channel");
            }
            else if (j == 1) {
               name = _("2 (Stereo) Input Channels");
            }
            else {
               name = wxString::Format(wxT("%d"), j + 1);
            }
            mInputChannels->Append(name);
         }
         newChannels = mInputDeviceSourceMaps[i].numChannels;
         if (oldChannels < newChannels && oldChannels >= 1)
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
}
void DeviceToolBar::SetDevices(DeviceSourceMap *in, DeviceSourceMap *out)
{
   if (in) {
      gPrefs->Write(wxT("/AudioIO/RecordingDevice"), in->deviceString);
      gPrefs->Write(wxT("/AudioIO/RecordingSourceIndex"), in->sourceIndex);
      if (in->totalSources >= 1) {
         gPrefs->Write(wxT("/AudioIO/RecordingSource"), in->sourceString);
      } else {
         gPrefs->Write(wxT("/AudioIO/RecordingSource"), wxT(""));
      }
      FillInputChannels();
   }

   if (out) {
      gPrefs->Write(wxT("/AudioIO/PlaybackDevice"), out->deviceString);
      if (out->totalSources >= 1) {
         gPrefs->Write(wxT("/AudioIO/PlaybackSource"), out->sourceString);
      } else {
         gPrefs->Write(wxT("/AudioIO/PlaybackSource"), wxT(""));
      }
   }
}

void DeviceToolBar::ChangeDevice(bool isInput)
{
   int newIndex = -1;
   wxChoice *combo = isInput ? mInput :mOutput;
   size_t i;

   int selectionIndex  = mInput->GetSelection();
   std::vector<DeviceSourceMap> &maps = isInput ? mInputDeviceSourceMaps : mOutputDeviceSourceMaps;

   wxString host     = gPrefs->Read(wxT("/AudioIO/Host"), wxT(""));
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
      // TO-DO: We *could* be smarter in this method and call HandleDeviceChange()  
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

   // Update the other project's DeviceToolBar.
   for (size_t i = 0; i < gAudacityProjects.GetCount(); i++) {
      gAudacityProjects[i]->GetDeviceToolBar()->UpdatePrefs();
   }
}

void DeviceToolBar::ShowInputDialog()
{
   ShowComboDialog(mInput, wxString(_("Select Input Device")));
}
void DeviceToolBar::ShowOutputDialog()
{
   ShowComboDialog(mOutput, wxString(_("Select Output Device")));
}
void DeviceToolBar::ShowHostDialog()
{
   ShowComboDialog(mHost, wxString(_("Select Audio Host")));
}
void DeviceToolBar::ShowChannelsDialog()
{
   ShowComboDialog(mInputChannels, wxString(_("Select Input Channels")));
}

void DeviceToolBar::ShowComboDialog(wxChoice *combo, const wxString &title)
{
   if (!combo || combo->GetCount() == 0) {
      wxMessageBox(_("Device information is not available."));
      return;
   }

#if USE_PORTMIXER
   wxArrayString inputSources = combo->GetStrings();

   wxDialog dlg(NULL, wxID_ANY, title);
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
      S.AddStandardButtons();
   }
   S.EndVerticalLay();

   dlg.SetSize(dlg.GetSizer()->GetMinSize());
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
