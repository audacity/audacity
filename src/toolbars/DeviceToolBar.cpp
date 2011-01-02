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
   if (map->totalSources <= 1)
      ret = map->deviceString;
   else
      ret = map->deviceString + wxString(": ", wxConvLocal) + map->sourceString;
   
   return ret;
}

static void AddSourcesFromStream(int deviceIndex, wxString &devName, wxArrayString *descs, std::vector<DeviceSourceMap> *maps, PaStream *stream)
{
   int i;
   PxMixer *portMixer;
   DeviceSourceMap map;

   map.deviceIndex  = deviceIndex;
   map.sourceIndex  = -1;
   map.deviceString = devName;
   map.totalSources = 0;
   portMixer = Px_OpenMixer(stream, 0);
   if (!portMixer) {
      maps->push_back(map);
      descs->Add(MakeDeviceSourceString(&((*maps)[maps->size() - 1])));
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
      descs->Add(MakeDeviceSourceString(&((*maps)[maps->size() - 1])));
   } else {
      //open up a stream with the device so portmixer can get the info out of it.
      for (i = 0; i < map.totalSources; i++) {
         map.sourceIndex  = i;
         map.sourceString = wxString(Px_GetInputSourceName(portMixer, i), wxConvLocal);
         maps->push_back(map);
         descs->Add(MakeDeviceSourceString(&((*maps)[maps->size() - 1])));
      }
   }
   Px_CloseMixer(portMixer);
}

static void AddSources(int deviceIndex, int rate, wxArrayString *descs, std::vector<DeviceSourceMap> *maps, int isInput)
{
   int error;
   DeviceSourceMap map;
   wxString devName;

   devName = DeviceName(Pa_GetDeviceInfo(deviceIndex));
   // This tries to open the device with the samplerate worked out above, which
   // will be the highest available for play and record on the device, or
   // 44.1kHz if the info cannot be fetched.

   PaStream *stream = NULL;

   PaStreamParameters parameters;

   parameters.device = deviceIndex;
   parameters.sampleFormat = paFloat32;
   parameters.hostApiSpecificStreamInfo = NULL;
   parameters.channelCount = 1;
   if (Pa_GetDeviceInfo(deviceIndex))
      parameters.suggestedLatency = isInput ?
      Pa_GetDeviceInfo(deviceIndex)->defaultLowInputLatency:
         Pa_GetDeviceInfo(deviceIndex)->defaultLowOutputLatency;
   else
      parameters.suggestedLatency = DEFAULT_LATENCY_CORRECTION/1000.0;
      // try opening for record and playback
   error = Pa_OpenStream(&stream,
                         isInput ? &parameters : NULL,
                         isInput ? NULL : &parameters,
                         rate, paFramesPerBufferUnspecified,
                         paClipOff | paDitherOff,
                         NULL, NULL);
   if (stream) {
      AddSourcesFromStream(deviceIndex, devName, descs, maps, stream);
      Pa_CloseStream(stream);
   } else {
      map.deviceIndex  = deviceIndex;
      map.sourceIndex  = -1;
      map.deviceString = devName;
      map.totalSources = 0;
      maps->push_back(map);
      descs->Add(MakeDeviceSourceString(&((*maps)[maps->size() - 1])));
   }
}

void DeviceToolBar::Populate()
{
   int i;
   wxArrayString inputs;
   wxArrayString outputs;

   int nDevices = Pa_GetDeviceCount();

   //The heirarchy for devices is Host/device/source.
   //Some newer systems aggregate this.
   //So we need to call port mixer for every device to get the sources
   for (i = 0; i < nDevices; i++) {
      const PaDeviceInfo *info = Pa_GetDeviceInfo(i);
      if (info->maxOutputChannels > 0) {
         AddSources(i, info->defaultSampleRate, &outputs, &mOutputDeviceSourceMaps, 0);
      }

      if (info->maxInputChannels > 0) {
         AddSources(i, info->defaultSampleRate, &inputs, &mInputDeviceSourceMaps, 1);
      }
   }

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
   wxString devName;
   wxString sourceName;
   wxString desc;
   devName = gPrefs->Read(wxT("/AudioIO/RecordingDevice"), wxT(""));
   sourceName = gPrefs->Read(wxT("/AudioIO/RecordingSource"), wxT(""));
   if (sourceName == wxT(""))
      desc = devName;
   else
      desc = devName + wxString(": ", wxConvLocal) + sourceName; 
   mInput->SetStringSelection(desc);


   devName = gPrefs->Read(wxT("/AudioIO/PlaybackDevice"), wxT(""));
   sourceName = gPrefs->Read(wxT("/AudioIO/PlaybackSource"), wxT(""));
   if (sourceName == wxT(""))
      desc = devName;
   else
      desc = devName + wxString(": ", wxConvLocal) + sourceName; 
   mOutput->SetStringSelection(desc);

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
#endif
}

void DeviceToolBar::OnChoice(wxCommandEvent &event)
{
   int inputSelectionIndex;
   int outputSelectionIndex;

   inputSelectionIndex  = mInput->GetSelection();
   outputSelectionIndex = mOutput->GetSelection();

   wxString oldInput = gPrefs->Read(wxT("/AudioIO/RecordingDevice"), wxT(""));
   wxString newInput = inputSelectionIndex >= 0 ? 
                        mInputDeviceSourceMaps[inputSelectionIndex].deviceString:
                        oldInput;
   wxString oldOutput = gPrefs->Read(wxT("/AudioIO/PlaybackDevice"), wxT(""));
   wxString newOutput = outputSelectionIndex >= 0 ?
                        mOutputDeviceSourceMaps[outputSelectionIndex].deviceString:
                        oldOutput;
   int oldInIndex = -1, newInIndex = -1, oldOutIndex = -1, newOutIndex = -1;
   int nDevices = Pa_GetDeviceCount();
   int i;
   bool foundCompatibleDevice = false;

   // Find device indices for input and output
   for (i = 0; i < nDevices; ++i)
   {
      const PaDeviceInfo *info = Pa_GetDeviceInfo(i);
      wxString name = DeviceName(info);

      if (name == oldInput) oldInIndex = i;
      if (name == newInput) newInIndex = i;
      if (name == oldOutput) oldOutIndex = i;
      if (name == newOutput) newOutIndex = i;
   }

   // This shouldn't happen for new choices (it's OK for old ones)
   if (newInIndex < 0 || newOutIndex < 0)
   {
      wxLogDebug(wxT("DeviceToolBar::OnChoice(): couldn't find device indices"));
      return;
   }

   const PaDeviceInfo *inInfo = Pa_GetDeviceInfo(newInIndex);
   const PaDeviceInfo *outInfo = Pa_GetDeviceInfo(newOutIndex);
   if (oldInIndex != newInIndex)
   {
      // We changed input; be sure the output device has the same API
      if (inInfo->hostApi != outInfo->hostApi) {
         // First try setting the same device as the input
         // I think it is okay to set the string selection as devicename
         // because I *believe* that the input sources only refer to inputs
         // but if we end up with blanks in the combobox post selection due
         // to api mismatch, then this needs to change.
         if (!mOutput->SetStringSelection(DeviceName(inInfo)))
         {
            // Not found; set output device to default for the API
            const PaHostApiInfo *apiInfo = Pa_GetHostApiInfo(inInfo->hostApi);
            outInfo = Pa_GetDeviceInfo(apiInfo->defaultOutputDevice);
            mOutput->SetStringSelection(DeviceName(outInfo));
         }
      }
   }
   else if (oldOutIndex != newOutIndex)
   {
      // We changed output; be sure the input device has the same API
      if (outInfo->hostApi != inInfo->hostApi) {
         // First try setting the same device as the output
         for (i = 0; i < (int)mInputDeviceSourceMaps.size(); i++) {
            if (mInputDeviceSourceMaps[i].deviceString == DeviceName(outInfo)) {
               mInput->SetStringSelection(MakeDeviceSourceString(&mInputDeviceSourceMaps[i]));
               foundCompatibleDevice = true;
               break;
            }
         }
         if (!foundCompatibleDevice) {
            // Not found; set input device to default for the API
            const PaHostApiInfo *apiInfo = Pa_GetHostApiInfo(outInfo->hostApi);
            inInfo = Pa_GetDeviceInfo(apiInfo->defaultInputDevice);
            for (i = 0; i < (int)mInputDeviceSourceMaps.size(); i++) {
               if (mInputDeviceSourceMaps[i].deviceString == DeviceName(inInfo)) {
                  break;
               }
            }
            mInput->SetStringSelection(MakeDeviceSourceString(&mInputDeviceSourceMaps[i]));
         }
      }
   }

   inputSelectionIndex  = mInput->GetSelection();
   outputSelectionIndex = mOutput->GetSelection();

   gPrefs->Write(wxT("/AudioIO/Host"),
         wxString(Pa_GetHostApiInfo(inInfo->hostApi)->name, wxConvLocal));

   if (inputSelectionIndex >= 0) {
      gPrefs->Write(wxT("/AudioIO/RecordingDevice"),
         mInputDeviceSourceMaps[inputSelectionIndex].deviceString);
      gPrefs->Write(wxT("/AudioIO/RecordingSourceIndex"),
         mInputDeviceSourceMaps[inputSelectionIndex].sourceIndex);
      if (mInputDeviceSourceMaps[inputSelectionIndex].sourceIndex >= 0) {
         gPrefs->Write(wxT("/AudioIO/RecordingSource"),
            mInputDeviceSourceMaps[inputSelectionIndex].sourceString);
      } else
         gPrefs->Write(wxT("/AudioIO/RecordingSource"), wxT(""));
   }

   if (outputSelectionIndex >= 0) {
      gPrefs->Write(wxT("/AudioIO/PlaybackDevice"),
         mOutputDeviceSourceMaps[outputSelectionIndex].deviceString);
      if (mOutputDeviceSourceMaps[outputSelectionIndex].sourceIndex >= 0) {
         gPrefs->Write(wxT("/AudioIO/PlaybackSource"),
            mOutputDeviceSourceMaps[outputSelectionIndex].sourceString);
      } else
         gPrefs->Write(wxT("/AudioIO/RecordingSource"), wxT(""));
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

