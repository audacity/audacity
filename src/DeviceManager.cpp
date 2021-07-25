/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2010 Audacity Team
   Michael Chinen

******************************************************************/


#include "DeviceManager.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>



#include "portaudio.h"
#ifdef __WXMSW__
#include "pa_win_wasapi.h"
#endif

#ifdef USE_PORTMIXER
#include "portmixer.h"
#endif

#ifndef WX_PRECOMP
#include <wx/app.h>
#include <wx/choice.h>
#include <wx/event.h>
#include <wx/intl.h>
#include <wx/log.h>
#include <wx/settings.h>
#include <wx/sizer.h>
#include <wx/statbmp.h>
#include <wx/tooltip.h>
#endif

#include "Project.h"

#include "AudioIOBase.h"

#include "DeviceChange.h" // for HAVE_DEVICE_CHANGE

wxDEFINE_EVENT(EVT_RESCANNED_DEVICES, wxCommandEvent);

DeviceManager DeviceManager::dm;

/// Gets the singleton instance
DeviceManager* DeviceManager::Instance()
{
   return &dm;
}

const std::vector<DeviceSourceMap> &DeviceManager::GetInputDeviceMaps()
{
   if (!m_inited)
      Init();
   return mInputDeviceSourceMaps;
}
const std::vector<DeviceSourceMap> &DeviceManager::GetOutputDeviceMaps()
{
   if (!m_inited)
      Init();
   return mOutputDeviceSourceMaps;
}


wxString MakeDeviceSourceString(const DeviceSourceMap *map)
{
   wxString ret;
   ret = map->deviceString;
   if (map->totalSources > 1)
      ret += wxT(": ") + map->sourceString;

   return ret;
}

DeviceSourceMap* DeviceManager::GetDefaultDevice(int hostIndex, int isInput)
{
   if (hostIndex < 0 || hostIndex >= Pa_GetHostApiCount()) {
      return NULL;
   }

   const struct PaHostApiInfo *apiinfo = Pa_GetHostApiInfo(hostIndex);   // get info on API
   std::vector<DeviceSourceMap> & maps = isInput ? mInputDeviceSourceMaps : mOutputDeviceSourceMaps;
   size_t i;
   int targetDevice = isInput ? apiinfo->defaultInputDevice : apiinfo->defaultOutputDevice;

   for (i = 0; i < maps.size(); i++) {
      if (maps[i].deviceIndex == targetDevice)
         return &maps[i];
   }

   wxLogDebug(wxT("GetDefaultDevice() no default device"));
   return NULL;
}

DeviceSourceMap* DeviceManager::GetDefaultOutputDevice(int hostIndex)
{
   return GetDefaultDevice(hostIndex, 0);
}
DeviceSourceMap* DeviceManager::GetDefaultInputDevice(int hostIndex)
{
   return GetDefaultDevice(hostIndex, 1);
}

//--------------- Device Enumeration --------------------------

//Port Audio requires we open the stream with a callback or a lot of devices will fail
//as this means open in blocking mode, so we use a dummy one.
static int DummyPaStreamCallback(
    const void *WXUNUSED(input), void * WXUNUSED(output),
    unsigned long WXUNUSED(frameCount),
    const PaStreamCallbackTimeInfo* WXUNUSED(timeInfo),
    PaStreamCallbackFlags WXUNUSED(statusFlags),
    void *WXUNUSED(userData) )
{
   return 0;
}

static void FillHostDeviceInfo(DeviceSourceMap *map, const PaDeviceInfo *info, int deviceIndex, int isInput)
{
   wxString hostapiName = wxSafeConvertMB2WX(Pa_GetHostApiInfo(info->hostApi)->name);
   wxString infoName = wxSafeConvertMB2WX(info->name);

   map->deviceIndex  = deviceIndex;
   map->hostIndex    = info->hostApi;
   map->deviceString = infoName;
   map->hostString   = hostapiName;
   map->numChannels  = isInput ? info->maxInputChannels : info->maxOutputChannels;
}

static void AddSourcesFromStream(int deviceIndex, const PaDeviceInfo *info, std::vector<DeviceSourceMap> *maps, PaStream *stream)
{
#ifdef USE_PORTMIXER
   int i;
#endif
   DeviceSourceMap map;

   map.sourceIndex  = -1;
   map.totalSources = 0;
   // Only inputs have sources, so we call FillHostDeviceInfo with a 1 to indicate this
   FillHostDeviceInfo(&map, info, deviceIndex, 1);

#ifdef USE_PORTMIXER
   PxMixer *portMixer = Px_OpenMixer(stream, 0);
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
#endif

   if (map.totalSources <= 1) {
      map.sourceIndex = 0;
      maps->push_back(map);
   }
#ifdef USE_PORTMIXER
     else {
      //open up a stream with the device so portmixer can get the info out of it.
      for (i = 0; i < map.totalSources; i++) {
         map.sourceIndex  = i;
         map.sourceString = wxString(wxSafeConvertMB2WX(Px_GetInputSourceName(portMixer, i)));
         maps->push_back(map);
      }
   }
   Px_CloseMixer(portMixer);
#endif
}

static bool IsInputDeviceAMapperDevice(const PaDeviceInfo *info)
{
   // For Windows only, portaudio returns the default mapper object
   // as the first index after a NEW hostApi index is detected (true for MME and DS)
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

static void AddSources(int deviceIndex, int rate, std::vector<DeviceSourceMap> *maps, int isInput)
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
   // Vista/Win7 separate these into two devices with the same names (but different
   // portaudio indices)
   // Also, for mapper devices we don't want to keep any sources, so check for it here
   if (isInput && !IsInputDeviceAMapperDevice(info)) {
      if (info)
         parameters.suggestedLatency = info->defaultLowInputLatency;
      else
         parameters.suggestedLatency = 10.0;

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
                 wxString(wxSafeConvertMB2WX(Pa_GetErrorText((PaError)error))));
   }
}


/// Gets a NEW list of devices by terminating and restarting portaudio
/// Assumes that DeviceManager is only used on the main thread.
void DeviceManager::Rescan()
{
   // get rid of the previous scan info
   this->mInputDeviceSourceMaps.clear();
   this->mOutputDeviceSourceMaps.clear();

   // if we are doing a second scan then restart portaudio to get NEW devices
   if (m_inited) {
      // check to see if there is a stream open - can happen if monitoring,
      // but otherwise Rescan() should not be available to the user.
      auto gAudioIO = AudioIOBase::Get();
      if (gAudioIO) {
         if (gAudioIO->IsMonitoring())
         {
            gAudioIO->StopStream();
            while (gAudioIO->IsBusy())
               wxMilliSleep(100);
         }
      }

      // restart portaudio - this updates the device list
      // FIXME: TRAP_ERR restarting PortAudio
      Pa_Terminate();
      Pa_Initialize();
   }

   // FIXME: TRAP_ERR PaErrorCode not handled in ReScan()
   int nDevices = Pa_GetDeviceCount();

   //The hierarchy for devices is Host/device/source.
   //Some newer systems aggregate this.
   //So we need to call port mixer for every device to get the sources
   for (int i = 0; i < nDevices; i++) {
      const PaDeviceInfo *info = Pa_GetDeviceInfo(i);
      if (info->maxOutputChannels > 0) {
         AddSources(i, info->defaultSampleRate, &mOutputDeviceSourceMaps, 0);
      }

      if (info->maxInputChannels > 0) {
#ifdef __WXMSW__
#if !defined(EXPERIMENTAL_FULL_WASAPI)
         if (Pa_GetHostApiInfo(info->hostApi)->type != paWASAPI ||
             PaWasapi_IsLoopback(i) > 0)
#endif
#endif
         AddSources(i, info->defaultSampleRate, &mInputDeviceSourceMaps, 1);
      }
   }

   // If this was not an initial scan update each device toolbar.
   if ( m_inited ) {
      wxCommandEvent e{ EVT_RESCANNED_DEVICES };
      wxTheApp->ProcessEvent( e );
   }

   m_inited = true;
   mRescanTime = std::chrono::steady_clock::now();
}


float DeviceManager::GetTimeSinceRescan() {
   auto now = std::chrono::steady_clock::now();
   auto dur = std::chrono::duration_cast<std::chrono::duration<float>>(now - mRescanTime);
   return dur.count();
}


//private constructor - Singleton.
DeviceManager::DeviceManager()
#if defined(EXPERIMENTAL_DEVICE_CHANGE_HANDLER)
#if defined(HAVE_DEVICE_CHANGE)
:  DeviceChangeHandler()
#endif
#endif
{
   m_inited = false;
   mRescanTime = std::chrono::steady_clock::now();
}

DeviceManager::~DeviceManager()
{

}

void DeviceManager::Init()
{
    Rescan();

#if defined(EXPERIMENTAL_DEVICE_CHANGE_HANDLER)
#if defined(HAVE_DEVICE_CHANGE)
   DeviceChangeHandler::Enable(true);
#endif
#endif
}

#if defined(EXPERIMENTAL_DEVICE_CHANGE_HANDLER)
#if defined(HAVE_DEVICE_CHANGE)
void DeviceManager::DeviceChangeNotification()
{
   Rescan();
   return;
}
#endif
#endif
