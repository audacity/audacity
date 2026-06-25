/*
 * PortMixer
 * Common Windows routines
 *
 * Copyright (c) 2002, 2006
 *
 * Written by Dominic Mazzoni and Augustus Saunders
 *        and Leland Lucius
 *
 * PortMixer is intended to work side-by-side with PortAudio,
 * the Portable Real-Time Audio Library by Ross Bencina and
 * Phil Burk.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files
 * (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge,
 * publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * Any person wishing to distribute modifications to the Software is
 * requested to send the modifications to the original developer so that
 * they can be incorporated into the canonical version.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */

#include <windows.h>
#include <tchar.h>

#include "portaudio.h"

#include "portmixer.h"
#include "px_mixer.h"
#include "px_win_common.h"

static UINT get_ctrls(HMIXEROBJ mixer, DWORD lineID, PxCtrl **pctrls);
static DWORD find_ctrl(HMIXEROBJ mixer, DWORD lineID, DWORD ctrlID);

#if defined(_DEBUG)
#include <stdio.h>
#include <stdlib.h>
static void dprintf(const char *format, ...)
{
   char buf[4096];
   va_list args;
   int cnt;

   va_start(args, format);
   cnt = _vsnprintf(buf, sizeof(buf) - 1, format, args);
   va_end(args);

   if (cnt > 0) {
      buf[cnt] = '\0';
      OutputDebugString(buf);
   }
}
#endif

static BOOL is_vista_or_later() 
{
   OSVERSIONINFOEX osvi;
   DWORDLONG dwlConditionMask = 0;
   int op=VER_GREATER_EQUAL;

   // Initialize the OSVERSIONINFOEX structure.

   ZeroMemory(&osvi, sizeof(OSVERSIONINFOEX));
   osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);
   osvi.dwMajorVersion = 6;
   osvi.dwMinorVersion = 0;
   osvi.wServicePackMajor = 0;
   osvi.wServicePackMinor = 0;

   // Initialize the condition mask.

   VER_SET_CONDITION( dwlConditionMask, VER_MAJORVERSION, op );
   VER_SET_CONDITION( dwlConditionMask, VER_MINORVERSION, op );

   // Perform the test.

   return VerifyVersionInfo(
      &osvi, 
      VER_MAJORVERSION | VER_MINORVERSION,
      dwlConditionMask);
}

int open_mixers(px_mixer *Px, UINT deviceIn, UINT deviceOut)
{
   PxInfo*info;
   MMRESULT res;
  
   if (is_vista_or_later()) {
      return open_ep_mixers(Px, deviceIn, deviceOut);
   }

   res = mixerGetID((HMIXEROBJ) (UINT_PTR) (deviceIn == WAVE_MAPPER ? 0 : deviceIn),
                    &deviceIn,
                    MIXER_OBJECTF_WAVEIN);
   if (res != MMSYSERR_NOERROR) {
      return FALSE;
   }

   res = mixerGetID((HMIXEROBJ) (UINT_PTR) (deviceOut == WAVE_MAPPER ? 0 : deviceOut),
                    &deviceOut,
                    MIXER_OBJECTF_WAVEOUT);
   if (res != MMSYSERR_NOERROR) {
      return FALSE;
   }

   if (!initialize(Px)) {
      return FALSE;
   }

   info = (PxInfo *) Px->info;
   info->hInputMixer = NULL;
   info->hOutputMixer = NULL;
   info->numInputs = 0;
   info->muxID = 0;
   info->speakerID = 0;
   info->waveID = 0;

   if (deviceIn != UINT_MAX) {
      res = mixerOpen((LPHMIXER) &info->hInputMixer,
                      deviceIn,
                      0,
                      0,
                      MIXER_OBJECTF_MIXER);
      if (res != MMSYSERR_NOERROR) {
         return cleanup(Px);
      }

      info->muxID = find_ctrl(info->hInputMixer,
                              MIXERLINE_COMPONENTTYPE_DST_WAVEIN,
                              MIXERCONTROL_CONTROLTYPE_MUX);
      if (info->muxID == -1) {
         info->muxID = find_ctrl(info->hInputMixer,
                                 MIXERLINE_COMPONENTTYPE_DST_WAVEIN,
                                 MIXERCONTROL_CONTROLTYPE_MIXER);
      }

      info->numInputs = get_ctrls(info->hInputMixer,
                                  MIXERLINE_COMPONENTTYPE_DST_WAVEIN,
                                  &info->src);

      if (info->numInputs == 0) {
         return cleanup(Px);
      }
   }

   if (deviceOut != UINT_MAX) {
      res = mixerOpen((LPHMIXER) &info->hOutputMixer,
                      deviceOut,
                      0,
                      0,
                      MIXER_OBJECTF_MIXER);
      if (res != MMSYSERR_NOERROR) {
         return cleanup(Px);
      }

      info->speakerID = find_ctrl(info->hOutputMixer,
                                  MIXERLINE_COMPONENTTYPE_DST_SPEAKERS,
                                  MIXERCONTROL_CONTROLTYPE_VOLUME);

      info->waveID = find_ctrl(info->hOutputMixer,
                               MIXERLINE_COMPONENTTYPE_SRC_WAVEOUT,
                               MIXERCONTROL_CONTROLTYPE_VOLUME);

      info->numOutputs = get_ctrls(info->hOutputMixer,
                                   MIXERLINE_COMPONENTTYPE_DST_SPEAKERS,
                                   &info->dst);

      if (info->numOutputs == 0) {
         return cleanup(Px);
      }
   }

   return TRUE;
}

static UINT get_ctrls(HMIXEROBJ mixer, DWORD lineID, PxCtrl **pctrls)
{
   MMRESULT res;
   MIXERLINE line;
   MIXERLINECONTROLS controls;
   MIXERCONTROL control;
   PxCtrl *ctrls = NULL;
   UINT num;
   UINT s;

   do {
      line.cbStruct = sizeof(MIXERLINE);
      line.dwComponentType = lineID;

      res = mixerGetLineInfo(mixer,
                             &line,
                             MIXER_GETLINEINFOF_COMPONENTTYPE);

      if (res != MMSYSERR_NOERROR) {
         break;
      }

      num = (UINT) line.cConnections;

      ctrls = calloc(num, sizeof(PxInfo));
      if (ctrls == NULL) {
         break;
      }

      for (s = 0; s < num; s++)
      {
         line.dwSource      = s;

         res = mixerGetLineInfo(mixer,
                                &line,
                                MIXER_GETLINEINFOF_SOURCE);

         if (res != MMSYSERR_NOERROR) {
            break;
         }

         ctrls[s].lineID = line.dwLineID;
         ctrls[s].name = strdup(line.szName);
         if (ctrls[s].name == NULL) {
            break;
         }

         controls.cbStruct = sizeof(MIXERLINECONTROLS);
         controls.dwLineID = line.dwLineID;
         controls.dwControlType = MIXERCONTROL_CONTROLTYPE_VOLUME;
         controls.cbmxctrl = sizeof(MIXERCONTROL);
         controls.pamxctrl = &control;

         control.cbStruct = sizeof(MIXERCONTROL);

         res = mixerGetLineControls(mixer,
                                    &controls,
                                    MIXER_GETLINECONTROLSF_ONEBYTYPE);

         if (res == MMSYSERR_NOERROR)
            ctrls[s].controlID = control.dwControlID;
         else
            ctrls[s].controlID = 0;
      }

      if (s != num) {
         break;
      }

      *pctrls = ctrls;

      return num;

   } while (FALSE);

   if (ctrls) {
      for (s = 0; s < num; s++) {
         if (ctrls[s].name) {
            free(ctrls[s].name);
         }
      }

      free(ctrls);
   }

   return 0;
}

static DWORD find_ctrl(HMIXEROBJ mixer, DWORD lineID, DWORD ctrlID)
{
   MMRESULT res;
   MIXERLINE line;
   MIXERLINECONTROLS controls;
   MIXERCONTROL control;

   line.cbStruct = sizeof(MIXERLINE);
   line.dwComponentType = lineID;

   res = mixerGetLineInfo(mixer,
                          &line,
                          MIXER_GETLINEINFOF_COMPONENTTYPE);

   if (res != MMSYSERR_NOERROR) {
      return -1;
   }

   controls.cbStruct = sizeof(MIXERLINECONTROLS);
   controls.dwLineID = line.dwLineID;
   controls.dwControlType = ctrlID;
   controls.cbmxctrl = sizeof(MIXERCONTROL);
   controls.pamxctrl = &control;

   control.cbStruct = sizeof(MIXERCONTROL);

   res = mixerGetLineControls(mixer,
                              &controls,
                              MIXER_GETLINECONTROLSF_ONEBYTYPE);

   if (res != MMSYSERR_NOERROR) {
      return -1;
   }

   return control.dwControlID;
}

static int initialize(px_mixer *Px)
{
   Px->info = calloc(1, sizeof(PxInfo));
   if (Px->info == NULL) {
      return FALSE;
   }

   Px->CloseMixer = close_mixer;
   Px->GetNumMixers = get_num_mixers;
   Px->GetMixerName = get_mixer_name;
   Px->GetMasterVolume = get_master_volume;
   Px->SetMasterVolume = set_master_volume;
   Px->SupportsPCMOutputVolume = supports_pcm_output_volume;
   Px->GetPCMOutputVolume = get_pcm_output_volume;
   Px->SetPCMOutputVolume = set_pcm_output_volume;
   Px->GetNumOutputVolumes = get_num_output_volumes;
   Px->GetOutputVolumeName = get_output_volume_name;
   Px->GetOutputVolume = get_output_volume;
   Px->SetOutputVolume = set_output_volume;
   Px->GetNumInputSources = get_num_input_sources;
   Px->GetInputSourceName = get_input_source_name;
   Px->GetCurrentInputSource = get_current_input_source;
   Px->SetCurrentInputSource = set_current_input_source;
   Px->GetInputVolume = get_input_volume;
   Px->SetInputVolume = set_input_volume;
   
// Px->SupportsOutputBalance = supports_output_balance;
// Px->GetOutputBalance = get_output_balance;
// Px->SetOutputBalance = set_output_balance;
   
// Px->SupportsPlaythrough = supports_play_through;
// Px->GetPlaythrough = get_play_through;
// Px->SetPlaythrough = set_play_through;
   
   return TRUE;
}

static int cleanup(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;
   int i;

   if (info) {
      if (info->hInputMixer) {
         mixerClose((HMIXER) info->hInputMixer);
      }
   
      if (info->hOutputMixer) {
         mixerClose((HMIXER) info->hOutputMixer);
      }

      if (info->inName) {
         free(info->inName);
      }

      if (info->outName) {
         free(info->outName);
      }

      if (info->src) {
         for (i = 0; i < info->numInputs; i++) {
            if (info->src[i].name) {
               free(info->src[i].name);
            }
         }

         free(info->src);
      }

      if (info->dst) {
         for (i = 0; i < info->numOutputs; i++) {
            if (info->dst[i].name) {
               free(info->dst[i].name);
            }
         }

         free(info->dst);
      }

      free(info);
      Px->info = NULL;
   }

   return FALSE;
}

static void close_mixer(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;

   cleanup(Px);
}

static int get_num_mixers(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;

   return (info->hInputMixer != NULL) + (info->hOutputMixer != NULL);
}

static const char *get_mixer_name(px_mixer *Px, int i)
{
   PxInfo *info = (PxInfo *)Px->info;
   MIXERCAPS caps;
   MMRESULT res;

   if (i > 1) {
      return NULL;
   }

   if (i == 0) {
      if (!info->inName) {
         res = mixerGetDevCaps((UINT_PTR) info->hInputMixer, &caps, sizeof(caps));
         if (res == MMSYSERR_NOERROR) {
            info->inName = strdup(caps.szPname);
         }
      }
      return info->inName;
   }

   if (!info->outName) {
      res = mixerGetDevCaps((UINT_PTR) info->hOutputMixer, &caps, sizeof(caps));
      if (res == MMSYSERR_NOERROR) {
         info->outName = strdup(caps.szPname);
      }
   }

   return info->outName;
}

static PxVolume VolumeFunction(HMIXEROBJ hMixer, DWORD controlID, PxVolume volume)
{
   MIXERCONTROLDETAILS details;
   MMRESULT result;
   MIXERCONTROLDETAILS_UNSIGNED value;

   if (hMixer == NULL) {
      return -1.0;
   }
   
   memset(&value, 0, sizeof(MIXERCONTROLDETAILS_UNSIGNED));
   
   details.cbStruct = sizeof(MIXERCONTROLDETAILS);
   details.dwControlID = controlID;
   details.cChannels = 1; /* all channels */
   details.cMultipleItems = 0;
   details.cbDetails = sizeof(MIXERCONTROLDETAILS_UNSIGNED);
   details.paDetails = &value;

   if (volume < 0.0) {
      result = mixerGetControlDetails(hMixer,
                                      &details,
                                      MIXER_GETCONTROLDETAILSF_VALUE);
      if (result != MMSYSERR_NOERROR)
         return -1.0;
      
      return (PxVolume)(value.dwValue / 65535.0);
   }
   
   value.dwValue = (unsigned short)(volume * 65535.0);
   result = mixerSetControlDetails(hMixer,
                                   &details,
                                   MIXER_GETCONTROLDETAILSF_VALUE);
   
   if (result != MMSYSERR_NOERROR)
      return -1.0;
   
   return 0.0;
}

static PxVolume get_master_volume(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;
   
   return VolumeFunction(info->hOutputMixer, info->speakerID, -1.0);
}

static void set_master_volume(px_mixer *Px, PxVolume volume)
{
   PxInfo *info = (PxInfo *)Px->info;

   VolumeFunction(info->hOutputMixer, info->speakerID, volume);
   
   return;
}

/*
|| Main output volume
*/

static int supports_pcm_output_volume(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;
   
   return (info->waveID != -1);
}

static PxVolume get_pcm_output_volume(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;
   
   return VolumeFunction(info->hOutputMixer, info->waveID, -1.0);
}

static void set_pcm_output_volume(px_mixer *Px, PxVolume volume)
{
   PxInfo *info = (PxInfo *)Px->info;

   VolumeFunction(info->hOutputMixer, info->waveID, volume);

   return;
}

/*
|| Output info
*/

static int get_num_output_volumes(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;

   return info->numOutputs;
}

static const char *get_output_volume_name(px_mixer *Px, int i)
{
   PxInfo *info = (PxInfo *)Px->info;

   if (i < info->numOutputs) {
      return info->dst[i].name;
   }

   return NULL;
}

/*
|| Output volume
*/

static PxVolume get_output_volume(px_mixer *Px, int i)
{
   PxInfo *info = (PxInfo *)Px->info;

   if (i < info->numOutputs) {
      return VolumeFunction(info->hOutputMixer,
                            info->src[i].controlID,
                            -1.0);
   }

   return -1.0;
}

static void set_output_volume(px_mixer *Px, int i, PxVolume volume)
{
   PxInfo *info = (PxInfo *)Px->info;

   if (i < info->numOutputs) {
      VolumeFunction(info->hOutputMixer,
                     info->src[i].controlID,
                     volume);
   }

   return;
}

/*
|| Input source
*/

static int get_num_input_sources(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;

   return info->numInputs;
}

static const char *get_input_source_name(px_mixer *Px, int i)
{
   PxInfo *info = (PxInfo *)Px->info;

   if (i < info->numInputs) {
      return info->src[i].name;
   }

   return NULL;
}

static int get_current_input_source(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;
   MIXERCONTROLDETAILS details;
   MIXERCONTROLDETAILS_LISTTEXT *list;
   MIXERCONTROLDETAILS_BOOLEAN *flags;
   MMRESULT res;
   int i = 0;
   int j;

   if (!info->hInputMixer) {
      return -1;
   }

   do {
      list = calloc(info->numInputs, sizeof(MIXERCONTROLDETAILS_LISTTEXT));
      flags = calloc(info->numInputs, sizeof(MIXERCONTROLDETAILS_BOOLEAN));
      if (list == NULL || flags == NULL) {
         break;
      }

      details.cbStruct = sizeof(MIXERCONTROLDETAILS);
      details.dwControlID = info->muxID;
      details.cMultipleItems = info->numInputs;
      details.cChannels = 1;   
      details.cbDetails = sizeof(MIXERCONTROLDETAILS_LISTTEXT);
      details.paDetails = &list[0];

      res = mixerGetControlDetails(info->hInputMixer,
                                   &details,
                                   MIXER_GETCONTROLDETAILSF_LISTTEXT);

      if (res != MMSYSERR_NOERROR) {
         break;
      }

      details.cbStruct = sizeof(MIXERCONTROLDETAILS);
      details.dwControlID = info->muxID;
      details.cMultipleItems = info->numInputs;
      details.cChannels = 1;   
      details.cbDetails = sizeof(MIXERCONTROLDETAILS_BOOLEAN);
      details.paDetails = &flags[0];

      res = mixerGetControlDetails(info->hInputMixer,
                                   &details,
                                   MIXER_SETCONTROLDETAILSF_VALUE);

      if (res != MMSYSERR_NOERROR) {
         break;
      }

      for (j = 0; j < info->numInputs; j++) {
         if (flags[j].fValue) {
            i = j;
            break;
         }
      }

      for (j = 0; j < info->numInputs; j++) {
         if (info->src[j].lineID == list[i].dwParam1 ) {
            i = j;
            break;
         }
      }

   } while(FALSE);

   if (list) {
      free(list);
   }

   if (flags) {
      free(flags);
   }

   return i;
}

static void set_current_input_source(px_mixer *Px, int i)
{
   PxInfo *info = (PxInfo *)Px->info;
   MIXERCONTROLDETAILS details;
   MIXERCONTROLDETAILS_LISTTEXT *list;
   MIXERCONTROLDETAILS_BOOLEAN *flags;
   MMRESULT res;
   int j;

   if (!info->hInputMixer) {
      return;
   }

   do {
      list = calloc(info->numInputs, sizeof(MIXERCONTROLDETAILS_LISTTEXT));
      flags = calloc(info->numInputs, sizeof(MIXERCONTROLDETAILS_BOOLEAN));
      if (list == NULL || flags == NULL) {
         break;
      }

      details.cbStruct = sizeof(MIXERCONTROLDETAILS);
      details.dwControlID = info->muxID;
      details.cMultipleItems = info->numInputs;
      details.cChannels = 1;   
      details.cbDetails = sizeof(MIXERCONTROLDETAILS_LISTTEXT);
      details.paDetails = &list[0];

      res = mixerGetControlDetails(info->hInputMixer,
                                   &details,
                                   MIXER_GETCONTROLDETAILSF_LISTTEXT);

      if (res != MMSYSERR_NOERROR) {
         break;
      }

      for (j = 0; j < info->numInputs; j++) {
         if (list[j].dwParam1 == info->src[i].lineID) {
            flags[j].fValue = TRUE;
            break;
         }
      }

      details.cbStruct = sizeof(MIXERCONTROLDETAILS);
      details.dwControlID = info->muxID;
      details.cMultipleItems = info->numInputs;
      details.cChannels = 1;   
      details.cbDetails = sizeof(MIXERCONTROLDETAILS_BOOLEAN);
      details.paDetails = &flags[0];

      res = mixerSetControlDetails(info->hInputMixer,
                                   &details,
                                   MIXER_SETCONTROLDETAILSF_VALUE);

      if (res != MMSYSERR_NOERROR) {
         break;
      }
   } while(FALSE);

   if (list) {
      free(list);
   }

   if (flags) {
      free(flags);
   }

   return;
}

/*
|| Input volume
*/

static PxVolume get_input_volume(px_mixer *Px)
{
   PxInfo *info = (PxInfo *)Px->info;
   int src;

   if (info->hInputMixer) {
      src = get_current_input_source(Px);
      if (src < info->numInputs) {
         return VolumeFunction(info->hInputMixer,
                               info->src[src].controlID,
                               -1.0);
      }
   }

   return -1.0;
}

static void set_input_volume(px_mixer *Px, PxVolume volume)
{
   PxInfo *info = (PxInfo *)Px->info;
   int src;

   if (info->hInputMixer) {
      src = get_current_input_source(Px);
      if (src < info->numInputs) {
         VolumeFunction(info->hInputMixer,
                        info->src[src].controlID,
                        volume);
      }
   }

   return;
}
