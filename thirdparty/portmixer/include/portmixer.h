#ifndef PORT_MIXER_H
#define PORT_MIXER_H

#ifdef __cplusplus
extern "C"
{
#endif /* __cplusplus */


/*
 * PortMixer
 * PortMixer API Header File
 *
 * Copyright (c) 2002, 2006
 *
 * Written by Dominic Mazzoni
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

#include "portaudio.h"

typedef void PxMixer;

typedef float PxVolume; /* 0.0 (min) --> 1.0 (max) */
typedef float PxBalance; /* -1.0 (left) --> 1.0 (right) */

/*
 Px_OpenMixer() returns a mixer which will work with the given PortAudio
 audio device.  Pass 0 as the index for the first (default) mixer.
*/

PxMixer* Px_OpenMixer(PaStream* pa_stream, PaDeviceIndex inputDeviceIndex, PaDeviceIndex outputDeviceIndex, int i);

/*
 Px_CloseMixer() closes a mixer opened using Px_OpenMixer and frees any
 memory associated with it. 
*/

void Px_CloseMixer( PxMixer *mixer );

/** @brief  Px_GetNumMixers returns the number of mixers which could be used
 * with the given PortAudio device.
 *
 * On most systems, there will be only one mixer for each device; however
 * there may be multiple mixers for each device, or possibly multiple mixers
 * which are independent of any particular PortAudio device. */

int Px_GetNumMixers( PxMixer *mixer );
const char *Px_GetMixerName( PxMixer *mixer, int i );

/*
 Master (output) volume
*/

PxVolume Px_GetMasterVolume( PxMixer *mixer );
void Px_SetMasterVolume( PxMixer *mixer, PxVolume volume );

/*
 Main output volume
*/

PxVolume Px_GetPCMOutputVolume( PxMixer *mixer );
void Px_SetPCMOutputVolume( PxMixer *mixer, PxVolume volume );
int Px_SupportsPCMOutputVolume( PxMixer* mixer ) ;

/*
 All output volumes
*/

int Px_GetNumOutputVolumes( PxMixer *mixer );
const char *Px_GetOutputVolumeName( PxMixer *mixer, int i );
PxVolume Px_GetOutputVolume( PxMixer *mixer, int i );
void Px_SetOutputVolume( PxMixer *mixer, int i, PxVolume volume );

/*
 Input source
*/

int Px_GetNumInputSources( PxMixer *mixer );
const char *Px_GetInputSourceName( PxMixer *mixer, int i);
int Px_GetCurrentInputSource( PxMixer *mixer ); /* may return -1 == none */
void Px_SetCurrentInputSource( PxMixer *mixer, int i );

/*
 Input volume
*/

PxVolume Px_GetInputVolume( PxMixer *mixer );
void Px_SetInputVolume( PxMixer *mixer, PxVolume volume );

/*
  Balance
*/

int Px_SupportsOutputBalance( PxMixer *mixer );
PxBalance Px_GetOutputBalance( PxMixer *mixer );
void Px_SetOutputBalance( PxMixer *mixer, PxBalance balance );

/*
  Playthrough
*/

int Px_SupportsPlaythrough( PxMixer *mixer );
PxVolume Px_GetPlaythrough( PxMixer *mixer );
void Px_SetPlaythrough( PxMixer *mixer, PxVolume volume );

#ifdef __cplusplus
}
#endif /* __cplusplus */
#endif /* PORT_AUDIO_H */
