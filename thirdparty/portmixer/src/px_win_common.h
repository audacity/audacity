#ifndef PX_WIN_COMMON_H
#define PX_WIN_COMMON_H
/*
 * PortMixer
 * Windows Common Header
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

#ifdef __cplusplus
extern "C"
{
#endif /* __cplusplus */

typedef struct PxCtrl
{
   char  *name;
   DWORD lineID;
   DWORD controlID;
} PxCtrl;

typedef struct PxInfo
{
   HMIXEROBJ   hInputMixer;
   char        *inName;
   int         numInputs;
   PxCtrl      *src;

   HMIXEROBJ   hOutputMixer;
   char        *outName;
   int         numOutputs;
   PxCtrl      *dst;

   DWORD       muxID;
   DWORD       speakerID;
   DWORD       waveID;
} PxInfo;

int open_mixers(px_mixer *Px, UINT deviceIn, UINT deviceOut);
int open_ep_mixers(px_mixer *Px, UINT deviceIn, UINT deviceOut);

#ifdef __cplusplus
}
#endif
#endif
