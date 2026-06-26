#ifndef PX_MIXER_H
#define PX_MIXER_H
/*
 * PortMixer
 * PortMixer core header
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

#define PX_MIXER_MAGIC 0x50544D52

#if !defined(NULL)
#define NULL 0
#endif

#if !defined(FALSE)
#define FALSE 0
#endif

#if !defined(TRUE)
#define TRUE 1
#endif

typedef struct px_mixer px_mixer;

struct px_mixer
{
   unsigned long magic;
   void *pa_stream;
   void *info;
   
   int input_device_index;
   int output_device_index;

   void (*CloseMixer) (px_mixer *Px);

   /*
   || Mixer info
   */
   int (*GetNumMixers) (px_mixer *Px);
   const char *(*GetMixerName) (px_mixer *Px, int i);

   /*
   || Master (output) volume
   */

   PxVolume (*GetMasterVolume) (px_mixer *Px);
   void (*SetMasterVolume) (px_mixer *Px, PxVolume volume);

   /*
   || Main output volume
   */

   int (*SupportsPCMOutputVolume) (px_mixer *Px);
   PxVolume (*GetPCMOutputVolume) (px_mixer *Px);
   void (*SetPCMOutputVolume) (px_mixer *Px, PxVolume volume);

   /*
   || Output info
   */

   int (*GetNumOutputVolumes) (px_mixer *Px);
   const char *(*GetOutputVolumeName) (px_mixer *Px, int i);

   /*
   || Output volume
   */

   PxVolume (*GetOutputVolume) (px_mixer *Px, int i);
   void (*SetOutputVolume) (px_mixer *Px, int i, PxVolume volume);

   /*
   || Input source
   */

   int (*GetNumInputSources) (px_mixer *Px);
   const char *(*GetInputSourceName) (px_mixer *Px, int i);
   int (*GetCurrentInputSource) (px_mixer *Px); /* may return -1 == none */
   void (*SetCurrentInputSource) (px_mixer *Px, int i);

   /*
   || Input volume
   */

   PxVolume (*GetInputVolume) (px_mixer *Px);
   void (*SetInputVolume) (px_mixer *Px, PxVolume volume);

   /*
   || Balance
   */

   int (*SupportsOutputBalance) (px_mixer *Px);
   PxBalance (*GetOutputBalance) (px_mixer *Px);
   void (*SetOutputBalance) (px_mixer *Px, PxBalance balance);

   /*
   || Playthrough
   */

   int (*SupportsPlaythrough) (px_mixer *Px);
   PxVolume (*GetPlaythrough) (px_mixer *Px);
   void (*SetPlaythrough) (px_mixer *Px, PxVolume volume);
};

static int initialize(px_mixer *Px);
static int cleanup(px_mixer *Px);

static void close_mixer(px_mixer *Px);
static int get_num_mixers(px_mixer *Px);
static const char *get_mixer_name(px_mixer *Px, int i);
static PxVolume get_master_volume(px_mixer *Px);
static void set_master_volume(px_mixer *Px, PxVolume volume);

/*
|| Main output volume
*/

static PxVolume get_pcm_output_volume(px_mixer *Px);
static void set_pcm_output_volume(px_mixer *Px, PxVolume volume);
static int supports_pcm_output_volume(px_mixer *Px);

/*
|| All output volumes
*/

static int get_num_output_volumes(px_mixer *Px);
static const char *get_output_volume_name(px_mixer *Px, int i);
static PxVolume get_output_volume(px_mixer *Px, int i);
static void set_output_volume(px_mixer *Px, int i, PxVolume volume);

/*
|| Input source
*/

static int get_num_input_sources(px_mixer *Px);
static const char *get_input_source_name(px_mixer *Px, int i);
static int get_current_input_source(px_mixer *Px);
static void set_current_input_source(px_mixer *Px, int i);

/*
|| Input volume
*/

static PxVolume get_input_volume(px_mixer *Px);
static void set_input_volume(px_mixer *Px, PxVolume volume);

/*
|| Balance
*/

static int supports_output_balance(px_mixer *Px);
static PxBalance get_output_balance(px_mixer *Px);
static void set_output_balance(px_mixer *Px, PxBalance balance);

/*
|| Playthrough
*/

static int supports_play_through(px_mixer *Px);
static PxVolume get_play_through(px_mixer *Px);
static void set_play_through(px_mixer *Px, PxVolume volume);

#ifdef __cplusplus
}
#endif
#endif
