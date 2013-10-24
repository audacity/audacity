/*
 *	TwoLAME: an optimized MPEG Audio Layer Two encoder
 *
 *	Copyright (C) 2001-2004 Michael Cheng
 *	Copyright (C) 2004-2006 The TwoLAME Project
 *
 *	This library is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU Lesser General Public
 *	License as published by the Free Software Foundation; either
 *	version 2.1 of the License, or (at your option) any later version.
 *
 *	This library is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *	Lesser General Public License for more details.
 *
 *	You should have received a copy of the GNU Lesser General Public
 *	License along with this library; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  $Id$
 *
 */

#ifndef __TWOLAME_H__
#define __TWOLAME_H__

#ifdef __cplusplus
extern "C" {
#endif

/** \file twolame.h */

/*
 * ATTENTION WIN32 USERS!
 * 
 * By default, when you use this header file, it is configured to use
 * symbols from the "twolame.dll" file. If you use the static version of
 * the library, define LIBTWOLAME_STATIC prior to including this header.
 */

#ifdef _WIN32
#ifdef LIBTWOLAME_STATIC
#define DLL_EXPORT
#else
#ifdef LIBTWOLAME_DLL_EXPORTS
#define DLL_EXPORT __declspec(dllexport)
#else
#define DLL_EXPORT __declspec(dllimport)
#endif
#endif
#else
#define DLL_EXPORT
#endif


#ifndef TRUE
#define TRUE	(1)
#endif

#ifndef FALSE
#define FALSE	(0)
#endif


/** MPEG modes */
    typedef enum {
        TWOLAME_AUTO_MODE = -1,
                            /**< Choose Mode Automatically */
        TWOLAME_STEREO = 0, /**< Stereo */
        TWOLAME_JOINT_STEREO,
                            /**< Joint Stereo */
        TWOLAME_DUAL_CHANNEL,
                            /**< Dual Channel */
        TWOLAME_MONO,       /**< Mono */
        TWOLAME_NOT_SET
    } TWOLAME_MPEG_mode;


/** MPEG Version.
 *
 *	MPEG2 is for Lower Sampling Frequencies - LSF < 32000.
 */
    typedef enum {
        TWOLAME_MPEG2 = 0,
                        /**< MPEG-2	 - for sample rates less than 32k */
        TWOLAME_MPEG1   /**< MPEG-1 */
    } TWOLAME_MPEG_version;


/** Padding types. */
    typedef enum {
        TWOLAME_PAD_NO = 0, /**< No Padding */
        TWOLAME_PAD_ALL     /**< Pad all frames */
//  TWOLAME_PAD_ADJUST      // unsupported by twolame
    } TWOLAME_Padding;

/** Emphasis types. */
    typedef enum {
        TWOLAME_EMPHASIS_N = 0,
                            /**< No Emphasis */
        TWOLAME_EMPHASIS_5 = 1,
                            /**< 50/15 ms */
        // reserved
        TWOLAME_EMPHASIS_C = 3
                            /**< CCIT J.17 */
    } TWOLAME_Emphasis;


/** Number of samples per frame of Layer 2 MPEG Audio */
#define TWOLAME_SAMPLES_PER_FRAME		(1152)


/** Opaque structure for the twolame encoder options. */
    struct twolame_options_struct;

/** Opaque data type for the twolame encoder options. */
    typedef struct twolame_options_struct twolame_options;





/** Get the version number of the TwoLAME library. 
 *	eg "0.3.1".
 *
 *	\return The version number as a C string
 */
    DLL_EXPORT const char *get_twolame_version(void);


/** Get the URL of the TwoLAME homepage. 
 *	eg "http://www.twolame.org/".
 *
 *	\return The url as a C string
 */
    DLL_EXPORT const char *get_twolame_url(void);


/** Print the library version and 
 *	encoder parameter settings to STDERR.
 *
 *	Will display differnent ammounts of information
 *	depending on the verbosity setting.
 *	If verbosity is set to 0 then no message will be displayed.
 *
 *	\param glopts		Options pointer created by twolame_init()
 *
 */
    DLL_EXPORT void twolame_print_config(twolame_options * glopts);


/** Initialise the twolame encoder.
 *
 *	Sets defaults for all parameters.
 *	Will return NULL if malloc() failed, otherwise 
 *	returns a pointer which you then need to pass to 
 *	all future API calls.
 *	
 *	\return a pointer to your new options data structure
 */
    DLL_EXPORT twolame_options *twolame_init(void);


/** Prepare to start encoding.
 *
 *	You must call twolame_init_params() before you start encoding.
 *	It will check call your parameters to make sure they are valid,
 *	as well as allocating buffers and initising internally used
 *	variables.
 *	
 *	\param glopts		Options pointer created by twolame_init()
 *	\return				0 if all patameters are valid, 
 *						non-zero if something is invalid
 */
    DLL_EXPORT int twolame_init_params(twolame_options * glopts);


/** Encode some 16-bit PCM audio to MP2.
 *
 *	Takes 16-bit PCM audio samples from seperate left and right
 *	buffers and places encoded audio into mp2buffer.
 *	
 *	\param glopts			twolame options pointer
 *	\param leftpcm			Left channel audio samples
 *	\param rightpcm			Right channel audio samples
 *	\param num_samples		Number of samples per channel
 *	\param mp2buffer		Buffer to place encoded audio into
 *	\param mp2buffer_size	Size of the output buffer
 *	\return					The number of bytes put in output buffer
 *							or a negative value on error
 */
    DLL_EXPORT int twolame_encode_buffer(twolame_options * glopts,
                                         const short int leftpcm[],
                                         const short int rightpcm[],
                                         int num_samples,
                                         unsigned char *mp2buffer, int mp2buffer_size);


/** Encode some 16-bit PCM audio to MP2.
 *
 *	Takes interleaved 16-bit PCM audio samples from a single 
 *	buffer and places encoded audio into mp2buffer.
 *	
 *	\param glopts			twolame options pointer
 *	\param pcm				Audio samples for left AND right channels
 *	\param num_samples		Number of samples per channel
 *	\param mp2buffer		Buffer to place encoded audio into
 *	\param mp2buffer_size	Size of the output buffer
 *	\return					The number of bytes put in output buffer
 *							or a negative value on error
 */
    DLL_EXPORT int twolame_encode_buffer_interleaved(twolame_options * glopts,
                                                     const short int pcm[],
                                                     int num_samples,
                                                     unsigned char *mp2buffer, int mp2buffer_size);


/** Encode some 32-bit PCM audio to MP2.
 *
 *	Takes 32-bit floating point PCM audio samples from seperate 
 *	left and right buffers and places encoded audio into mp2buffer.
 *
 *	Note: the 32-bit samples are currently scaled down to 
 *	16-bit samples internally.
 *	
 *	\param glopts			twolame options pointer
 *	\param leftpcm			Left channel audio samples
 *	\param rightpcm			Right channel audio samples
 *	\param num_samples		Number of samples per channel
 *	\param mp2buffer		Buffer to place encoded audio into
 *	\param mp2buffer_size	Size of the output buffer
 *	\return					The number of bytes put in output buffer
 *							or a negative value on error
 */
    DLL_EXPORT int twolame_encode_buffer_float32(twolame_options * glopts,
                                                 const float leftpcm[],
                                                 const float rightpcm[],
                                                 int num_samples,
                                                 unsigned char *mp2buffer, int mp2buffer_size);


/** Encode some 32-bit PCM audio to MP2.
 *
 *	Takes 32-bit floating point PCM audio samples from a single 
 *	buffer and places encoded audio into mp2buffer.
 *	
 *	\param glopts			twolame options pointer
 *	\param pcm				Audio samples for left AND right channels
 *	\param num_samples		Number of samples per channel
 *	\param mp2buffer		Buffer to place encoded audio into
 *	\param mp2buffer_size	Size of the output buffer
 *	\return					The number of bytes put in output buffer
 *							or a negative value on error
 */
    int twolame_encode_buffer_float32_interleaved(twolame_options * glopts,
                                                  const float pcm[],
                                                  int num_samples,
                                                  unsigned char *mp2buffer, int mp2buffer_size);


/** Encode any remains buffered PCM audio to MP2.
 *
 *	Encodes any remaining audio samples in the libtwolame
 *	internal sample buffer. This function will return at
 *	most a single frame of MPEG Audio, and at least 0 frames.
 *	
 *	\param glopts			twolame options pointer
 *	\param mp2buffer		Buffer to place encoded audio into
 *	\param mp2buffer_size	Size of the output buffer
 *	\return					The number of bytes put in output buffer
 *							or a negative value on error
 */
    DLL_EXPORT int twolame_encode_flush(twolame_options * glopts,
                                        unsigned char *mp2buffer, int mp2buffer_size);


/** Shut down the twolame encoder.
 *
 *	Shuts down the twolame encoder and frees all memory
 *	that it previously allocated. You should call this
 *	once you have finished all your encoding. This function
 *	will set your glopts pointer to NULL for you.
 *
 *	\param glopts			pointer to twolame options pointer
 */
    DLL_EXPORT void twolame_close(twolame_options ** glopts);



/** Set the verbosity of the encoder.
 *
 *	Sets how verbose the encoder is with the debug and
 *	informational messages it displays. The higher the
 *	number, the more messages it will display.
 *	Set to 0 for no status messages to STDERR 
 *	( error messages will still be displayed ).
 *
 *	Default: 1
 *
 *	\param glopts			pointer to twolame options pointer
 *	\param verbosity		integer between 0 and 10
 *	\return					0 if successful, 
 *							non-zero on failure
 */
    DLL_EXPORT int twolame_set_verbosity(twolame_options * glopts, int verbosity);


/** Get the verbosity of the encoder.
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			integer indicating the verbosity of the encoder (0-10)
 */
    DLL_EXPORT int twolame_get_verbosity(twolame_options * glopts);


/** Set the MPEG Audio Mode (Mono, Stereo, etc) for 
 *	the output stream.
 *
 *	Default: TWOLAME_AUTO_MODE
 *
 *	\param glopts			pointer to twolame options pointer
 *	\param mode				the mode to set to
 *	\return					0 if successful, 
 *							non-zero on failure
 */
    DLL_EXPORT int twolame_set_mode(twolame_options * glopts, TWOLAME_MPEG_mode mode);


/** Get the MPEG Audio mode of the output stream.
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			the MPEG audio mode
 */
    DLL_EXPORT TWOLAME_MPEG_mode twolame_get_mode(twolame_options * glopts);


/** Get a string name for the current MPEG Audio mode.
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			the name of the MPEG audio mode as a string
 */
    DLL_EXPORT const char *twolame_get_mode_name(twolame_options * glopts);


/** Set the MPEG version of the MPEG audio stream. 
 *
 *	Default: TWOLAME_MPEG1
 *
 *	\param glopts			pointer to twolame options pointer
 *	\param version			the version to set to
 *	\return					0 if successful, 
 *							non-zero on failure
 */
    DLL_EXPORT int twolame_set_version(twolame_options * glopts, TWOLAME_MPEG_version version);


/** Get the MPEG version of the output stream.
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			the MPEG version
 */
    DLL_EXPORT TWOLAME_MPEG_version twolame_get_version(twolame_options * glopts);


/** Get a string name for the current MPEG version.
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			the name of the MPEG version as a string
 */
    DLL_EXPORT const char *twolame_get_version_name(twolame_options * glopts);


/** Get the number of bytes per MPEG audio frame, for current settings.
 *
 *	\param glopts			pointer to twolame options pointer
 *	\return					the number of bytes per frame
 *
 */
    DLL_EXPORT int twolame_get_framelength(twolame_options * glopts);


/** Set the Psychoacoustic Model used to encode the audio.
 *
 *	Default: 3
 *
 *	\param glopts			pointer to twolame options pointer
 *	\param psymodel			the psychoacoustic model number
 *	\return					0 if successful, 
 *							non-zero on failure
 */
    DLL_EXPORT int twolame_set_psymodel(twolame_options * glopts, int psymodel);


/** Get the Psychoacoustic Model used to encode the audio.
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			the psychoacoustic model number
 */
    DLL_EXPORT int twolame_get_psymodel(twolame_options * glopts);


/** Set the number of channels in the input stream.
 *
 *	If this is different the number of channels in
 *	the output stream (set by mode) then the encoder
 *	will automatically downmix/upmix the audio.
 *
 *	Default: 2
 *
 *	\param glopts			pointer to twolame options pointer
 *	\param num_channels		the number of input channels
 *	\return					0 if successful, 
 *							non-zero on failure
 */
    DLL_EXPORT int twolame_set_num_channels(twolame_options * glopts, int num_channels);


/** Get the number of channels in the input stream.
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			the number of channels
 */
    DLL_EXPORT int twolame_get_num_channels(twolame_options * glopts);


/** Set the scaling level for audio before encoding.
 *
 *	Set to 0 to disable.
 *
 *	Default: 0
 *
 *	\param glopts			pointer to twolame options pointer
 *	\param scale			the amount to scale by
 *	\return					0 if successful, 
 *							non-zero on failure
 */
    DLL_EXPORT int twolame_set_scale(twolame_options * glopts, float scale);


/** Get the scaling level for audio before encoding.
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			the amount to scale audio sample by
 */
    DLL_EXPORT float twolame_get_scale(twolame_options * glopts);

/** Set the scaling level for left channel audio before encoding.
 *
 *	Set to 0 to disable.
 *
 *	Default: 0
 *
 *	\param glopts			pointer to twolame options pointer
 *	\param scale			the amount to scale by
 *	\return					0 if successful, 
 *							non-zero on failure
 */
    DLL_EXPORT int twolame_set_scale_left(twolame_options * glopts, float scale);


/** Get the scaling level for audio left channel before encoding.
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			the amount to scale left channel audio samples by
 */
    DLL_EXPORT float twolame_get_scale_left(twolame_options * glopts);


/** Set the scaling level for right channel audio before encoding.
 *
 *	Set to 0 to disable.
 *
 *	Default: 0
 *
 *	\param glopts			pointer to twolame options pointer
 *	\param scale			the amount to scale by
 *	\return					0 if successful, 
 *							non-zero on failure
 */
    DLL_EXPORT int twolame_set_scale_right(twolame_options * glopts, float scale);


/** Get the scaling level for audio right channel before encoding.
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			the amount to scale right channel audio samples by
 */
    DLL_EXPORT float twolame_get_scale_right(twolame_options * glopts);


/** Set the samplerate of the PCM audio input.
 *
 *	Default: 44100
 *
 *	\param glopts			pointer to twolame options pointer
 *	\param samplerate		the samplerate in Hz
 *	\return					0 if successful, 
 *							non-zero on failure
 */
    DLL_EXPORT int twolame_set_in_samplerate(twolame_options * glopts, int samplerate);


/** Get the samplerate of the PCM audio input.
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			the input samplerate
 */
    DLL_EXPORT int twolame_get_in_samplerate(twolame_options * glopts);


/** Set the samplerate of the MPEG audio output.
 *
 *	Default: 44100
 *
 *	\param glopts			pointer to twolame options pointer
 *	\param samplerate		the samplerate in Hz
 *	\return					0 if successful, 
 *							non-zero on failure
 */
    DLL_EXPORT int twolame_set_out_samplerate(twolame_options * glopts, int samplerate);


/** Get the samplerate of the MPEG audio output.
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			the output samplerate
 */
    DLL_EXPORT int twolame_get_out_samplerate(twolame_options * glopts);


/** Set the bitrate of the MPEG audio output stream.
 *
 *	Default: 192
 *
 *	\param glopts			pointer to twolame options pointer
 *	\param bitrate			the bitrate in kbps
 *	\return					0 if successful, 
 *							non-zero on failure
 */
    DLL_EXPORT int twolame_set_bitrate(twolame_options * glopts, int bitrate);


/** Get the bitrate of the MPEG audio output.
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			the output bitrate in kbps
 */
    DLL_EXPORT int twolame_get_bitrate(twolame_options * glopts);


/** Set the bitrate of the MPEG audio output stream (LAME style).
 *
 *	same as twolame_set_bitrate()
 */
    DLL_EXPORT int twolame_set_brate(twolame_options * glopts, int bitrate);


/** Get the bitrate of the MPEG audio output stream (LAME style).
 *
 *	same as twolame_get_bitrate()
 */
    DLL_EXPORT int twolame_get_brate(twolame_options * glopts);


/** Set frame padding for the MPEG audio output stream.
 *
 *	i.e. adjust frame sizes to achieve overall target bitrate
 *
 *	Default: TWOLAME_PAD_NO
 *
 *	\param glopts			pointer to twolame options pointer
 *	\param padding			the padding type
 *	\return					0 if successful, 
 *							non-zero on failure
 */
    DLL_EXPORT int twolame_set_padding(twolame_options * glopts, TWOLAME_Padding padding);

/** Get the padding type of the MPEG audio output.
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			the output bitrate in kbps
 */
    DLL_EXPORT TWOLAME_Padding twolame_get_padding(twolame_options * glopts);


/** Enable/Disable Energy Level Extension.
 *
 *	Enable writing the peak PCM level (energy level) at the end of each
 *	MPEG audio frame (in the ancillary bits). This function will
 *	automatically call twolame_set_num_ancillary_bits() to set the required
 *	number of ancillary bits for this feature.
 *
 *	The energy level extension is commonly used in the broadcast industry 
 *	for visualising the audio in editing applications without decoding.
 *
 *	Default: FALSE
 *
 *	\param glopts			pointer to twolame options pointer
 *	\param energylevels		energy level extension state (TRUE/FALSE)
 *	\return					0 if successful, 
 *							non-zero on failure
 */
    DLL_EXPORT int twolame_set_energy_levels(twolame_options * glopts, int energylevels);


/** Get the Energy Level Extension state.
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			state of the Energy Level Extension (TRUE/FALSE)
 */
    DLL_EXPORT int twolame_get_energy_levels(twolame_options * glopts);


/** Set number of Ancillary Bits at end of frame.
 *
 *	Default: 0
 *
 *	\param glopts			pointer to twolame options pointer
 *	\param num				number of bits to reserve
 *	\return					0 if successful, 
 *							non-zero on failure
 */
    DLL_EXPORT int twolame_set_num_ancillary_bits(twolame_options * glopts, int num);


/** Get the number of Ancillary Bits at end of frame.
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			number of Ancillary Bits at end of frame
 */
    DLL_EXPORT int twolame_get_num_ancillary_bits(twolame_options * glopts);



/** Set the type of pre-emphasis to be applied to the decoded audio.
 *
 *	Default: TWOLAME_EMPHASIS_N
 *
 *	\param glopts			pointer to twolame options pointer
 *	\param emphasis			the type of pre-emphasis
 *	\return					0 if successful, 
 *							non-zero on failure
 */
    DLL_EXPORT int twolame_set_emphasis(twolame_options * glopts, TWOLAME_Emphasis emphasis);


/** Get the type of pre-emphasis to be applied to the decoded audio.
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			the type of pre-emphasis
 */
    DLL_EXPORT TWOLAME_Emphasis twolame_get_emphasis(twolame_options * glopts);


/** Enable/Disable CRC Error Protection.
 *
 *	Default: FALSE
 *
 *	\param glopts			pointer to twolame options pointer
 *	\param err_protection	error protection state (TRUE/FALSE)
 *	\return					0 if successful, 
 *							non-zero on failure
 */
    DLL_EXPORT int twolame_set_error_protection(twolame_options * glopts, int err_protection);


/** Get the CRC Error Protection state.
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			state of Error Protection (TRUE/FALSE)
 */
    DLL_EXPORT int twolame_get_error_protection(twolame_options * glopts);


/** Set the MPEG Audio Copyright flag.
 *
 *	Indicates that MPEG stream is copyrighted.
 *
 *	Default: FALSE
 *
 *	\param glopts			pointer to twolame options pointer
 *	\param copyright		copyright flag state (TRUE/FALSE)
 *	\return					0 if successful, 
 *							non-zero on failure
 */
    DLL_EXPORT int twolame_set_copyright(twolame_options * glopts, int copyright);


/** Get the copright flag state
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			state of the copyright flag (TRUE/FALSE)
 */
    DLL_EXPORT int twolame_get_copyright(twolame_options * glopts);


/** Set the MPEG Audio Original flag.
 *
 *	Default: FALSE
 *
 *	\param glopts			pointer to twolame options pointer
 *	\param original			original flag state (TRUE/FALSE)
 *	\return					0 if successful, 
 *							non-zero on failure
 */
    DLL_EXPORT int twolame_set_original(twolame_options * glopts, int original);


/** Get the origianl flag state.
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			state of the original flag (TRUE/FALSE)
 */
    DLL_EXPORT int twolame_get_original(twolame_options * glopts);


/** Enable/Disable VBR (Variable Bit Rate) mode.
 *
 *	Default: FALSE
 *
 *	\param glopts			pointer to twolame options pointer
 *	\param vbr				VBR state (TRUE/FALSE)
 *	\return					0 if successful, 
 *							non-zero on failure
 */
    DLL_EXPORT int twolame_set_VBR(twolame_options * glopts, int vbr);


/** Get the VBR state.
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			state of VBR (TRUE/FALSE)
 */
    DLL_EXPORT int twolame_get_VBR(twolame_options * glopts);


/** Set the level/quality of the VBR audio.
 *
 *	The level value can is a measurement of quality - the higher 
 *	the level the higher the average bitrate of the resultant file.
 *
 *	Default: 5.0
 *
 *	\param glopts			pointer to twolame options pointer
 *	\param level			quality level (-10 to 10)
 *	\return					0 if successful, 
 *							non-zero on failure
 */
    DLL_EXPORT int twolame_set_VBR_level(twolame_options * glopts, float level);


/** Get the level/quality of the VBR audio.
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			quality value for VBR 
 */
    DLL_EXPORT float twolame_get_VBR_level(twolame_options * glopts);



/* 
   Using the twolame_set_VBR_q()/twolame_get_VBR_q functions 
   are deprecated, please use twolame_set_VBR_level() instead.
*/
    DLL_EXPORT int twolame_set_VBR_q(twolame_options * glopts, float level);
    DLL_EXPORT float twolame_get_VBR_q(twolame_options * glopts);



/** Set the adjustment (in dB) applied to the ATH for Psycho models 3 and 4.
 *
 *	Default: 0.0
 *
 *	\param glopts			pointer to twolame options pointer
 *	\param level			adjustment level in db
 *	\return					0 if successful, 
 *							non-zero on failure
 */
    DLL_EXPORT int twolame_set_ATH_level(twolame_options * glopts, float level);


/** Get the adjustment (in dB) applied to the ATH for Psycho models 3 and 4.
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			adjustment level in db
 */
    DLL_EXPORT float twolame_get_ATH_level(twolame_options * glopts);


/** Set the upper bitrate for VBR
 *
 *	Default: 0 (off)
 *
 *	\param glopts			pointer to twolame options pointer
 *	\param bitrate			upper bitrate for VBR
 *	\return					0 if successful, 
 *							non-zero on failure
 */
    DLL_EXPORT int twolame_set_VBR_max_bitrate_kbps(twolame_options * glopts, int bitrate);

/** Get the upper bitrate for VBR.
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			the upper bitrate for VBR
 */
    DLL_EXPORT int twolame_get_VBR_max_bitrate_kbps(twolame_options * glopts);


/** Enable/Disable the quick mode for psycho model calculation.
 *
 *	Default: FALSE
 *
 *	\param glopts			pointer to twolame options pointer
 *	\param quickmode		the state of quick mode (TRUE/FALSE)
 *	\return					0 if successful, 
 *							non-zero on failure
 */
    DLL_EXPORT int twolame_set_quick_mode(twolame_options * glopts, int quickmode);

/** Get the state of quick mode.
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			the state of quick mode (TRUE/FALSE)
 */
    DLL_EXPORT int twolame_get_quick_mode(twolame_options * glopts);


/** Set how often the psy model is calculated.
 *
 *	Default: 10
 *
 *	\param glopts			pointer to twolame options pointer
 *	\param quickcount		number of frames between calculations
 *	\return					0 if successful, 
 *							non-zero on failure
 */
    DLL_EXPORT int twolame_set_quick_count(twolame_options * glopts, int quickcount);

/** Get the how often the psy model is calculated.
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			number of frames between calculations
 */
    DLL_EXPORT int twolame_get_quick_count(twolame_options * glopts);







/* WARNING: DAB support is currently broken */





/** Enable/Disable the Eureka 147 DAB extensions for MP2.
 *
 *	Default: FALSE
 *
 *	\param glopts			pointer to twolame options pointer
 *	\param dab				state of DAB extensions (TRUE/FALSE)
 *	\return					0 if successful, 
 *							non-zero on failure
 */
    DLL_EXPORT int twolame_set_DAB(twolame_options * glopts, int dab);

/** Get the state of the DAB extensions
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			the state of DAB (TRUE/FALSE)
 */
    DLL_EXPORT int twolame_get_DAB(twolame_options * glopts);


/** Set the number of bytes to reserve for DAB XPAD data.
 *
 *	Default: 0
 *
 *	\param glopts			pointer to twolame options pointer
 *	\param length			number of bytes to reserve
 *	\return					0 if successful, 
 *							non-zero on failure
 */
    DLL_EXPORT int twolame_set_DAB_xpad_length(twolame_options * glopts, int length);


/** Get the number of bytes reserved for DAB XPAD data.
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			number of XPAD bytes
 */
    DLL_EXPORT int twolame_get_DAB_xpad_length(twolame_options * glopts);


/** Set the CRC error protection length for DAB.
 *
 *	Default: 2
 *
 *	\param glopts			pointer to twolame options pointer
 *	\param length			length of DAB CRC
 *	\return					0 if successful, 
 *							non-zero on failure
 */
    DLL_EXPORT int twolame_set_DAB_crc_length(twolame_options * glopts, int length);


/** Get the CRC error protection length for DAB.
 *
 *	\param glopts	pointer to twolame options pointer
 *	\return			length of DAB CRC
 */
    DLL_EXPORT int twolame_get_DAB_crc_length(twolame_options * glopts);



#ifdef __cplusplus
}
#endif
#endif                          /* _TWOLAME_H_ */
// vim:ts=4:sw=4:nowrap:
