/*
 * PortAudio Portable Real-Time Audio Library
 * Latest Version at: http://www.portaudio.com
 * Linux OSS Implementation by douglas repetto and Phil Burk
 *
 * Copyright (c) 1999-2000 Phil Burk
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

/* Modification history:
   20020621: pa_unix_oss.c split into pa_unix.c, pa_unix.h, pa_unix_oss.c by
       Augustus Saunders. See pa_unix.c for previous history. Pa_FlushStream
       added by Augustus Saunders for Solaris compatibility.
   PLB20021018 - Fill device info table with actual sample rates instead of wished for rates.
               - Allow stream to open if sample rate within 10% of desired rate.
*/

#include "pa_unix.h"

#if defined(__linux__)
#include <linux/soundcard.h>
#elif defined(__FreeBSD__)
#include <sys/soundcard.h>
#else
#include <machine/soundcard.h> /* JH20010905 */
#endif


#ifndef AFMT_S16_NE
#define AFMT_S16_NE  Get_AFMT_S16_NE()
/*********************************************************************
 * Some versions of OSS do not define AFMT_S16_NE. So check CPU.
 * PowerPC is Big Endian. X86 is Little Endian.
 */
int Get_AFMT_S16_NE( void )
{
    long testData = 1; 
    char *ptr = (char *) &testData;
    int isLittle = ( *ptr == 1 ); /* Does address point to least significant byte? */
    return isLittle ? AFMT_S16_LE : AFMT_S16_BE;
}
#endif /* AFMT_S16_NE */


/*********************************************************************
 * Try to open the named device.
 * If it opens, try to set various rates and formats and fill in 
 * the device info structure.
 */
PaError Pa_QueryDevice( const char *deviceName, internalPortAudioDevice *pad )
{
    int result = paHostError;
    int tempDevHandle;
    int numChannels, maxNumChannels;
    int format;
    int numSampleRates;
    int sampleRate;
    int numRatesToTry;
    int lastRate;
    int ratesToTry[9] = {96000, 48000, 44100, 32000, 24000, 22050, 16000, 11025, 8000};
    int i;

    /* douglas:
     we have to do this querying in a slightly different order. apparently
     some sound cards will give you different info based on their settings. 
     e.g. a card might give you stereo at 22kHz but only mono at 44kHz.
     the correct order for OSS is: format, channels, sample rate
     
    */
    if ( (tempDevHandle = open(deviceName,O_WRONLY|O_NONBLOCK))  == -1 )
    {
        DBUG(("Pa_QueryDevice: could not open %s\n", deviceName ));
        return paHostError;
    }

    /*  Ask OSS what formats are supported by the hardware. */
    pad->pad_Info.nativeSampleFormats = 0;

    if (ioctl(tempDevHandle, SNDCTL_DSP_GETFMTS, &format) == -1)
    {
        ERR_RPT(("Pa_QueryDevice: could not get format info\n" ));
        goto error;
    }
    if( format & AFMT_U8 )     pad->pad_Info.nativeSampleFormats |= paUInt8;
    if( format & AFMT_S16_NE ) pad->pad_Info.nativeSampleFormats |= paInt16;

    /* Negotiate for the maximum number of channels for this device. PLB20010927
     * Consider up to 16 as the upper number of channels.
     * Variable numChannels should contain the actual upper limit after the call.
     * Thanks to John Lazzaro and Heiko Purnhagen for suggestions.
     */
    maxNumChannels = 0;
    for( numChannels = 1; numChannels <= 16; numChannels++ )
    {
        int temp = numChannels;
        DBUG(("Pa_QueryDevice: use SNDCTL_DSP_CHANNELS, numChannels = %d\n", numChannels ))
        if(ioctl(tempDevHandle, SNDCTL_DSP_CHANNELS, &temp) < 0 )
        {
            /* ioctl() failed so bail out if we already have stereo */
            if( numChannels > 2 ) break;
        }
        else
        {
            /* ioctl() worked but bail out if it does not support numChannels.
             * We don't want to leave gaps in the numChannels supported.
             */
            if( (numChannels > 2) && (temp != numChannels) ) break;
            DBUG(("Pa_QueryDevice: temp = %d\n", temp ))
            if( temp > maxNumChannels ) maxNumChannels = temp; /* Save maximum. */
        }
    }

    /* The above negotiation may fail for an old driver so try this older technique. */
    if( maxNumChannels < 1 )
    {
        int stereo = 1;
        if(ioctl(tempDevHandle, SNDCTL_DSP_STEREO, &stereo) < 0)
        {
            maxNumChannels = 1;
        }
        else
        {
            maxNumChannels = (stereo) ? 2 : 1;
        }
        DBUG(("Pa_QueryDevice: use SNDCTL_DSP_STEREO, maxNumChannels = %d\n", maxNumChannels ))
    }

    pad->pad_Info.maxOutputChannels = maxNumChannels;
    DBUG(("Pa_QueryDevice: maxNumChannels = %d\n", maxNumChannels))

    /* During channel negotiation, the last ioctl() may have failed. This can
     * also cause sample rate negotiation to fail. Hence the following, to return
     * to a supported number of channels. SG20011005 */
    {
        int temp = maxNumChannels;
        if( temp > 2 ) temp = 2; /* use most reasonable default value */
        ioctl(tempDevHandle, SNDCTL_DSP_CHANNELS, &temp);
    }

    /* FIXME - for now, assume maxInputChannels = maxOutputChannels.
     *    Eventually do separate queries for O_WRONLY and O_RDONLY
    */
    pad->pad_Info.maxInputChannels = pad->pad_Info.maxOutputChannels;

    DBUG(("Pa_QueryDevice: maxInputChannels = %d\n",
          pad->pad_Info.maxInputChannels))


    /* Determine available sample rates by trying each one and seeing result.
     * OSS often supports funky rates such as 44188 instead of 44100!
     */
    numSampleRates = 0;
    lastRate = 0;
    numRatesToTry = sizeof(ratesToTry)/sizeof(int);
    for (i = 0; i < numRatesToTry; i++)
    {
        sampleRate = ratesToTry[i];

        if (ioctl(tempDevHandle, SNDCTL_DSP_SPEED, &sampleRate) >= 0 ) /* PLB20010817 */
        {
            /* Use whatever rate OSS tells us. PLB20021018 */
            if (sampleRate != lastRate)
            {
                DBUG(("Pa_QueryDevice: adding sample rate: %d\n", sampleRate))
                pad->pad_SampleRates[numSampleRates] = (float)sampleRate;
                numSampleRates++;
                lastRate = sampleRate;
            }
            else
            {
                DBUG(("Pa_QueryDevice: dang - got sample rate %d again!\n", sampleRate))
            }
        }
    }

    DBUG(("Pa_QueryDevice: final numSampleRates = %d\n", numSampleRates))
    if (numSampleRates==0)   /* HP20010922 */
    {
        /* Desparate attempt to keep running even though no good rates found! */
        ERR_RPT(("Pa_QueryDevice: no supported sample rate (or SNDCTL_DSP_SPEED ioctl call failed). Force 44100 Hz\n" ));
        pad->pad_SampleRates[numSampleRates++] = 44100;
    }

    pad->pad_Info.numSampleRates = numSampleRates;
    pad->pad_Info.sampleRates = pad->pad_SampleRates; /* use pointer to embedded array */

    pad->pad_Info.name = deviceName;

    result = paNoError;

error:
    /* We MUST close the handle here or we won't be able to reopen it later!!!  */
    close(tempDevHandle);

    return result;
}

/*******************************************************************************************/
PaError Pa_SetupDeviceFormat( int devHandle, int numChannels, int sampleRate )
{
    PaError result = paNoError;
    int     tmp;

    /* Set format, channels, and rate in this order to keep OSS happy. */
    /* Set data format. FIXME - handle more native formats. */
    tmp = AFMT_S16_NE;
    if( ioctl(devHandle,SNDCTL_DSP_SETFMT,&tmp) == -1)
    {
        ERR_RPT(("Pa_SetupDeviceFormat: could not SNDCTL_DSP_SETFMT\n" ));
        return paHostError;
    }
    if( tmp != AFMT_S16_NE )
    {
        ERR_RPT(("Pa_SetupDeviceFormat: HW does not support AFMT_S16_NE\n" ));
        return paHostError;
    }


    /* Set number of channels. */
    tmp = numChannels;
    if (ioctl(devHandle, SNDCTL_DSP_CHANNELS, &numChannels) == -1)
    {
        ERR_RPT(("Pa_SetupDeviceFormat: could not SNDCTL_DSP_CHANNELS\n" ));
        return paHostError;
    }
    if( tmp != numChannels)
    {
        ERR_RPT(("Pa_SetupDeviceFormat: HW does not support %d channels\n", numChannels ));
        return paHostError;
    }

    /* Set playing frequency. */
    tmp = sampleRate;
    if( ioctl(devHandle,SNDCTL_DSP_SPEED,&tmp) == -1)
    {
        ERR_RPT(("Pa_SetupDeviceFormat: could not SNDCTL_DSP_SPEED\n" ));
        return paHostError;
    }
    else if( tmp != sampleRate )
    {
        int percentError = abs( (100 * (sampleRate - tmp)) / sampleRate );
        PRINT(("Pa_SetupDeviceFormat: warning - requested sample rate = %d Hz - closest = %d\n",
            sampleRate, tmp ));
        /* Allow sample rate within 10% off of requested rate. PLB20021018
        * Sometimes OSS uses a funky rate like 44188 instead of 44100.
        */
        if( percentError > 10 )
        {
            ERR_RPT(("Pa_SetupDeviceFormat: HW does not support %d Hz sample rate\n",sampleRate ));
           return paHostError;
        }
    }
    
    return result;
}

PaError Pa_SetupOutputDeviceFormat( int devHandle, int numChannels, int sampleRate )
{
  return Pa_SetupDeviceFormat(devHandle, numChannels, sampleRate);
}

PaError Pa_SetupInputDeviceFormat( int devHandle, int numChannels, int sampleRate )
{
  return Pa_SetupDeviceFormat(devHandle, numChannels, sampleRate);
}


/*******************************************************************************************
** Set number of fragments and size of fragments to achieve desired latency.
*/

static int CalcHigherLogTwo( int n )
{
    int log2 = 0;
    while( (1<<log2) < n ) log2++;
    return log2;
}

void Pa_SetLatency( int devHandle, int numBuffers, int framesPerBuffer, int channelsPerFrame  )
{
    int     tmp;
    int     bufferSize, powerOfTwo;

    /* Increase size of buffers and reduce number of buffers to reduce latency inside driver. */
    while( numBuffers > 8 )
    {
        numBuffers = (numBuffers + 1) >> 1;
        framesPerBuffer = framesPerBuffer << 1;
    }

    /* calculate size of buffers in bytes */
    bufferSize = framesPerBuffer * channelsPerFrame * sizeof(short); /* FIXME - other sizes? */

    /* Calculate next largest power of two */
    powerOfTwo = CalcHigherLogTwo( bufferSize );
    DBUG(("Pa_SetLatency: numBuffers = %d, framesPerBuffer = %d, powerOfTwo = %d\n",
          numBuffers, framesPerBuffer, powerOfTwo ));

    /* Encode info into a single int */
    tmp=(numBuffers<<16) + powerOfTwo;

    if(ioctl(devHandle,SNDCTL_DSP_SETFRAGMENT,&tmp) == -1)
    {
        ERR_RPT(("Pa_SetLatency: could not SNDCTL_DSP_SETFRAGMENT\n" ));
        /* Don't return an error. Best to just continue and hope for the best. */
        ERR_RPT(("Pa_SetLatency: numBuffers = %d, framesPerBuffer = %d, powerOfTwo = %d\n",
                 numBuffers, framesPerBuffer, powerOfTwo ));
    }
}

/***********************************************************************/
PaTimestamp Pa_StreamTime( PortAudioStream *stream )
{
    internalPortAudioStream *past = (internalPortAudioStream *) stream;
    PaHostSoundControl *pahsc;

    count_info    info;
    unsigned int  delta;
    unsigned int  numchan;

    if( past == NULL ) return paBadStreamPtr;
    
    pahsc = (PaHostSoundControl *) past->past_DeviceData;

    if( pahsc->pahsc_NativeOutputBuffer )
    {
       ioctl(pahsc->pahsc_OutputHandle, SNDCTL_DSP_GETOPTR, &info);
       numchan = past->past_NumOutputChannels;
    }
    else
    {
       ioctl(pahsc->pahsc_InputHandle, SNDCTL_DSP_GETIPTR, &info);
       numchan = past->past_NumInputChannels;
    }
    
    delta = info.bytes - pahsc->pahsc_LastPosPtr;
    
    if (delta > 0x000FFFFF)
    	delta = 0;
    	
    return (pahsc->pahsc_LastStreamBytes + delta) / (numchan * sizeof(short));
}

void Pa_UpdateStreamTime(PaHostSoundControl *pahsc)
{
    count_info   info;
    unsigned int delta;

  /* Update current stream time (using a double so that
     we don't wrap around like info.bytes does) */
  if( pahsc->pahsc_NativeOutputBuffer )
  {
    ioctl(pahsc->pahsc_OutputHandle, SNDCTL_DSP_GETOPTR, &info);
  }
  else
  {
    ioctl(pahsc->pahsc_InputHandle, SNDCTL_DSP_GETIPTR, &info);
  }
  delta = info.bytes - pahsc->pahsc_LastPosPtr;
  
  if (delta <= 0x000FFFFF) {
  	pahsc->pahsc_LastStreamBytes += delta;
  	pahsc->pahsc_LastPosPtr = info.bytes;
	}
}

PaError Pa_FlushStream(int devHandle)
{
  /* AS: This doesn't do anything under OSS; it was added for Solaris.*/

  return paNoError;
}
