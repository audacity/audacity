/*
 * $Id: dsound_wrapper.c,v 1.3 2003-03-02 08:01:45 dmazzoni Exp $
 * Simplified DirectSound interface.
 *
 * Author: Phil Burk & Robert Marsanyi
 *
 * PortAudio Portable Real-Time Audio Library
 * For more information see: http://www.softsynth.com/portaudio/
 * DirectSound Implementation
 * Copyright (c) 1999-2000 Phil Burk & Robert Marsanyi
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
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define INITGUID     // Needed to build IID_IDirectSoundNotify. See objbase.h for info.
#include <objbase.h>
#include <unknwn.h>
#include "dsound_wrapper.h"
#include "pa_trace.h"

/************************************************************************************/
void DSW_Term( DSoundWrapper *dsw )
{
    // Cleanup the sound buffers
    if (dsw->dsw_OutputBuffer)
    {
        IDirectSoundBuffer_Stop( dsw->dsw_OutputBuffer );
        IDirectSoundBuffer_Release( dsw->dsw_OutputBuffer );
        dsw->dsw_OutputBuffer = NULL;
    }
#if SUPPORT_AUDIO_CAPTURE
    if (dsw->dsw_InputBuffer)
    {
        IDirectSoundCaptureBuffer_Stop( dsw->dsw_InputBuffer );
        IDirectSoundCaptureBuffer_Release( dsw->dsw_InputBuffer );
        dsw->dsw_InputBuffer = NULL;
    }
    if (dsw->dsw_pDirectSoundCapture)
    {
        IDirectSoundCapture_Release( dsw->dsw_pDirectSoundCapture );
        dsw->dsw_pDirectSoundCapture = NULL;
    }
#endif /* SUPPORT_AUDIO_CAPTURE */
    if (dsw->dsw_pDirectSound)
    {
        IDirectSound_Release( dsw->dsw_pDirectSound );
        dsw->dsw_pDirectSound = NULL;
    }
}
/************************************************************************************/
HRESULT DSW_Init( DSoundWrapper *dsw )
{
    memset( dsw, 0, sizeof(DSoundWrapper) );
    return 0;
}
/************************************************************************************/
HRESULT DSW_InitOutputDevice( DSoundWrapper *dsw, LPGUID lpGUID )
{
    // Create the DS object
    HRESULT hr = DirectSoundCreate( lpGUID, &dsw->dsw_pDirectSound, NULL );
    if( hr != DS_OK ) return hr;
    return hr;
}

/************************************************************************************/
HRESULT DSW_InitOutputBuffer( DSoundWrapper *dsw, unsigned long nFrameRate, int nChannels, int bytesPerBuffer )
{
    DWORD          dwDataLen;
    DWORD          playCursor;
    HRESULT        result;
    LPDIRECTSOUNDBUFFER pPrimaryBuffer;
    HWND           hWnd;
    HRESULT        hr;
    WAVEFORMATEX   wfFormat;
    DSBUFFERDESC   primaryDesc;
    DSBUFFERDESC   secondaryDesc;
    unsigned char* pDSBuffData;
    LARGE_INTEGER  counterFrequency;
    dsw->dsw_OutputSize = bytesPerBuffer;
    dsw->dsw_OutputRunning = FALSE;
    dsw->dsw_OutputUnderflows = 0;
    dsw->dsw_FramesWritten = 0;
    dsw->dsw_BytesPerFrame = nChannels * sizeof(short);
    // We were using getForegroundWindow() but sometimes the ForegroundWindow may not be the
    // applications's window. Also if that window is closed before the Buffer is closed
    // then DirectSound can crash. (Thanks for Scott Patterson for reporting this.)
    // So we will use GetDesktopWindow() which was suggested by Miller Puckette.
    // hWnd = GetForegroundWindow();
    hWnd = GetDesktopWindow();
    // Set cooperative level to DSSCL_EXCLUSIVE so that we can get 16 bit output, 44.1 KHz.
    // Exclusize also prevents unexpected sounds from other apps during a performance.
    if ((hr = IDirectSound_SetCooperativeLevel( dsw->dsw_pDirectSound,
              hWnd, DSSCL_EXCLUSIVE)) != DS_OK)
    {
        return hr;
    }
    // -----------------------------------------------------------------------
    // Create primary buffer and set format just so we can specify our custom format.
    // Otherwise we would be stuck with the default which might be 8 bit or 22050 Hz.
    // Setup the primary buffer description
    ZeroMemory(&primaryDesc, sizeof(DSBUFFERDESC));
    primaryDesc.dwSize        = sizeof(DSBUFFERDESC);
    primaryDesc.dwFlags       = DSBCAPS_PRIMARYBUFFER; // all panning, mixing, etc done by synth
    primaryDesc.dwBufferBytes = 0;
    primaryDesc.lpwfxFormat   = NULL;
    // Create the buffer
    if ((result = IDirectSound_CreateSoundBuffer( dsw->dsw_pDirectSound,
                  &primaryDesc, &pPrimaryBuffer, NULL)) != DS_OK) return result;
    // Define the buffer format
    wfFormat.wFormatTag = WAVE_FORMAT_PCM;
    wfFormat.nChannels = nChannels;
    wfFormat.nSamplesPerSec = nFrameRate;
    wfFormat.wBitsPerSample = 8 * sizeof(short);
    wfFormat.nBlockAlign = wfFormat.nChannels * wfFormat.wBitsPerSample / 8;
    wfFormat.nAvgBytesPerSec = wfFormat.nSamplesPerSec * wfFormat.nBlockAlign;
    wfFormat.cbSize = 0;  /* No extended format info. */
    // Set the primary buffer's format
    if((result = IDirectSoundBuffer_SetFormat( pPrimaryBuffer, &wfFormat)) != DS_OK) return result;
    // ----------------------------------------------------------------------
    // Setup the secondary buffer description
    ZeroMemory(&secondaryDesc, sizeof(DSBUFFERDESC));
    secondaryDesc.dwSize = sizeof(DSBUFFERDESC);
    secondaryDesc.dwFlags =  DSBCAPS_GLOBALFOCUS | DSBCAPS_GETCURRENTPOSITION2;
    secondaryDesc.dwBufferBytes = bytesPerBuffer;
    secondaryDesc.lpwfxFormat = &wfFormat;
    // Create the secondary buffer
    if ((result = IDirectSound_CreateSoundBuffer( dsw->dsw_pDirectSound,
                  &secondaryDesc, &dsw->dsw_OutputBuffer, NULL)) != DS_OK) return result;
    // Lock the DS buffer
    if ((result = IDirectSoundBuffer_Lock( dsw->dsw_OutputBuffer, 0, dsw->dsw_OutputSize, (LPVOID*)&pDSBuffData,
                                           &dwDataLen, NULL, 0, 0)) != DS_OK) return result;
    // Zero the DS buffer
    ZeroMemory(pDSBuffData, dwDataLen);
    // Unlock the DS buffer
    if ((result = IDirectSoundBuffer_Unlock( dsw->dsw_OutputBuffer, pDSBuffData, dwDataLen, NULL, 0)) != DS_OK) return result;
    if( QueryPerformanceFrequency( &counterFrequency ) )
    {
        int framesInBuffer = bytesPerBuffer / (nChannels * sizeof(short));
        dsw->dsw_CounterTicksPerBuffer.QuadPart = (counterFrequency.QuadPart * framesInBuffer) / nFrameRate;
        AddTraceMessage("dsw_CounterTicksPerBuffer = %d\n", dsw->dsw_CounterTicksPerBuffer.LowPart );
    }
    else
    {
        dsw->dsw_CounterTicksPerBuffer.QuadPart = 0;
    }
    // Let DSound set the starting write position because if we set it to zero, it looks like the
    // buffer is full to begin with. This causes a long pause before sound starts when using large buffers.
    hr = IDirectSoundBuffer_GetCurrentPosition( dsw->dsw_OutputBuffer, &playCursor, &dsw->dsw_WriteOffset );
    if( hr != DS_OK )
    {
        return hr;
    }
    dsw->dsw_FramesWritten = dsw->dsw_WriteOffset / dsw->dsw_BytesPerFrame;
    /* printf("DSW_InitOutputBuffer: playCursor = %d, writeCursor = %d\n", playCursor, dsw->dsw_WriteOffset ); */
    return DS_OK;
}

/************************************************************************************/
HRESULT DSW_StartOutput( DSoundWrapper *dsw )
{
    HRESULT        hr;
    QueryPerformanceCounter( &dsw->dsw_LastPlayTime );
    dsw->dsw_LastPlayCursor = 0;
    dsw->dsw_FramesPlayed = 0;
    hr = IDirectSoundBuffer_SetCurrentPosition( dsw->dsw_OutputBuffer, 0 );
    if( hr != DS_OK )
    {
        return hr;
    }
    // Start the buffer playback in a loop.
    if( dsw->dsw_OutputBuffer != NULL )
    {
        hr = IDirectSoundBuffer_Play( dsw->dsw_OutputBuffer, 0, 0, DSBPLAY_LOOPING );
        if( hr != DS_OK )
        {
            return hr;
        }
        dsw->dsw_OutputRunning = TRUE;
    }

    return 0;
}
/************************************************************************************/
HRESULT DSW_StopOutput( DSoundWrapper *dsw )
{
    // Stop the buffer playback
    if( dsw->dsw_OutputBuffer != NULL )
    {
        dsw->dsw_OutputRunning = FALSE;
        return IDirectSoundBuffer_Stop( dsw->dsw_OutputBuffer );
    }
    else return 0;
}

/************************************************************************************/
HRESULT DSW_QueryOutputSpace( DSoundWrapper *dsw, long *bytesEmpty )
{
    HRESULT hr;
    DWORD   playCursor;
    DWORD   writeCursor;
    long    numBytesEmpty;
    long    playWriteGap;
    // Query to see how much room is in buffer.
    // Note: Even though writeCursor is not used, it must be passed to prevent DirectSound from dieing
    // under WinNT. The Microsoft documentation says we can pass NULL but apparently not.
    // Thanks to Max Rheiner for the fix.
    hr = IDirectSoundBuffer_GetCurrentPosition( dsw->dsw_OutputBuffer, &playCursor, &writeCursor );
    if( hr != DS_OK )
    {
        return hr;
    }
    AddTraceMessage("playCursor", playCursor);
    AddTraceMessage("dsw_WriteOffset", dsw->dsw_WriteOffset);
    // Determine size of gap between playIndex and WriteIndex that we cannot write into.
    playWriteGap = writeCursor - playCursor;
    if( playWriteGap < 0 ) playWriteGap += dsw->dsw_OutputSize; // unwrap
    /* DirectSound doesn't have a large enough playCursor so we cannot detect wrap-around. */
    /* Attempt to detect playCursor wrap-around and correct it. */
    if( dsw->dsw_OutputRunning && (dsw->dsw_CounterTicksPerBuffer.QuadPart != 0) )
    {
        /* How much time has elapsed since last check. */
        LARGE_INTEGER   currentTime;
        LARGE_INTEGER   elapsedTime;
        long            bytesPlayed;
        long            bytesExpected;
        long            buffersWrapped;
        QueryPerformanceCounter( &currentTime );
        elapsedTime.QuadPart = currentTime.QuadPart - dsw->dsw_LastPlayTime.QuadPart;
        dsw->dsw_LastPlayTime = currentTime;
        /* How many bytes does DirectSound say have been played. */
        bytesPlayed = playCursor - dsw->dsw_LastPlayCursor;
        if( bytesPlayed < 0 ) bytesPlayed += dsw->dsw_OutputSize; // unwrap
        dsw->dsw_LastPlayCursor = playCursor;
        /* Calculate how many bytes we would have expected to been played by now. */
        bytesExpected = (long) ((elapsedTime.QuadPart * dsw->dsw_OutputSize) / dsw->dsw_CounterTicksPerBuffer.QuadPart);
        buffersWrapped = (bytesExpected - bytesPlayed) / dsw->dsw_OutputSize;
        if( buffersWrapped > 0 )
        {
            AddTraceMessage("playCursor wrapped! bytesPlayed", bytesPlayed );
            AddTraceMessage("playCursor wrapped! bytesExpected", bytesExpected );
            playCursor += (buffersWrapped * dsw->dsw_OutputSize);
            bytesPlayed += (buffersWrapped * dsw->dsw_OutputSize);
        }
        /* Maintain frame output cursor. */
        dsw->dsw_FramesPlayed += (bytesPlayed / dsw->dsw_BytesPerFrame);
    }
    numBytesEmpty = playCursor - dsw->dsw_WriteOffset;
    if( numBytesEmpty < 0 ) numBytesEmpty += dsw->dsw_OutputSize; // unwrap offset
    /* Have we underflowed? */
    if( numBytesEmpty > (dsw->dsw_OutputSize - playWriteGap) )
    {
        if( dsw->dsw_OutputRunning )
        {
            dsw->dsw_OutputUnderflows += 1;
            AddTraceMessage("underflow detected! numBytesEmpty", numBytesEmpty );
        }
        dsw->dsw_WriteOffset = writeCursor;
        numBytesEmpty = dsw->dsw_OutputSize - playWriteGap;
    }
    *bytesEmpty = numBytesEmpty;
    return hr;
}

/************************************************************************************/
HRESULT DSW_ZeroEmptySpace( DSoundWrapper *dsw )
{
    HRESULT hr;
    LPBYTE lpbuf1 = NULL;
    LPBYTE lpbuf2 = NULL;
    DWORD dwsize1 = 0;
    DWORD dwsize2 = 0;
    long  bytesEmpty;
    hr = DSW_QueryOutputSpace( dsw, &bytesEmpty ); // updates dsw_FramesPlayed
    if (hr != DS_OK) return hr;
    if( bytesEmpty == 0 ) return DS_OK;
    // Lock free space in the DS
    hr = IDirectSoundBuffer_Lock( dsw->dsw_OutputBuffer, dsw->dsw_WriteOffset, bytesEmpty, (void **) &lpbuf1, &dwsize1,
                                  (void **) &lpbuf2, &dwsize2, 0);
    if (hr == DS_OK)
    {
        // Copy the buffer into the DS
        ZeroMemory(lpbuf1, dwsize1);
        if(lpbuf2 != NULL)
        {
            ZeroMemory(lpbuf2, dwsize2);
        }
        // Update our buffer offset and unlock sound buffer
        dsw->dsw_WriteOffset = (dsw->dsw_WriteOffset + dwsize1 + dwsize2) % dsw->dsw_OutputSize;
        IDirectSoundBuffer_Unlock( dsw->dsw_OutputBuffer, lpbuf1, dwsize1, lpbuf2, dwsize2);
        dsw->dsw_FramesWritten += bytesEmpty / dsw->dsw_BytesPerFrame;
    }
    return hr;
}

/************************************************************************************/
HRESULT DSW_WriteBlock( DSoundWrapper *dsw, char *buf, long numBytes )
{
    HRESULT hr;
    LPBYTE lpbuf1 = NULL;
    LPBYTE lpbuf2 = NULL;
    DWORD dwsize1 = 0;
    DWORD dwsize2 = 0;
    // Lock free space in the DS
    hr = IDirectSoundBuffer_Lock( dsw->dsw_OutputBuffer, dsw->dsw_WriteOffset, numBytes, (void **) &lpbuf1, &dwsize1,
                                  (void **) &lpbuf2, &dwsize2, 0);
    if (hr == DS_OK)
    {
        // Copy the buffer into the DS
        CopyMemory(lpbuf1, buf, dwsize1);
        if(lpbuf2 != NULL)
        {
            CopyMemory(lpbuf2, buf+dwsize1, dwsize2);
        }
        // Update our buffer offset and unlock sound buffer
        dsw->dsw_WriteOffset = (dsw->dsw_WriteOffset + dwsize1 + dwsize2) % dsw->dsw_OutputSize;
        IDirectSoundBuffer_Unlock( dsw->dsw_OutputBuffer, lpbuf1, dwsize1, lpbuf2, dwsize2);
        dsw->dsw_FramesWritten += numBytes / dsw->dsw_BytesPerFrame;
    }
    return hr;
}

/************************************************************************************/
DWORD DSW_GetOutputStatus( DSoundWrapper *dsw )
{
    DWORD status;
    if (IDirectSoundBuffer_GetStatus( dsw->dsw_OutputBuffer, &status ) != DS_OK)
        return( DSERR_INVALIDPARAM );
    else
        return( status );
}

#if SUPPORT_AUDIO_CAPTURE
/* These routines are used to support audio input.
 * Do NOT compile these calls when using NT4 because it does
 * not support the entry points.
 */
/************************************************************************************/
HRESULT DSW_InitInputDevice( DSoundWrapper *dsw, LPGUID lpGUID )
{
    HRESULT hr = DirectSoundCaptureCreate(  lpGUID, &dsw->dsw_pDirectSoundCapture,   NULL );
    if( hr != DS_OK ) return hr;
    return hr;
}
/************************************************************************************/
HRESULT DSW_InitInputBuffer( DSoundWrapper *dsw, unsigned long nFrameRate, int nChannels, int bytesPerBuffer )
{
    DSCBUFFERDESC  captureDesc;
    WAVEFORMATEX   wfFormat;
    HRESULT        result;
    // Define the buffer format
    wfFormat.wFormatTag      = WAVE_FORMAT_PCM;
    wfFormat.nChannels       = nChannels;
    wfFormat.nSamplesPerSec  = nFrameRate;
    wfFormat.wBitsPerSample  = 8 * sizeof(short);
    wfFormat.nBlockAlign     = wfFormat.nChannels * (wfFormat.wBitsPerSample / 8);
    wfFormat.nAvgBytesPerSec = wfFormat.nSamplesPerSec * wfFormat.nBlockAlign;
    wfFormat.cbSize          = 0;   /* No extended format info. */
    dsw->dsw_InputSize = bytesPerBuffer;
    // ----------------------------------------------------------------------
    // Setup the secondary buffer description
    ZeroMemory(&captureDesc, sizeof(DSCBUFFERDESC));
    captureDesc.dwSize = sizeof(DSCBUFFERDESC);
    captureDesc.dwFlags =  0;
    captureDesc.dwBufferBytes = bytesPerBuffer;
    captureDesc.lpwfxFormat = &wfFormat;
    // Create the capture buffer
    if ((result = IDirectSoundCapture_CreateCaptureBuffer( dsw->dsw_pDirectSoundCapture,
                  &captureDesc, &dsw->dsw_InputBuffer, NULL)) != DS_OK) return result;
    dsw->dsw_ReadOffset = 0;  // reset last read position to start of buffer
    return DS_OK;
}

/************************************************************************************/
HRESULT DSW_StartInput( DSoundWrapper *dsw )
{
    // Start the buffer playback
    if( dsw->dsw_InputBuffer != NULL )
    {
        return IDirectSoundCaptureBuffer_Start( dsw->dsw_InputBuffer, DSCBSTART_LOOPING );
    }
    else return 0;
}

/************************************************************************************/
HRESULT DSW_StopInput( DSoundWrapper *dsw )
{
    // Stop the buffer playback
    if( dsw->dsw_InputBuffer != NULL )
    {
        return IDirectSoundCaptureBuffer_Stop( dsw->dsw_InputBuffer );
    }
    else return 0;
}

/************************************************************************************/
HRESULT DSW_QueryInputFilled( DSoundWrapper *dsw, long *bytesFilled )
{
    HRESULT hr;
    DWORD capturePos;
    DWORD readPos;
    long  filled;
    // Query to see how much data is in buffer.
    // We don't need the capture position but sometimes DirectSound doesn't handle NULLS correctly
    // so let's pass a pointer just to be safe.
    hr = IDirectSoundCaptureBuffer_GetCurrentPosition( dsw->dsw_InputBuffer, &capturePos, &readPos );
    if( hr != DS_OK )
    {
        return hr;
    }
    filled = readPos - dsw->dsw_ReadOffset;
    if( filled < 0 ) filled += dsw->dsw_InputSize; // unwrap offset
    *bytesFilled = filled;
    return hr;
}

/************************************************************************************/
HRESULT DSW_ReadBlock( DSoundWrapper *dsw, char *buf, long numBytes )
{
    HRESULT hr;
    LPBYTE lpbuf1 = NULL;
    LPBYTE lpbuf2 = NULL;
    DWORD dwsize1 = 0;
    DWORD dwsize2 = 0;
    // Lock free space in the DS
    hr = IDirectSoundCaptureBuffer_Lock ( dsw->dsw_InputBuffer, dsw->dsw_ReadOffset, numBytes, (void **) &lpbuf1, &dwsize1,
                                          (void **) &lpbuf2, &dwsize2, 0);
    if (hr == DS_OK)
    {
        // Copy from DS to the buffer
        CopyMemory( buf, lpbuf1, dwsize1);
        if(lpbuf2 != NULL)
        {
            CopyMemory( buf+dwsize1, lpbuf2, dwsize2);
        }
        // Update our buffer offset and unlock sound buffer
        dsw->dsw_ReadOffset = (dsw->dsw_ReadOffset + dwsize1 + dwsize2) % dsw->dsw_InputSize;
        IDirectSoundCaptureBuffer_Unlock ( dsw->dsw_InputBuffer, lpbuf1, dwsize1, lpbuf2, dwsize2);
    }
    return hr;
}

#endif /* SUPPORT_AUDIO_CAPTURE */
