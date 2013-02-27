
/*
 * PortAudio Portable Real-Time Audio Library
 * Latest Version at: http://www.portaudio.com
 *
 * Copyright (c) 1999-2010 Phil Burk and Ross Bencina
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
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

/*
 * The text above constitutes the entire PortAudio license; however, 
 * the PortAudio community also makes the following non-binding requests:
 *
 * Any person wishing to distribute modifications to the Software is
 * requested to send the modifications to the original developer so that
 * they can be incorporated into the canonical version. It is also 
 * requested that these non-binding requests be included along with the 
 * license above.
 */

#include "paqa_tools.h"


/*******************************************************************/
void PaQa_ListAudioDevices(void)
{
    int     i, numDevices;
    const   PaDeviceInfo *deviceInfo;		
    numDevices = Pa_GetDeviceCount();    
    for( i=0; i<numDevices; i++ )
    {
        deviceInfo = Pa_GetDeviceInfo( i );
        printf( "#%d: ", i );
        printf( "%2d in", deviceInfo->maxInputChannels  );
        printf( ", %2d out", deviceInfo->maxOutputChannels  );
		printf( ",  %s", deviceInfo->name );
        printf( ", on %s\n",  Pa_GetHostApiInfo( deviceInfo->hostApi )->name );
	}
}

/*******************************************************************/
void PaQa_ConvertToFloat( const void *input, int numSamples, PaSampleFormat inFormat, float *output )
{
	int i;
	switch( inFormat )
	{
		case paUInt8:
		{
			unsigned char *data = (unsigned char *)input;
			for( i=0; i<numSamples; i++ )
			{
				int value = *data++;
				value -= 128;
				*output++ = value / 128.0f;
			}
		}
			break;
			
		case paInt8:
		{
			char *data = (char *)input;
			for( i=0; i<numSamples; i++ )
			{
				int value = *data++;
				*output++ = value / 128.0f;
			}
		}
			break;
			
		case paInt16:
		{
			short *data = (short *)input;
			for( i=0; i<numSamples; i++ )
			{
				*output++ = *data++ / 32768.0f;
			}
		}
			break;
			
		case paInt32:
		{
			int *data = (int *)input;
			for( i=0; i<numSamples; i++ )
			{
				int value = (*data++) >> 8;
				float fval = (float) (value / ((double) 0x00800000));
				*output++ = fval;
			}
		}
			break;
	}
	
}

/*******************************************************************/
void PaQa_ConvertFromFloat( const float *input, int numSamples, PaSampleFormat outFormat, void *output )
{
	int i;
	switch( outFormat )
	{
		case paUInt8:
		{
			unsigned char *data = (unsigned char *)output;
			for( i=0; i<numSamples; i++ )
			{
				float value = *input++;
				int byte = ((int) (value * 127)) + 128;
				*data++ = (unsigned char) byte;
			}
		}
			break;
			
		case paInt8:
		{
			char *data = (char *)output;
			for( i=0; i<numSamples; i++ )
			{
				float value = *input++;
				int byte = (int) (value * 127);
				*data++ = (char) byte;
			}
		}
			break;
			
		case paInt16:
		{
			short *data = (short *)output;
			for( i=0; i<numSamples; i++ )
			{
				float value = *input++;
				// Use assymmetric conversion to avoid clipping.
				short sval = value * 32767.0;
				*data++ = sval;
			}
		}
			break;
			
		case paInt32:
		{
			int *data = (int *)output;
			for( i=0; i<numSamples; i++ )
			{
				float value = *input++;
				// Use assymmetric conversion to avoid clipping.
				int ival = value * ((double) 0x007FFFF0);
				ival = ival << 8;
				*data++ = ival;
			}
		}
			break;
	}
	
}
