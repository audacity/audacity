/** @file pa_test_jack_wasapi.c
	@ingroup test_src
	@brief Print out jack information for WASAPI endpoints
	@author Reid Bishop <rbish@attglobal.net>
*/
/*
 * $Id: pa_test_jack_wasapi.c 1368 2008-03-01 00:38:27Z rbishop $
 *
 * This program uses the PortAudio Portable Audio Library.
 * For more information see: http://www.portaudio.com/
 * Copyright (c) 1999-2010 Ross Bencina and Phil Burk
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
#include <stdio.h>
#include "portaudio.h"
#include "pa_win_wasapi.h"


/*
* Helper function to determine if a given enum is present in mask variable
*
*/
static int IsInMask(int val, int val2)
{
	return ((val & val2) == val2);
}

/*
* This routine enumerates through the ChannelMapping for the IJackDescription
*/

static void EnumIJackChannels(int channelMapping)
{
	printf("Channel Mapping: ");
	if(channelMapping == PAWIN_SPEAKER_DIRECTOUT)
	{
		printf("DIRECTOUT\n");
		return;
	}
	if(IsInMask(channelMapping, PAWIN_SPEAKER_FRONT_LEFT))
		printf("FRONT_LEFT, ");
	if(IsInMask(channelMapping, PAWIN_SPEAKER_FRONT_RIGHT))
		printf("FRONT_RIGHT, ");
	if(IsInMask(channelMapping, PAWIN_SPEAKER_FRONT_CENTER))
		printf("FRONT_CENTER, ");
	if(IsInMask(channelMapping, PAWIN_SPEAKER_LOW_FREQUENCY))
		printf("LOW_FREQUENCY, ");
	if(IsInMask(channelMapping, PAWIN_SPEAKER_BACK_LEFT))
		printf("BACK_LEFT, ");
	if(IsInMask(channelMapping,PAWIN_SPEAKER_BACK_RIGHT))
		printf("BACK_RIGHT, ");
	if(IsInMask(channelMapping, PAWIN_SPEAKER_FRONT_LEFT_OF_CENTER))
		printf("FRONT_LEFT_OF_CENTER, ");
	if(IsInMask(channelMapping, PAWIN_SPEAKER_FRONT_RIGHT_OF_CENTER))
		printf("FRONT_RIGHT_OF_CENTER, ");
	if(IsInMask(channelMapping, PAWIN_SPEAKER_BACK_CENTER))
		printf("BACK_CENTER, ");
	if(IsInMask(channelMapping,PAWIN_SPEAKER_SIDE_LEFT))
		printf("SIDE_LEFT, ");
	if(IsInMask(channelMapping,PAWIN_SPEAKER_SIDE_RIGHT))
		printf("SIDE_RIGHT, ");
	if(IsInMask(channelMapping, PAWIN_SPEAKER_TOP_CENTER))
		printf("TOP_CENTER, ");
	if(IsInMask(channelMapping, PAWIN_SPEAKER_TOP_FRONT_LEFT))
		printf("TOP_FRONT_LEFT, ");
	if(IsInMask(channelMapping, PAWIN_SPEAKER_TOP_FRONT_CENTER))
		printf("TOP_FRONT_CENTER, ");
	if(IsInMask(channelMapping, PAWIN_SPEAKER_TOP_FRONT_RIGHT))
		printf("TOP_FRONT_RIGHT, ");
	if(IsInMask(channelMapping, PAWIN_SPEAKER_TOP_BACK_LEFT))
		printf("TOP_BACK_LEFT, ");
	if(IsInMask(channelMapping, PAWIN_SPEAKER_TOP_BACK_CENTER))
		printf("TOP_BACK_CENTER, ");
	if(IsInMask(channelMapping, PAWIN_SPEAKER_TOP_BACK_RIGHT))
		printf("TOP_BACK_RIGHT, ");

	printf("\n");
}

/*
* This routine enumerates through the Jack Connection Types enums for IJackDescription
*/
static void EnumIJackConnectionType(int cType)
{
	printf("Connection Type: ");
	switch(cType)
	{
		case eJackConnTypeUnknown:
			printf("eJackConnTypeUnknown");
			break;
		case eJackConnType3Point5mm:
			printf("eJackConnType3Point5mm");
			break;
		case eJackConnTypeQuarter:
			printf("eJackConnTypeQuarter");
			break;
		case eJackConnTypeAtapiInternal:
			printf("eJackConnTypeAtapiInternal");
			break;
		case eJackConnTypeRCA:
			printf("eJackConnTypeRCA");
			break;
		case eJackConnTypeOptical:
			printf("eJackConnTypeOptical");
			break;
		case eJackConnTypeOtherDigital:
			printf("eJackConnTypeOtherDigital");
			break;
		case eJackConnTypeOtherAnalog:
			printf("eJackConnTypeOtherAnalog");
			break;
		case eJackConnTypeMultichannelAnalogDIN:
			printf("eJackConnTypeMultichannelAnalogDIN");
			break;
		case eJackConnTypeXlrProfessional:
			printf("eJackConnTypeXlrProfessional");
			break;
		case eJackConnTypeRJ11Modem:
			printf("eJackConnTypeRJ11Modem");
			break;
		case eJackConnTypeCombination:
			printf("eJackConnTypeCombination");
			break;
	}
	printf("\n");
}

/*
* This routine enumerates through the GeoLocation enums for the IJackDescription
*/
static void EnumIJackGeoLocation(int iVal)
{
	printf("Geometric Location: ");
	switch(iVal)
	{
	case eJackGeoLocRear:
		printf("eJackGeoLocRear");
		break;
	case eJackGeoLocFront:
		printf("eJackGeoLocFront");
		break;
	case eJackGeoLocLeft:
		printf("eJackGeoLocLeft");
		break;
	case eJackGeoLocRight:
		printf("eJackGeoLocRight");
		break;
	case eJackGeoLocTop:
		printf("eJackGeoLocTop");
		break;
	case eJackGeoLocBottom:
		printf("eJackGeoLocBottom");
		break;
	case eJackGeoLocRearPanel:
		printf("eJackGeoLocRearPanel");
		break;
	case eJackGeoLocRiser:
		printf("eJackGeoLocRiser");
		break;
	case eJackGeoLocInsideMobileLid:
		printf("eJackGeoLocInsideMobileLid");
		break;
	case eJackGeoLocDrivebay:
		printf("eJackGeoLocDrivebay");
		break;
	case eJackGeoLocHDMI:
		printf("eJackGeoLocHDMI");
		break;
	case eJackGeoLocOutsideMobileLid:
		printf("eJackGeoLocOutsideMobileLid");
		break;
	case eJackGeoLocATAPI:
		printf("eJackGeoLocATAPI");
		break;
	}
	printf("\n");
}

/*
* This routine enumerates through the GenLocation enums for the IJackDescription
*/
static void EnumIJackGenLocation(int iVal)
{
	printf("General Location: ");
	switch(iVal)
	{
		case eJackGenLocPrimaryBox:
			printf("eJackGenLocPrimaryBox");
			break;
		case eJackGenLocInternal:
			printf("eJackGenLocInternal");
			break;
		case eJackGenLocSeparate:
			printf("eJackGenLocSeparate");
			break;
		case eJackGenLocOther:
			printf("eJackGenLocOther");
			break;
	}
	printf("\n");
}

/*
* This routine enumerates through the PortConnection enums for the IJackDescription
*/
static void EnumIJackPortConnection(int iVal)
{
	printf("Port Type: ");
	switch(iVal)
	{
		case eJackPortConnJack:
			printf("eJackPortConnJack");
			break;
		case eJackPortConnIntegratedDevice:
			printf("eJackPortConnIntegratedDevice");
			break;
		case eJackPortConnBothIntegratedAndJack:
			printf("eJackPortConnBothIntegratedAndJack");
			break;
		case eJackPortConnUnknown:
			printf("eJackPortConnUnknown");
			break;
	}
	printf("\n");
}

/*
* This routine retrieves and parses the KSJACK_DESCRIPTION structure for
* the provided device ID.
*/
static PaError GetJackInformation(int deviceId)
{
    PaError err;
	int i;
	int jackCount = 0;
	PaWasapiJackDescription jackDesc;

	err = PaWasapi_GetJackCount(deviceId, &jackCount);
	if( err != paNoError ) return err;

	fprintf( stderr,"Number of Jacks: %d \n", jackCount );

	for( i = 0; i<jackCount; i++ )
	{
		fprintf( stderr,"Jack #%d:\n", i );

		err = PaWasapi_GetJackDescription(deviceId, i, &jackDesc);
		if( err != paNoError ) 
		{
			fprintf( stderr,"Failed getting description." );
			continue;
		}
		else
		{
			printf("Is connected: %s\n",(jackDesc.isConnected)?"true":"false");
			EnumIJackChannels(jackDesc.channelMapping);
			EnumIJackConnectionType(jackDesc.connectionType);
			EnumIJackGeoLocation(jackDesc.geoLocation);
			EnumIJackGenLocation(jackDesc.genLocation);
			EnumIJackPortConnection(jackDesc.portConnection);
			printf("Jack Color: 0x%06X\n", jackDesc.color);
			printf("\n");
		}
	}
	return 0;
}


/*******************************************************************/
int main(void);
int main(void)
{
    PaError err;
	const PaDeviceInfo *device;
    int i;
	int jackCount = 0;
	int isInput = 0;

	printf("PortAudio Test: WASAPI Jack Configuratin");
    err = Pa_Initialize();
    if( err != paNoError ) goto error;

	/* Find all WASAPI devices */
	for( i = 0; i < Pa_GetDeviceCount(); ++i )
	{
		device = Pa_GetDeviceInfo(i);
		if( Pa_GetDeviceInfo(i)->hostApi == Pa_HostApiTypeIdToHostApiIndex(paWASAPI) )
		{
			if( device->maxOutputChannels == 0 )
			{
				isInput = 1;
			}
			printf("------------------------------------------\n");
			printf("Device: %s",device->name);
			if(isInput)
				printf("  (Input) %d Channels\n",device->maxInputChannels);
			else
				printf("  (Output) %d Channels\n",device->maxOutputChannels);
			// Try to see if this WASAPI device can provide Jack information
			err = GetJackInformation(i);
			if( err != paNoError ) goto error;
		}
	}
    Pa_Terminate();
    printf("Test finished.\n");
    return err;

error:
    Pa_Terminate();
    fprintf( stderr, "An error occured while using the portaudio stream\n" );
    fprintf( stderr, "Error number: %d\n", err );
    fprintf( stderr, "Error message: %s\n", Pa_GetErrorText( err ) );
    return err;
}
