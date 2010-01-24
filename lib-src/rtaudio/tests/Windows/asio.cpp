/*
	Steinberg Audio Stream I/O API
	(c) 1996, Steinberg Soft- und Hardware GmbH

	asio.cpp
	
	asio functions entries which translate the
	asio interface to the asiodrvr class methods
*/ 
	
#include <string.h>
#include "asiosys.h"		// platform definition
#include "asio.h"

#if MAC
#include "asiodrvr.h"

#pragma export on

AsioDriver *theAsioDriver = 0;

extern "C"
{

long main()
{
	return 'ASIO';
}

#elif WINDOWS

#include "windows.h"
#include "iasiodrv.h"
#include "asiodrivers.h"

IASIO *theAsioDriver = 0;
extern AsioDrivers *asioDrivers;

#elif SGI || SUN || BEOS || LINUX
#include "asiodrvr.h"
static AsioDriver *theAsioDriver = 0;
#endif

//-----------------------------------------------------------------------------------------------------
ASIOError ASIOInit(ASIODriverInfo *info)
{
#if MAC || SGI || SUN || BEOS || LINUX
	if(theAsioDriver)
	{
		delete theAsioDriver;
		theAsioDriver = 0;
	}		
	info->driverVersion = 0;
	strcpy(info->name, "No ASIO Driver");
	theAsioDriver = getDriver();
	if(!theAsioDriver)
	{
		strcpy(info->errorMessage, "Not enough memory for the ASIO driver!"); 
		return ASE_NotPresent;
	}
	if(!theAsioDriver->init(info->sysRef))
	{
		theAsioDriver->getErrorMessage(info->errorMessage);
		delete theAsioDriver;
		theAsioDriver = 0;
		return ASE_NotPresent;
	}
	strcpy(info->errorMessage, "No ASIO Driver Error");
	theAsioDriver->getDriverName(info->name);
	info->driverVersion = theAsioDriver->getDriverVersion();
	return ASE_OK;

#else

	info->driverVersion = 0;
	strcpy(info->name, "No ASIO Driver");
	if(theAsioDriver)	// must be loaded!
	{
		if(!theAsioDriver->init(info->sysRef))
		{
			theAsioDriver->getErrorMessage(info->errorMessage);
			theAsioDriver = 0;
			return ASE_NotPresent;
		}		

		strcpy(info->errorMessage, "No ASIO Driver Error");
		theAsioDriver->getDriverName(info->name);
		info->driverVersion = theAsioDriver->getDriverVersion();
		return ASE_OK;
	}
	return ASE_NotPresent;

#endif	// !MAC
}

ASIOError ASIOExit(void)
{
	if(theAsioDriver)
	{
#if WINDOWS
		asioDrivers->removeCurrentDriver();
#else
		delete theAsioDriver;
#endif
	}		
	theAsioDriver = 0;
	return ASE_OK;
}

ASIOError ASIOStart(void)
{
	if(!theAsioDriver)
		return ASE_NotPresent;
	return theAsioDriver->start();
}

ASIOError ASIOStop(void)
{
	if(!theAsioDriver)
		return ASE_NotPresent;
	return theAsioDriver->stop();
}

ASIOError ASIOGetChannels(long *numInputChannels, long *numOutputChannels)
{
	if(!theAsioDriver)
	{
		*numInputChannels = *numOutputChannels = 0;
		return ASE_NotPresent;
	}
	return theAsioDriver->getChannels(numInputChannels, numOutputChannels);
}

ASIOError ASIOGetLatencies(long *inputLatency, long *outputLatency)
{
	if(!theAsioDriver)
	{
		*inputLatency = *outputLatency = 0;
		return ASE_NotPresent;
	}
	return theAsioDriver->getLatencies(inputLatency, outputLatency);
}

ASIOError ASIOGetBufferSize(long *minSize, long *maxSize, long *preferredSize, long *granularity)
{
	if(!theAsioDriver)
	{
		*minSize = *maxSize = *preferredSize = *granularity = 0;
		return ASE_NotPresent;
	}
	return theAsioDriver->getBufferSize(minSize, maxSize, preferredSize, granularity);
}

ASIOError ASIOCanSampleRate(ASIOSampleRate sampleRate)
{
	if(!theAsioDriver)
		return ASE_NotPresent;
	return theAsioDriver->canSampleRate(sampleRate);
}

ASIOError ASIOGetSampleRate(ASIOSampleRate *currentRate)
{
	if(!theAsioDriver)
		return ASE_NotPresent;
	return theAsioDriver->getSampleRate(currentRate);
}

ASIOError ASIOSetSampleRate(ASIOSampleRate sampleRate)
{
	if(!theAsioDriver)
		return ASE_NotPresent;
	return theAsioDriver->setSampleRate(sampleRate);
}

ASIOError ASIOGetClockSources(ASIOClockSource *clocks, long *numSources)
{
	if(!theAsioDriver)
	{
		*numSources = 0;
		return ASE_NotPresent;
	}
	return theAsioDriver->getClockSources(clocks, numSources);
}

ASIOError ASIOSetClockSource(long reference)
{
	if(!theAsioDriver)
		return ASE_NotPresent;
	return theAsioDriver->setClockSource(reference);
}

ASIOError ASIOGetSamplePosition(ASIOSamples *sPos, ASIOTimeStamp *tStamp)
{
	if(!theAsioDriver)
		return ASE_NotPresent;
	return theAsioDriver->getSamplePosition(sPos, tStamp);
}

ASIOError ASIOGetChannelInfo(ASIOChannelInfo *info)
{
	if(!theAsioDriver)
	{
		info->channelGroup = -1;
		info->type = ASIOSTInt16MSB;
		strcpy(info->name, "None");
		return ASE_NotPresent;
	}
	return theAsioDriver->getChannelInfo(info);
}

ASIOError ASIOCreateBuffers(ASIOBufferInfo *bufferInfos, long numChannels,
	long bufferSize, ASIOCallbacks *callbacks)
{
	if(!theAsioDriver)
	{
		ASIOBufferInfo *info = bufferInfos;
		for(long i = 0; i < numChannels; i++, info++)
			info->buffers[0] = info->buffers[1] = 0;
		return ASE_NotPresent;
	}
	return theAsioDriver->createBuffers(bufferInfos, numChannels, bufferSize, callbacks);
}

ASIOError ASIODisposeBuffers(void)
{
	if(!theAsioDriver)
		return ASE_NotPresent;
	return theAsioDriver->disposeBuffers();
}

ASIOError ASIOControlPanel(void)
{
	if(!theAsioDriver)
		return ASE_NotPresent;
	return theAsioDriver->controlPanel();
}

ASIOError ASIOFuture(long selector, void *opt)
{
	if(!theAsioDriver)
		return ASE_NotPresent;
	return theAsioDriver->future(selector, opt);
}

ASIOError ASIOOutputReady(void)
{
	if(!theAsioDriver)
		return ASE_NotPresent;
	return theAsioDriver->outputReady();
}

#if MAC
}	// extern "C"
#pragma export off
#endif


