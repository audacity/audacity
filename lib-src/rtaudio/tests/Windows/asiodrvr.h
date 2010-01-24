/*
	Steinberg Audio Stream I/O API
	(c) 1996, Steinberg Soft- und Hardware GmbH
	charlie (May 1996)

	asiodrvr.h
	c++ superclass to implement asio functionality. from this,
	you can derive whatever required
*/

#ifndef _asiodrvr_
#define _asiodrvr_

// cpu and os system we are running on
#include "asiosys.h"
// basic "C" interface
#include "asio.h"

class AsioDriver;
extern AsioDriver *getDriver();		// for generic constructor 

#if WINDOWS
#include <windows.h>
#include "combase.h"
#include "iasiodrv.h"
class AsioDriver : public IASIO ,public CUnknown
{
public:
	AsioDriver(LPUNKNOWN pUnk, HRESULT *phr);

	DECLARE_IUNKNOWN
	// Factory method
	static CUnknown *CreateInstance(LPUNKNOWN pUnk, HRESULT *phr);
	// IUnknown
	virtual HRESULT STDMETHODCALLTYPE NonDelegatingQueryInterface(REFIID riid,void **ppvObject);

#else

class AsioDriver
{
public:
	AsioDriver();
#endif
	virtual ~AsioDriver();

	virtual ASIOBool init(void* sysRef);
	virtual void getDriverName(char *name);	// max 32 bytes incl. terminating zero
	virtual long getDriverVersion();
	virtual void getErrorMessage(char *string);	// max 124 bytes incl.

	virtual ASIOError start();
	virtual ASIOError stop();

	virtual ASIOError getChannels(long *numInputChannels, long *numOutputChannels);
	virtual ASIOError getLatencies(long *inputLatency, long *outputLatency);
	virtual ASIOError getBufferSize(long *minSize, long *maxSize,
		long *preferredSize, long *granularity);

	virtual ASIOError canSampleRate(ASIOSampleRate sampleRate);
	virtual ASIOError getSampleRate(ASIOSampleRate *sampleRate);
	virtual ASIOError setSampleRate(ASIOSampleRate sampleRate);
	virtual ASIOError getClockSources(ASIOClockSource *clocks, long *numSources);
	virtual ASIOError setClockSource(long reference);

	virtual ASIOError getSamplePosition(ASIOSamples *sPos, ASIOTimeStamp *tStamp);
	virtual ASIOError getChannelInfo(ASIOChannelInfo *info);

	virtual ASIOError createBuffers(ASIOBufferInfo *bufferInfos, long numChannels,
		long bufferSize, ASIOCallbacks *callbacks);
	virtual ASIOError disposeBuffers();

	virtual ASIOError controlPanel();
	virtual ASIOError future(long selector, void *opt);
	virtual ASIOError outputReady();
};
#endif
