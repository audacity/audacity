/*
 * $Id: $
 * Portable Audio I/O Library
 * Windows MME low level buffer user guided parameters search
 *
 * Copyright (c) 2010 Ross Bencina
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
#include <time.h>
#include <math.h>

#define  _WIN32_WINNT 0x0501 /* for GetNativeSystemInfo */ 
#include <windows.h>    /* required when using pa_win_wmme.h */
#include <mmsystem.h>   /* required when using pa_win_wmme.h */

#include <conio.h>      /* for _getch */


#include "portaudio.h"
#include "pa_win_wmme.h"


#define DEFAULT_SAMPLE_RATE             (44100.)

#ifndef M_PI
#define M_PI  (3.14159265)
#endif

#define TABLE_SIZE              (2048)

#define CHANNEL_COUNT           (2)


/* seach parameters. we test all buffer counts in this range */
#define MIN_WMME_BUFFER_COUNT        (2)
#define MAX_WMME_BUFFER_COUNT        (12)


/*******************************************************************/
/* functions to query and print Windows version information */

typedef BOOL (WINAPI *LPFN_ISWOW64PROCESS) (HANDLE, PBOOL);

LPFN_ISWOW64PROCESS fnIsWow64Process;

static BOOL IsWow64()
{
    BOOL bIsWow64 = FALSE;

    //IsWow64Process is not available on all supported versions of Windows.
    //Use GetModuleHandle to get a handle to the DLL that contains the function
    //and GetProcAddress to get a pointer to the function if available.

    fnIsWow64Process = (LPFN_ISWOW64PROCESS) GetProcAddress(
        GetModuleHandle(TEXT("kernel32")),"IsWow64Process" );

    if(NULL != fnIsWow64Process)
    {
        if (!fnIsWow64Process(GetCurrentProcess(),&bIsWow64))
        {
            //handle error
        }
    }
    return bIsWow64;
}

static void printWindowsVersionInfo( FILE *fp )
{
    OSVERSIONINFOEX osVersionInfoEx;
    SYSTEM_INFO systemInfo;
    const char *osName = "Unknown";
    const char *osProductType = "";
    const char *processorArchitecture = "Unknown";

    memset( &osVersionInfoEx, 0, sizeof(OSVERSIONINFOEX) );
    osVersionInfoEx.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);
    GetVersionEx( &osVersionInfoEx );

    
    if( osVersionInfoEx.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS ){
        switch( osVersionInfoEx.dwMinorVersion ){
            case 0: osName = "Windows 95"; break;
            case 10: osName = "Windows 98"; break;  // could also be 98SE (I've seen code discriminate based 
                                                    // on osInfo.Version.Revision.ToString() == "2222A")
            case 90: osName = "Windows Me"; break;
        }
    }else if( osVersionInfoEx.dwPlatformId == VER_PLATFORM_WIN32_NT ){
        switch( osVersionInfoEx.dwMajorVersion ){
            case 3: osName = "Windows NT 3.51"; break;
            case 4: osName = "Windows NT 4.0"; break;
            case 5: switch( osVersionInfoEx.dwMinorVersion ){
                        case 0: osName = "Windows 2000"; break;
                        case 1: osName = "Windows XP"; break;
                        case 2:
                            if( osVersionInfoEx.wSuiteMask & 0x00008000 /*VER_SUITE_WH_SERVER*/ ){
                                osName = "Windows Home Server";
                            }else{
                                if( osVersionInfoEx.wProductType == VER_NT_WORKSTATION ){
                                    osName = "Windows XP Professional x64 Edition (?)";
                                }else{
                                    if( GetSystemMetrics(/*SM_SERVERR2*/89) == 0 )
                                        osName = "Windows Server 2003";
                                    else
                                        osName = "Windows Server 2003 R2";
                                }
                            }break;
                    }break;
            case 6:switch( osVersionInfoEx.dwMinorVersion ){
                        case 0: 
                            if( osVersionInfoEx.wProductType == VER_NT_WORKSTATION )
                                osName = "Windows Vista";
                            else
                                osName = "Windows Server 2008";
                            break;
                        case 1: 
                            if( osVersionInfoEx.wProductType == VER_NT_WORKSTATION )
                                osName = "Windows 7";
                            else
                                osName = "Windows Server 2008 R2";
                            break;
                    }break;
        }
    }

    if(osVersionInfoEx.dwMajorVersion == 4)
    {
        if(osVersionInfoEx.wProductType == VER_NT_WORKSTATION)
            osProductType = "Workstation";
        else if(osVersionInfoEx.wProductType == VER_NT_SERVER)
            osProductType = "Server";
    }
    else if(osVersionInfoEx.dwMajorVersion == 5)
    {
        if(osVersionInfoEx.wProductType == VER_NT_WORKSTATION)
        {
            if((osVersionInfoEx.wSuiteMask & VER_SUITE_PERSONAL) == VER_SUITE_PERSONAL)
                osProductType = "Home Edition"; // Windows XP Home Edition
            else
                osProductType = "Professional"; // Windows XP / Windows 2000 Professional
        }
        else if(osVersionInfoEx.wProductType == VER_NT_SERVER)
        {
            if(osVersionInfoEx.dwMinorVersion == 0) 
            {
                if((osVersionInfoEx.wSuiteMask & VER_SUITE_DATACENTER) == VER_SUITE_DATACENTER)
                    osProductType = "Datacenter Server"; // Windows 2000 Datacenter Server
                else if((osVersionInfoEx.wSuiteMask & VER_SUITE_ENTERPRISE) == VER_SUITE_ENTERPRISE)
                    osProductType = "Advanced Server"; // Windows 2000 Advanced Server
                else
                    osProductType = "Server"; // Windows 2000 Server
            }
        }
        else
        {
            if((osVersionInfoEx.wSuiteMask & VER_SUITE_DATACENTER) == VER_SUITE_DATACENTER)
                osProductType = "Datacenter Edition"; // Windows Server 2003 Datacenter Edition
            else if((osVersionInfoEx.wSuiteMask & VER_SUITE_ENTERPRISE) == VER_SUITE_ENTERPRISE)
                osProductType = "Enterprise Edition"; // Windows Server 2003 Enterprise Edition
            else if((osVersionInfoEx.wSuiteMask & VER_SUITE_BLADE) == VER_SUITE_BLADE)
                osProductType = "Web Edition"; // Windows Server 2003 Web Edition
            else
                osProductType = "Standard Edition"; // Windows Server 2003 Standard Edition
        }
    }

    memset( &systemInfo, 0, sizeof(SYSTEM_INFO) );
    GetNativeSystemInfo( &systemInfo );

    if( systemInfo.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_INTEL )
        processorArchitecture = "x86";
    else if( systemInfo.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_AMD64 )
        processorArchitecture = "x64";
    else if( systemInfo.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_IA64 )
        processorArchitecture = "Itanium";


    fprintf( fp, "OS name and edition: %s %s\n", osName, osProductType );
    fprintf( fp, "OS version: %d.%d.%d %S\n", 
                osVersionInfoEx.dwMajorVersion, osVersionInfoEx.dwMinorVersion, 
                osVersionInfoEx.dwBuildNumber, osVersionInfoEx.szCSDVersion );
    fprintf( fp, "Processor architecture: %s\n", processorArchitecture );
    fprintf( fp, "WoW64 process: %s\n", IsWow64() ? "Yes" : "No" );
}

static void printTimeAndDate( FILE *fp )
{
    struct tm *local;
    time_t t;

    t = time(NULL);
    local = localtime(&t);
    fprintf(fp, "Local time and date: %s", asctime(local));
    local = gmtime(&t);
    fprintf(fp, "UTC time and date: %s", asctime(local));
}

/*******************************************************************/

typedef struct
{
    float sine[TABLE_SIZE];
	double phase;
    double phaseIncrement;
    volatile int fadeIn;
    volatile int fadeOut;
    double amp;
}
paTestData;

static paTestData data;

/* This routine will be called by the PortAudio engine when audio is needed.
** It may called at interrupt level on some machines so don't do anything
** that could mess up the system like calling malloc() or free().
*/
static int patestCallback( const void *inputBuffer, void *outputBuffer,
                            unsigned long framesPerBuffer,
                            const PaStreamCallbackTimeInfo* timeInfo,
                            PaStreamCallbackFlags statusFlags,
                            void *userData )
{
    paTestData *data = (paTestData*)userData;
    float *out = (float*)outputBuffer;
    unsigned long i,j;

    (void) timeInfo; /* Prevent unused variable warnings. */
    (void) statusFlags;
    (void) inputBuffer;
    
    for( i=0; i<framesPerBuffer; i++ )
    {
        float x = data->sine[(int)data->phase];
        data->phase += data->phaseIncrement;
        if( data->phase >= TABLE_SIZE ){
			data->phase -= TABLE_SIZE;
		}

        x *= data->amp;
        if( data->fadeIn ){
            data->amp += .001;
            if( data->amp >= 1. )
                data->fadeIn = 0;
        }else if( data->fadeOut ){
            if( data->amp > 0 )
                data->amp -= .001;
        }

		for( j = 0; j < CHANNEL_COUNT; ++j ){
            *out++ = x;
		}
	}
    
    if( data->amp > 0 )
        return paContinue;
    else
        return paComplete;
}


#define YES     1
#define NO      0


static int playUntilKeyPress( int deviceIndex, float sampleRate, 
                             int framesPerUserBuffer, int framesPerWmmeBuffer, int wmmeBufferCount )
{
    PaStreamParameters outputParameters;
    PaWinMmeStreamInfo wmmeStreamInfo;
    PaStream *stream;
    PaError err;
    int c;

    outputParameters.device = deviceIndex;
    outputParameters.channelCount = CHANNEL_COUNT;
    outputParameters.sampleFormat = paFloat32; /* 32 bit floating point processing */
    outputParameters.suggestedLatency = 0; /*Pa_GetDeviceInfo( outputParameters.device )->defaultLowOutputLatency;*/
    outputParameters.hostApiSpecificStreamInfo = NULL;

    wmmeStreamInfo.size = sizeof(PaWinMmeStreamInfo);
    wmmeStreamInfo.hostApiType = paMME; 
    wmmeStreamInfo.version = 1;
    wmmeStreamInfo.flags = paWinMmeUseLowLevelLatencyParameters | paWinMmeDontThrottleOverloadedProcessingThread;
    wmmeStreamInfo.framesPerBuffer = framesPerWmmeBuffer;
    wmmeStreamInfo.bufferCount = wmmeBufferCount;
    outputParameters.hostApiSpecificStreamInfo = &wmmeStreamInfo;

    err = Pa_OpenStream(
              &stream,
              NULL, /* no input */
              &outputParameters,
              sampleRate,
              framesPerUserBuffer,
              paClipOff | paPrimeOutputBuffersUsingStreamCallback,      /* we won't output out of range samples so don't bother clipping them */
              patestCallback,
              &data );
    if( err != paNoError ) goto error;

    data.amp = 0;
    data.fadeIn = 1;
    data.fadeOut = 0;
    data.phase = 0;
    data.phaseIncrement = 15 + ((rand()%100) / 10); // randomise pitch

    err = Pa_StartStream( stream );
    if( err != paNoError ) goto error;


    do{
        printf( "Trying buffer size %d.\nIf it sounds smooth (without clicks or glitches) press 'y', if it sounds bad press 'n' ('q' to quit)\n", framesPerWmmeBuffer );
        c = tolower(_getch());
        if( c == 'q' ){
            Pa_Terminate();
            exit(0);
        }
    }while( c != 'y' && c != 'n' );

    data.fadeOut = 1;
    while( Pa_IsStreamActive(stream) == 1 )
        Pa_Sleep( 100 );

    err = Pa_StopStream( stream );
    if( err != paNoError ) goto error;

    err = Pa_CloseStream( stream );
    if( err != paNoError ) goto error;

    return (c == 'y') ? YES : NO;

error:
    return err;
}

/*******************************************************************/
static void usage( int wmmeHostApiIndex )
{
    int i;

    fprintf( stderr, "PortAudio WMME output latency user guided test\n" );
    fprintf( stderr, "Usage: x.exe mme-device-index [sampleRate [min-buffer-count max-buffer-count]]\n" );
    fprintf( stderr, "Invalid device index. Use one of these:\n" );
    for( i=0; i < Pa_GetDeviceCount(); ++i ){

        if( Pa_GetDeviceInfo(i)->hostApi == wmmeHostApiIndex && Pa_GetDeviceInfo(i)->maxOutputChannels > 0  )
            fprintf( stderr, "%d (%s)\n", i, Pa_GetDeviceInfo(i)->name );
    }
    Pa_Terminate();
    exit(-1);
}

/*
    ideas: 
        o- could be testing with 80% CPU load
        o- could test with different channel counts
*/

int main(int argc, char* argv[])
{
    PaError err;
    int i;
    int deviceIndex;
    int wmmeBufferCount, wmmeBufferSize, smallestWorkingBufferSize;
    int smallestWorkingBufferingLatencyFrames;
    int min, max, mid;
    int testResult;
    FILE *resultsFp;
    int wmmeHostApiIndex;
    const PaHostApiInfo *wmmeHostApiInfo;
    double sampleRate = DEFAULT_SAMPLE_RATE;
    int wmmeMinBufferCount = MIN_WMME_BUFFER_COUNT;
    int wmmeMaxBufferCount = MAX_WMME_BUFFER_COUNT;

    err = Pa_Initialize();
    if( err != paNoError ) goto error;

    wmmeHostApiIndex = Pa_HostApiTypeIdToHostApiIndex( paMME );
    wmmeHostApiInfo = Pa_GetHostApiInfo( wmmeHostApiIndex );

    if( argc > 5 )
        usage(wmmeHostApiIndex);

	deviceIndex = wmmeHostApiInfo->defaultOutputDevice;
	if( argc >= 2 ){
        deviceIndex = -1;
		if( sscanf( argv[1], "%d", &deviceIndex ) != 1 )
            usage(wmmeHostApiIndex);
        if( deviceIndex < 0 || deviceIndex >= Pa_GetDeviceCount() || Pa_GetDeviceInfo(deviceIndex)->hostApi != wmmeHostApiIndex ){
            usage(wmmeHostApiIndex);
        }
	}

    printf( "Using device id %d (%s)\n", deviceIndex, Pa_GetDeviceInfo(deviceIndex)->name );

    if( argc >= 3 ){
        if( sscanf( argv[2], "%lf", &sampleRate ) != 1 )
            usage(wmmeHostApiIndex);
    }

    printf( "Testing with sample rate %f.\n", (float)sampleRate );

    if( argc == 4 ){
        if( sscanf( argv[3], "%d", &wmmeMinBufferCount ) != 1 )
            usage(wmmeHostApiIndex);
        wmmeMaxBufferCount = wmmeMinBufferCount;
    }

    if( argc == 5 ){
        if( sscanf( argv[3], "%d", &wmmeMinBufferCount ) != 1 )
            usage(wmmeHostApiIndex);
        if( sscanf( argv[4], "%d", &wmmeMaxBufferCount ) != 1 )
            usage(wmmeHostApiIndex);
    }

    printf( "Testing buffer counts from %d to %d\n", wmmeMinBufferCount, wmmeMaxBufferCount );


    /* initialise sinusoidal wavetable */
    for( i=0; i<TABLE_SIZE; i++ )
    {
        data.sine[i] = (float) sin( ((double)i/(double)TABLE_SIZE) * M_PI * 2. );
    }

	data.phase = 0;

    resultsFp = fopen( "results.txt", "at" );
    fprintf( resultsFp, "*** WMME smallest working output buffer sizes\n" );

    printTimeAndDate( resultsFp );
    printWindowsVersionInfo( resultsFp );
    
    fprintf( resultsFp, "audio device: %s\n", Pa_GetDeviceInfo( deviceIndex )->name );
    fflush( resultsFp );

    fprintf( resultsFp, "Sample rate: %f\n", (float)sampleRate );
    fprintf( resultsFp, "Buffer count, Smallest working buffer size (frames), Smallest working buffering latency (frames), Smallest working buffering latency (Seconds)\n" );

    for( wmmeBufferCount = wmmeMinBufferCount; wmmeBufferCount <= wmmeMaxBufferCount; ++wmmeBufferCount ){
 
        printf( "Test %d of %d\n", (wmmeBufferCount - wmmeMinBufferCount) + 1, (wmmeMaxBufferCount-wmmeMinBufferCount) + 1 );
        printf( "Testing with %d buffers...\n", wmmeBufferCount );

        /*
            Binary search after Niklaus Wirth
            from http://en.wikipedia.org/wiki/Binary_search_algorithm#The_algorithm
         */
        min = 1;
        max = (int)((sampleRate * .3) / (wmmeBufferCount-1)); //8192;    /* we assume that this size works 300ms */
        smallestWorkingBufferSize = 0;

        do{
            mid = min + ((max - min) / 2);

            wmmeBufferSize = mid;
            testResult = playUntilKeyPress( deviceIndex, sampleRate, wmmeBufferSize, wmmeBufferSize, wmmeBufferCount );

            if( testResult == YES ){
                max = mid - 1;
                smallestWorkingBufferSize = wmmeBufferSize;
            }else{
                min = mid + 1;
            }
             
        }while( (min <= max) && (testResult == YES || testResult == NO) );

        smallestWorkingBufferingLatencyFrames = smallestWorkingBufferSize * (wmmeBufferCount - 1);

        printf( "Smallest working buffer size for %d buffers is: %d\n", wmmeBufferCount, smallestWorkingBufferSize );
        printf( "Corresponding to buffering latency of %d frames, or %f seconds.\n", smallestWorkingBufferingLatencyFrames, smallestWorkingBufferingLatencyFrames / sampleRate );

        fprintf( resultsFp, "%d, %d, %d, %f\n", wmmeBufferCount, smallestWorkingBufferSize, smallestWorkingBufferingLatencyFrames, smallestWorkingBufferingLatencyFrames / sampleRate );
        fflush( resultsFp );
    }

    fprintf( resultsFp, "###\n" );
    fclose( resultsFp );
    
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

