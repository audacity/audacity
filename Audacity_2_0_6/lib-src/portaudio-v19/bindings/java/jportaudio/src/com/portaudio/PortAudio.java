/*
 * Portable Audio I/O Library
 * Java Binding for PortAudio
 *
 * Based on the Open Source API proposed by Ross Bencina
 * Copyright (c) 2008 Ross Bencina
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

/** @file
 @ingroup bindings_java

 @brief Java wrapper for the PortAudio API.
*/
package com.portaudio;

/**
 * Java methods that call PortAudio via JNI. This is a portable audio I/O
 * library that can be used as an alternative to JavaSound.
 * 
 * Please see the PortAudio documentation for a full explanation.
 * 
 * http://portaudio.com/docs/
 * http://portaudio.com/docs/v19-doxydocs/portaudio_8h.html
 * 
 * This Java binding does not support audio callbacks because an audio callback
 * should never block. Calling into a Java virtual machine might block for
 * garbage collection or synchronization. So only the blocking read/write mode
 * is supported.
 * 
 * @see BlockingStream
 * @see DeviceInfo
 * @see HostApiInfo
 * @see StreamInfo
 * @see StreamParameters
 * 
 * @author Phil Burk
 * 
 */
public class PortAudio
{
	public final static int FLAG_CLIP_OFF = (1 << 0);
	public final static int FLAG_DITHER_OFF = (1 << 1);

	/** Sample Formats */
	public final static int FORMAT_FLOAT_32 = (1 << 0);
	public final static int FORMAT_INT_32 = (1 << 1); // not supported
	public final static int FORMAT_INT_24 = (1 << 2); // not supported
	public final static int FORMAT_INT_16 = (1 << 3);
	public final static int FORMAT_INT_8 = (1 << 4); // not supported
	public final static int FORMAT_UINT_8 = (1 << 5); // not supported

	/** These HOST_API_TYPES will not change in the future. */
	public final static int HOST_API_TYPE_DEV = 0;
	public final static int HOST_API_TYPE_DIRECTSOUND = 1;
	public final static int HOST_API_TYPE_MME = 2;
	public final static int HOST_API_TYPE_ASIO = 3;
	/** Apple Sound Manager. Obsolete. */
	public final static int HOST_API_TYPE_SOUNDMANAGER = 4;
	public final static int HOST_API_TYPE_COREAUDIO = 5;
	public final static int HOST_API_TYPE_OSS = 7;
	public final static int HOST_API_TYPE_ALSA = 8;
	public final static int HOST_API_TYPE_AL = 9;
	public final static int HOST_API_TYPE_BEOS = 10;
	public final static int HOST_API_TYPE_WDMKS = 11;
	public final static int HOST_API_TYPE_JACK = 12;
	public final static int HOST_API_TYPE_WASAPI = 13;
	public final static int HOST_API_TYPE_AUDIOSCIENCE = 14;
	public final static int HOST_API_TYPE_COUNT = 15;

	static
	{
		String os = System.getProperty( "os.name" ).toLowerCase();
		// On Windows we have separate libraries for 32 and 64-bit JVMs.
		if( os.indexOf( "win" ) >= 0 )
		{
			if( System.getProperty( "os.arch" ).contains( "64" ) )
			{
				System.loadLibrary( "jportaudio_x64" );
			}
			else
			{
				System.loadLibrary( "jportaudio_x86" );
			}
		}
		else
		{
			System.loadLibrary( "jportaudio" );
		}
		System.out.println( "---- JPortAudio version " + getVersion() + ", "
				+ getVersionText() );
	}

	/**
	 * @return the release number of the currently running PortAudio build, eg
	 *         1900.
	 */
	public native static int getVersion();

	/**
	 * @return a textual description of the current PortAudio build, eg
	 *         "PortAudio V19-devel 13 October 2002".
	 */
	public native static String getVersionText();

	/**
	 * Library initialization function - call this before using PortAudio. This
	 * function initializes internal data structures and prepares underlying
	 * host APIs for use. With the exception of getVersion(), getVersionText(),
	 * and getErrorText(), this function MUST be called before using any other
	 * PortAudio API functions.
	 */
	public native static void initialize();

	/**
	 * Library termination function - call this when finished using PortAudio.
	 * This function deallocates all resources allocated by PortAudio since it
	 * was initialized by a call to initialize(). In cases where Pa_Initialise()
	 * has been called multiple times, each call must be matched with a
	 * corresponding call to terminate(). The final matching call to terminate()
	 * will automatically close any PortAudio streams that are still open.
	 */
	public native static void terminate();

	/**
	 * @return the number of available devices. The number of available devices
	 *         may be zero.
	 */
	public native static int getDeviceCount();

	private native static void getDeviceInfo( int index, DeviceInfo deviceInfo );

	/**
	 * @param index
	 *            A valid device index in the range 0 to (getDeviceCount()-1)
	 * @return An DeviceInfo structure.
	 * @throws RuntimeException
	 *             if the device parameter is out of range.
	 */
	public static DeviceInfo getDeviceInfo( int index )
	{
		DeviceInfo deviceInfo = new DeviceInfo();
		getDeviceInfo( index, deviceInfo );
		return deviceInfo;
	}

	/**
	 * @return the number of available host APIs.
	 */
	public native static int getHostApiCount();

	private native static void getHostApiInfo( int index,
			HostApiInfo hostApiInfo );

	/**
	 * @param index
	 * @return information about the Host API
	 */
	public static HostApiInfo getHostApiInfo( int index )
	{
		HostApiInfo hostApiInfo = new HostApiInfo();
		getHostApiInfo( index, hostApiInfo );
		return hostApiInfo;
	}

	/**
	 * @param hostApiType
	 *            A unique host API identifier, for example
	 *            HOST_API_TYPE_COREAUDIO.
	 * @return a runtime host API index
	 */
	public native static int hostApiTypeIdToHostApiIndex( int hostApiType );

	/**
	 * @param hostApiIndex
	 *            A valid host API index ranging from 0 to (getHostApiCount()-1)
	 * @param apiDeviceIndex
	 *            A valid per-host device index in the range 0 to
	 *            (getHostApiInfo(hostApi).deviceCount-1)
	 * @return standard PortAudio device index
	 */
	public native static int hostApiDeviceIndexToDeviceIndex( int hostApiIndex,
			int apiDeviceIndex );

	public native static int getDefaultInputDevice();

	public native static int getDefaultOutputDevice();

	public native static int getDefaultHostApi();

	/**
	 * @param inputStreamParameters
	 *            input description, may be null
	 * @param outputStreamParameters
	 *            output description, may be null
	 * @param sampleRate
	 *            typically 44100 or 48000, or maybe 22050, 16000, 8000, 96000
	 * @return 0 if supported or a negative error
	 */
	public native static int isFormatSupported(
			StreamParameters inputStreamParameters,
			StreamParameters outputStreamParameters, int sampleRate );

	private native static void openStream( BlockingStream blockingStream,
			StreamParameters inputStreamParameters,
			StreamParameters outputStreamParameters, int sampleRate,
			int framesPerBuffer, int flags );

	/**
	 * 
	 * @param inputStreamParameters
	 *            input description, may be null
	 * @param outputStreamParameters
	 *            output description, may be null
	 * @param sampleRate
	 *            typically 44100 or 48000, or maybe 22050, 16000, 8000, 96000
	 * @param framesPerBuffer
	 * @param flags
	 * @return
	 */
	public static BlockingStream openStream(
			StreamParameters inputStreamParameters,
			StreamParameters outputStreamParameters, int sampleRate,
			int framesPerBuffer, int flags )
	{
		BlockingStream blockingStream = new BlockingStream();
		openStream( blockingStream, inputStreamParameters,
				outputStreamParameters, sampleRate, framesPerBuffer, flags );
		return blockingStream;
	}

}
