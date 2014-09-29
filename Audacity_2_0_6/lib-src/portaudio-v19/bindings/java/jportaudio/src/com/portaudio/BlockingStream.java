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

 @brief A blocking read/write stream.
*/
package com.portaudio;

/**
 * Represents a stream for blocking read/write I/O.
 * 
 * This Java object contains the pointer to a PortAudio stream stored as a long.
 * It is passed to PortAudio when calling stream related functions.
 * 
 * To create one of these, call PortAudio.openStream().
 * 
 * @see PortAudio
 * 
 * @author Phil Burk
 * 
 */
public class BlockingStream
{
	// nativeStream is only accessed by the native code. It contains a pointer
	// to a PaStream.
	private long nativeStream;
	private int inputFormat = -1;
	private int outputFormat = -1;

	protected BlockingStream()
	{
	}

	/**
	 * @return number of frames that can be read without blocking.
	 */
	public native int getReadAvailable();

	/**
	 * @return number of frames that can be written without blocking.
	 */
	public native int getWriteAvailable();

	private native boolean readFloats( float[] buffer, int numFrames );

	private native boolean writeFloats( float[] buffer, int numFrames );

	/**
	 * Read 32-bit floating point data from the stream into the array.
	 * 
	 * @param buffer
	 * @param numFrames
	 *            number of frames to read
	 * @return true if an input overflow occurred
	 */
	public boolean read( float[] buffer, int numFrames )
	{
		if( inputFormat != PortAudio.FORMAT_FLOAT_32 )
		{
			throw new RuntimeException(
					"Tried to read float samples from a non float stream." );
		}
		return readFloats( buffer, numFrames );
	}

	/**
	 * Write 32-bit floating point data to the stream from the array. The data
	 * should be in the range -1.0 to +1.0.
	 * 
	 * @param buffer
	 * @param numFrames
	 *            number of frames to write
	 * @return true if an output underflow occurred
	 */
	public boolean write( float[] buffer, int numFrames )
	{
		if( outputFormat != PortAudio.FORMAT_FLOAT_32 )
		{
			throw new RuntimeException(
					"Tried to write float samples to a non float stream." );
		}
		return writeFloats( buffer, numFrames );
	}

	private native boolean readShorts( short[] buffer, int numFrames );

	private native boolean writeShorts( short[] buffer, int numFrames );

	/**
	 * Read 16-bit integer data to the stream from the array.
	 * 
	 * @param buffer
	 * @param numFrames
	 *            number of frames to write
	 * @return true if an input overflow occurred
	 */
	public boolean read( short[] buffer, int numFrames )
	{
		if( inputFormat != PortAudio.FORMAT_INT_16 )
		{
			throw new RuntimeException(
					"Tried to read short samples from a non short stream." );
		}
		return readShorts( buffer, numFrames );
	}

	/**
	 * Write 16-bit integer data to the stream from the array.
	 * 
	 * @param buffer
	 * @param numFrames
	 *            number of frames to write
	 * @return true if an output underflow occurred
	 */
	public boolean write( short[] buffer, int numFrames )
	{
		if( outputFormat != PortAudio.FORMAT_INT_16 )
		{
			throw new RuntimeException(
					"Tried to write short samples from a non short stream." );
		}
		return writeShorts( buffer, numFrames );
	}

	/**
	 * Atart audio I/O.
	 */
	public native void start();

	/**
	 * Wait for the stream to play all of the data that has been written then
	 * stop.
	 */
	public native void stop();

	/**
	 * Stop immediately and lose any data that was written but not played.
	 */
	public native void abort();

	/**
	 * Close the stream and zero out the pointer. Do not reference the stream
	 * after this.
	 */
	public native void close();

	public native boolean isStopped();

	public native boolean isActive();

	public String toString()
	{
		return "BlockingStream: streamPtr = " + Long.toHexString( nativeStream )
				+ ", inFormat = " + inputFormat + ", outFormat = "
				+ outputFormat;
	}

	/**
	 * Get audio time related to this stream. Note that it may not start at 0.0.
	 */
	public native double getTime();

	private native void getInfo( StreamInfo streamInfo );

	public StreamInfo getInfo()
	{
		StreamInfo streamInfo = new StreamInfo();
		getInfo( streamInfo );
		return streamInfo;
	}
}
