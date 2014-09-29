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

#include "com_portaudio_BlockingStream.h"
#include "portaudio.h"
#include "jpa_tools.h"

#ifndef FALSE
#define FALSE  (0)
#endif
#ifndef TRUE
#define TRUE  (!FALSE)
#endif

/*
 * Class:     com_portaudio_BlockingStream
 * Method:    getReadAvailable
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_com_portaudio_BlockingStream_getReadAvailable
  (JNIEnv *env, jobject blockingStream)
{
	PaStream *stream =jpa_GetStreamPointer( env, blockingStream );
	if( stream == NULL ) return 0;
	return Pa_GetStreamReadAvailable( stream );
}

/*
 * Class:     com_portaudio_BlockingStream
 * Method:    getWriteAvailable
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_com_portaudio_BlockingStream_getWriteAvailable
  (JNIEnv *env, jobject blockingStream)
{
	PaStream *stream =jpa_GetStreamPointer( env, blockingStream );
	if( stream == NULL ) return 0;
	return Pa_GetStreamWriteAvailable( stream );
}


/*
 * Class:     com_portaudio_BlockingStream
 * Method:    writeFloats
 * Signature: ([FI)Z
 */
JNIEXPORT jboolean JNICALL Java_com_portaudio_BlockingStream_writeFloats
  (JNIEnv *env, jobject blockingStream, jfloatArray buffer, jint numFrames)
{
	jfloat *carr;
	jint err;
	PaStream *stream =jpa_GetStreamPointer( env, blockingStream );
	if( buffer == NULL )
	{
		(*env)->ThrowNew( env, (*env)->FindClass(env,"java/lang/RuntimeException"),
                  "null stream buffer");
		return FALSE;
	}
	carr = (*env)->GetFloatArrayElements(env, buffer, NULL);
	if (carr == NULL)
	{
		(*env)->ThrowNew( env, (*env)->FindClass(env,"java/lang/RuntimeException"),
                  "invalid stream buffer");
		return FALSE;
	}
	err = Pa_WriteStream( stream, carr, numFrames );
	(*env)->ReleaseFloatArrayElements(env, buffer, carr, 0);
	if( err == paOutputUnderflowed )
	{
		return TRUE;
	}
	else
	{
		jpa_CheckError( env, err );
		return FALSE;
	}
}

/*
 * Class:     com_portaudio_BlockingStream
 * Method:    readFloats
 * Signature: ([FI)Z
 */
JNIEXPORT jboolean JNICALL Java_com_portaudio_BlockingStream_readFloats
  (JNIEnv *env, jobject blockingStream, jfloatArray buffer, jint numFrames)
{
	jfloat *carr;
	jint err;
	PaStream *stream =jpa_GetStreamPointer( env, blockingStream );
	if( buffer == NULL )
	{
		(*env)->ThrowNew( env, (*env)->FindClass(env,"java/lang/RuntimeException"),
                  "null stream buffer");
		return FALSE;
	}
	carr = (*env)->GetFloatArrayElements(env, buffer, NULL);
	if (carr == NULL)
	{
		(*env)->ThrowNew( env, (*env)->FindClass(env,"java/lang/RuntimeException"),
                  "invalid stream buffer");
		return FALSE;
	}
	err = Pa_ReadStream( stream, carr, numFrames );
	(*env)->ReleaseFloatArrayElements(env, buffer, carr, 0);
	if( err == paInputOverflowed )
	{
		return TRUE;
	}
	else
	{
		jpa_CheckError( env, err );
		return FALSE;
	}
}

/*
 * Class:     com_portaudio_BlockingStream
 * Method:    writeShorts
 * Signature: ([SI)Z
 */
JNIEXPORT jboolean JNICALL Java_com_portaudio_BlockingStream_writeShorts
  (JNIEnv *env, jobject blockingStream, jfloatArray buffer, jint numFrames)
{
	jshort *carr;
	jint err;
	PaStream *stream =jpa_GetStreamPointer( env, blockingStream );
	if( buffer == NULL )
	{
		(*env)->ThrowNew( env, (*env)->FindClass(env,"java/lang/RuntimeException"),
                  "null stream buffer");
		return FALSE;
	}
	carr = (*env)->GetShortArrayElements(env, buffer, NULL);
	if (carr == NULL)
	{
		(*env)->ThrowNew( env, (*env)->FindClass(env,"java/lang/RuntimeException"),
                  "invalid stream buffer");
		return FALSE;
	}
	err = Pa_WriteStream( stream, carr, numFrames );
	(*env)->ReleaseShortArrayElements(env, buffer, carr, 0);
	if( err == paOutputUnderflowed )
	{
		return TRUE;
	}
	else
	{
		jpa_CheckError( env, err );
		return FALSE;
	}
}

/*
 * Class:     com_portaudio_BlockingStream
 * Method:    readShorts
 * Signature: ([SI)Z
 */
JNIEXPORT jboolean JNICALL Java_com_portaudio_BlockingStream_readShorts
  (JNIEnv *env, jobject blockingStream, jfloatArray buffer, jint numFrames)
{
	jshort *carr;
	jint err;
	PaStream *stream =jpa_GetStreamPointer( env, blockingStream );
	if( buffer == NULL )
	{
		(*env)->ThrowNew( env, (*env)->FindClass(env,"java/lang/RuntimeException"),
                  "null stream buffer");
		return FALSE;
	}
	carr = (*env)->GetShortArrayElements(env, buffer, NULL);
	if (carr == NULL)
	{
		(*env)->ThrowNew( env, (*env)->FindClass(env,"java/lang/RuntimeException"),
                  "invalid stream buffer");
		return FALSE;
	}
	err = Pa_ReadStream( stream, carr, numFrames );
	(*env)->ReleaseShortArrayElements(env, buffer, carr, 0);
	if( err == paInputOverflowed )
	{
		return TRUE;
	}
	else
	{
		jpa_CheckError( env, err );
		return FALSE;
	}
}

/*
 * Class:     com_portaudio_BlockingStream
 * Method:    start
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_com_portaudio_BlockingStream_start
  (JNIEnv *env, jobject blockingStream )
{
	PaStream *stream = jpa_GetStreamPointer( env, blockingStream );
	int err = Pa_StartStream( stream );
	jpa_CheckError( env, err );
}

/*
 * Class:     com_portaudio_BlockingStream
 * Method:    stop
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_com_portaudio_BlockingStream_stop
  (JNIEnv *env, jobject blockingStream )
{
	PaStream *stream =jpa_GetStreamPointer( env, blockingStream );
	int err = Pa_StopStream( stream );
	jpa_CheckError( env, err );
}
/*
 * Class:     com_portaudio_BlockingStream
 * Method:    abort
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_com_portaudio_BlockingStream_abort
  (JNIEnv *env, jobject blockingStream )
{
	PaStream *stream =jpa_GetStreamPointer( env, blockingStream );
	int err = Pa_AbortStream( stream );
	jpa_CheckError( env, err );
}

/*
 * Class:     com_portaudio_BlockingStream
 * Method:    close
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_com_portaudio_BlockingStream_close
  (JNIEnv *env, jobject blockingStream )
{
	jclass cls;
	PaStream *stream =jpa_GetStreamPointer( env, blockingStream );
	if( stream != NULL )
	{
		int err = Pa_CloseStream( stream );
		jpa_CheckError( env, err );
		cls = (*env)->GetObjectClass(env, blockingStream);
		jpa_SetLongField( env, cls, blockingStream, "nativeStream", (jlong) 0 );
	}
}

/*
 * Class:     com_portaudio_BlockingStream
 * Method:    isStopped
 * Signature: ()V
 */
JNIEXPORT jboolean JNICALL Java_com_portaudio_BlockingStream_isStopped
  (JNIEnv *env, jobject blockingStream )
{
	int err;
	PaStream *stream =jpa_GetStreamPointer( env, blockingStream );
	if( stream == NULL ) return 1;
	err = Pa_IsStreamStopped( stream );
	return (jpa_CheckError( env, err ) > 0);
}
/*
 * Class:     com_portaudio_BlockingStream
 * Method:    isActive
 * Signature: ()V
 */
JNIEXPORT jboolean JNICALL Java_com_portaudio_BlockingStream_isActive
  (JNIEnv *env, jobject blockingStream )
{
	int err;
	PaStream *stream =jpa_GetStreamPointer( env, blockingStream );
	if( stream == NULL ) return 0;
	err = Pa_IsStreamActive( stream );
	return (jpa_CheckError( env, err ) > 0);
}


/*
 * Class:     com_portaudio_BlockingStream
 * Method:    getTime
 * Signature: ()D
 */
JNIEXPORT jdouble JNICALL Java_com_portaudio_BlockingStream_getTime
  (JNIEnv *env, jobject blockingStream )
{
	PaStream *stream =jpa_GetStreamPointer( env, blockingStream );
	if( stream == NULL ) return 0.0;
	return Pa_GetStreamTime( stream );
}


/*
 * Class:     com_portaudio_BlockingStream
 * Method:    getInfo
 * Signature: ()Lcom/portaudio/StreamInfo;
 */
JNIEXPORT void JNICALL Java_com_portaudio_BlockingStream_getInfo
  (JNIEnv *env, jobject blockingStream, jobject streamInfo)
{
	
	PaStream *stream =jpa_GetStreamPointer( env, blockingStream );
	const PaStreamInfo *info = Pa_GetStreamInfo( stream );
	if( streamInfo == NULL )
	{
		jpa_ThrowError( env, "Invalid stream." );
	}
	else
	{
		/* Get a reference to obj's class */
		jclass cls = (*env)->GetObjectClass(env, streamInfo);
 
		jpa_SetIntField( env, cls, streamInfo, "structVersion", info->structVersion );
		jpa_SetDoubleField( env, cls, streamInfo, "inputLatency", info->inputLatency );
		jpa_SetDoubleField( env, cls, streamInfo, "outputLatency", info->outputLatency );
		jpa_SetDoubleField( env, cls, streamInfo, "sampleRate", info->sampleRate );
	}
}

