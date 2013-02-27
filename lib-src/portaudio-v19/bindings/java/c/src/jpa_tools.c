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

#include "com_portaudio_PortAudio.h"
#include "portaudio.h"
#include "jpa_tools.h"

jint jpa_GetIntField( JNIEnv *env, jclass cls, jobject obj, const char *fieldName )
{
     /* Look for the instance field maxInputChannels in cls */
     jfieldID fid = (*env)->GetFieldID(env, cls, fieldName, "I");
     if (fid == NULL)
	 {
		 jpa_ThrowError( env, "Cannot find integer JNI field." );
		return 0;
     }
	 else
	 {
		return (*env)->GetIntField(env, obj, fid );
	 }
}

void jpa_SetIntField( JNIEnv *env, jclass cls, jobject obj, const char *fieldName, jint value )
{
     /* Look for the instance field maxInputChannels in cls */
     jfieldID fid = (*env)->GetFieldID(env, cls, fieldName, "I");
     if (fid == NULL)
	 {
		 jpa_ThrowError( env, "Cannot find integer JNI field." );
     }
	 else
	 {
		(*env)->SetIntField(env, obj, fid, value );
	 }
}

jlong jpa_GetLongField( JNIEnv *env, jclass cls, jobject obj, const char *fieldName )
{
     /* Look for the instance field maxInputChannels in cls */
     jfieldID fid = (*env)->GetFieldID(env, cls, fieldName, "J");
     if (fid == NULL)
	 {
		 jpa_ThrowError( env, "Cannot find long JNI field." );
		return 0L;
     }
	 else
	 {
		return (*env)->GetLongField(env, obj, fid );
	 }
}

void jpa_SetLongField( JNIEnv *env, jclass cls, jobject obj, const char *fieldName, jlong value )
{
     /* Look for the instance field maxInputChannels in cls */
     jfieldID fid = (*env)->GetFieldID(env, cls, fieldName, "J");
     if (fid == NULL)
	 {
		 jpa_ThrowError( env, "Cannot find long JNI field." );
     }
	 else
	 {
		(*env)->SetLongField(env, obj, fid, value );
	 }
}


void jpa_SetDoubleField( JNIEnv *env, jclass cls, jobject obj, const char *fieldName, jdouble value )
{
     /* Look for the instance field maxInputChannels in cls */
     jfieldID fid = (*env)->GetFieldID(env, cls, fieldName, "D");
     if (fid == NULL)
	 {
		 jpa_ThrowError( env, "Cannot find double JNI field." );
     }
	 else
	 {
		(*env)->SetDoubleField(env, obj, fid, value );
	 }
}


jdouble jpa_GetDoubleField( JNIEnv *env, jclass cls, jobject obj, const char *fieldName )
{
     /* Look for the instance field maxInputChannels in cls */
     jfieldID fid = (*env)->GetFieldID(env, cls, fieldName, "D");
     if (fid == NULL)
	 {
		 jpa_ThrowError( env, "Cannot find double JNI field." );
		return 0;
     }
	 else
	 {
		return (*env)->GetDoubleField(env, obj, fid );
	 }
}

void jpa_SetStringField( JNIEnv *env, jclass cls, jobject obj, const char *fieldName, const char *value )
{
     /* Look for the instance field maxInputChannels in cls */
     jfieldID fid = (*env)->GetFieldID(env, cls, fieldName, "Ljava/lang/String;");
     if (fid == NULL)
	 {
		 jpa_ThrowError( env, "Cannot find String JNI field." );
     }
	 else
	 {
		jstring jstr = (*env)->NewStringUTF(env, value);
		if (jstr == NULL)
		{
			jpa_ThrowError( env, "Cannot create new String." );
		}
		else
		{
			(*env)->SetObjectField(env, obj, fid, jstr );
		}
	 }
}

PaStreamParameters *jpa_FillStreamParameters( JNIEnv *env, jobject jstreamParam, PaStreamParameters *myParams )
{
	jclass cls;
	
	if( jstreamParam == NULL ) return NULL; // OK, not an error

	cls = (*env)->GetObjectClass(env, jstreamParam);
	
	myParams->channelCount = jpa_GetIntField( env, cls, jstreamParam, "channelCount" );
	myParams->device = jpa_GetIntField( env, cls, jstreamParam, "device" );
	myParams->sampleFormat = jpa_GetIntField( env, cls, jstreamParam, "sampleFormat" );
	myParams->suggestedLatency = jpa_GetDoubleField( env, cls, jstreamParam, "suggestedLatency" );
	myParams->hostApiSpecificStreamInfo = NULL;

	return myParams;
}

// Create an exception that will be thrown when we return from the JNI call.
jint jpa_ThrowError( JNIEnv *env, const char *message )
{
	return (*env)->ThrowNew(env, (*env)->FindClass( env, "java/lang/RuntimeException"),
                  message );
}

// Throw an exception on error.
jint jpa_CheckError( JNIEnv *env, PaError err )
{
	if( err == -1 )
	{
        return jpa_ThrowError( env, "-1, possibly no available default device" );
    }
    else if( err < 0 )
    {
		if( err == paUnanticipatedHostError )
		{
			const PaHostErrorInfo *hostErrorInfo = Pa_GetLastHostErrorInfo();
			return jpa_ThrowError( env, hostErrorInfo->errorText );
		}
		else
		{
			return jpa_ThrowError( env, Pa_GetErrorText( err ) );
		}
	}
	else
	{
		return err;
	}
}

// Get the stream pointer from a BlockingStream long field.
PaStream *jpa_GetStreamPointer( JNIEnv *env, jobject blockingStream )
{
	jclass cls = (*env)->GetObjectClass(env, blockingStream);
	return (PaStream *) jpa_GetLongField( env, cls, blockingStream, "nativeStream" );
}
