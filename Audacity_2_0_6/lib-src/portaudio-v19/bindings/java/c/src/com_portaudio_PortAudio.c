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

/*
 * Class:     com_portaudio_PortAudio
 * Method:    getVersion
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_com_portaudio_PortAudio_getVersion
  (JNIEnv *env, jclass clazz)
{
	return Pa_GetVersion();
}

/*
 * Class:     com_portaudio_PortAudio
 * Method:    getVersionText
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_portaudio_PortAudio_getVersionText
  (JNIEnv *env, jclass clazz)
{
	return (*env)->NewStringUTF(env, Pa_GetVersionText() );
}

/*
 * Class:     com_portaudio_PortAudio
 * Method:    initialize
 * Signature: ()I
 */
JNIEXPORT void JNICALL Java_com_portaudio_PortAudio_initialize
  (JNIEnv *env, jclass clazz)
{
	PaError err = Pa_Initialize();
	jpa_CheckError( env, err );
}

/*
 * Class:     com_portaudio_PortAudio
 * Method:    terminate
 * Signature: ()I
 */
JNIEXPORT void JNICALL Java_com_portaudio_PortAudio_terminate
  (JNIEnv *env, jclass clazz)
{
	PaError err = Pa_Terminate();
	jpa_CheckError( env, err );
}

/*
 * Class:     com_portaudio_PortAudio
 * Method:    getDeviceCount
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_com_portaudio_PortAudio_getDeviceCount
  (JNIEnv *env, jclass clazz)
{
	jint count = Pa_GetDeviceCount();
	return jpa_CheckError( env, count );
}

/*
 * Class:     com_portaudio_PortAudio
 * Method:    getDeviceInfo
 * Signature: (ILcom/portaudio/DeviceInfo;)I
 */
JNIEXPORT void JNICALL Java_com_portaudio_PortAudio_getDeviceInfo
  (JNIEnv *env, jclass clazz, jint index, jobject deviceInfo)
{
	const PaDeviceInfo *info;
	     /* Get a reference to obj's class */
	jclass cls = (*env)->GetObjectClass(env, deviceInfo);
 
	info = Pa_GetDeviceInfo( index );
	if( info == NULL )
	{
		jpa_ThrowError( env, "Pa_GetDeviceInfo returned NULL." );
	}
	else
	{
		jpa_SetStringField( env, cls, deviceInfo, "name", info->name );
		jpa_SetIntField( env, cls, deviceInfo, "maxInputChannels", info->maxInputChannels );
		jpa_SetIntField( env, cls, deviceInfo, "maxOutputChannels", info->maxOutputChannels );
		jpa_SetIntField( env, cls, deviceInfo, "hostApi", info->hostApi );
		jpa_SetDoubleField( env, cls, deviceInfo, "defaultSampleRate", info->defaultSampleRate );
		jpa_SetDoubleField( env, cls, deviceInfo, "defaultLowInputLatency", info->defaultLowInputLatency );
		jpa_SetDoubleField( env, cls, deviceInfo, "defaultLowInputLatency", info->defaultHighInputLatency );
		jpa_SetDoubleField( env, cls, deviceInfo, "defaultLowOutputLatency", info->defaultLowOutputLatency );
		jpa_SetDoubleField( env, cls, deviceInfo, "defaultHighOutputLatency", info->defaultHighOutputLatency );
	}
}

/*
 * Class:     com_portaudio_PortAudio
 * Method:    geHostApiCount
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_com_portaudio_PortAudio_getHostApiCount
  (JNIEnv *env, jclass clazz)
{
	jint count = Pa_GetHostApiCount();
	return jpa_CheckError( env, count );
}


/*
 * Class:     com_portaudio_PortAudio
 * Method:    hostApiTypeIdToHostApiIndex
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_com_portaudio_PortAudio_hostApiTypeIdToHostApiIndex
  (JNIEnv *env, jclass clazz, jint hostApiType)
{
	return Pa_HostApiTypeIdToHostApiIndex( (PaHostApiTypeId) hostApiType );
}

/*
 * Class:     com_portaudio_PortAudio
 * Method:    hostApiDeviceIndexToDeviceIndex
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL Java_com_portaudio_PortAudio_hostApiDeviceIndexToDeviceIndex
  (JNIEnv *env, jclass clazz, jint hostApiIndex, jint apiDeviceIndex)
{
	return Pa_HostApiDeviceIndexToDeviceIndex( hostApiIndex, apiDeviceIndex );
}


/*
 * Class:     com_portaudio_PortAudio
 * Method:    getHostApiInfo
 * Signature: (ILcom/portaudio/HostApiInfo;)I
 */
JNIEXPORT void JNICALL Java_com_portaudio_PortAudio_getHostApiInfo
  (JNIEnv *env, jclass clazz, jint index, jobject hostApiInfo)
{
	const PaHostApiInfo *info;
	     /* Get a reference to obj's class */
	jclass cls = (*env)->GetObjectClass(env, hostApiInfo);
 
	info = Pa_GetHostApiInfo( index );
	if( info == NULL )
	{
		jpa_ThrowError( env, "Pa_GetHostApiInfo returned NULL." );
	}
	else
	{
		jpa_SetIntField( env, cls, hostApiInfo, "version", info->structVersion );
		jpa_SetIntField( env, cls, hostApiInfo, "type", info->type );
		jpa_SetStringField( env, cls, hostApiInfo, "name", info->name );
		jpa_SetIntField( env, cls, hostApiInfo, "deviceCount", info->deviceCount );
		jpa_SetIntField( env, cls, hostApiInfo, "defaultInputDevice", info->defaultInputDevice );
		jpa_SetIntField( env, cls, hostApiInfo, "defaultOutputDevice", info->defaultOutputDevice );
	}
}

/*
 * Class:     com_portaudio_PortAudio
 * Method:    getDefaultInputDevice
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_com_portaudio_PortAudio_getDefaultInputDevice
  (JNIEnv *env, jclass clazz)
{
	jint deviceId = Pa_GetDefaultInputDevice();
	return jpa_CheckError( env, deviceId );
}

/*
 * Class:     com_portaudio_PortAudio
 * Method:    getDefaultOutputDevice
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_com_portaudio_PortAudio_getDefaultOutputDevice
  (JNIEnv *env, jclass clazz)
{
	jint deviceId = Pa_GetDefaultOutputDevice();
	return jpa_CheckError( env, deviceId );
}

/*
 * Class:     com_portaudio_PortAudio
 * Method:    getDefaultHostApi
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_com_portaudio_PortAudio_getDefaultHostApi
  (JNIEnv *env, jclass clazz)
{
	jint deviceId = Pa_GetDefaultHostApi();
	return jpa_CheckError( env, deviceId );
}

/*
 * Class:     com_portaudio_PortAudio
 * Method:    isFormatSupported
 * Signature: (Lcom/portaudio/StreamParameters;Lcom/portaudio/StreamParameters;I)I
 */
JNIEXPORT jint JNICALL Java_com_portaudio_PortAudio_isFormatSupported
  (JNIEnv *env, jclass clazz, jobject inParams, jobject outParams, jint sampleRate )
{
	PaStreamParameters myInParams, *paInParams;
	PaStreamParameters myOutParams, *paOutParams;
	
	paInParams = jpa_FillStreamParameters(  env, inParams, &myInParams );
	paOutParams = jpa_FillStreamParameters(  env, outParams, &myOutParams );
	
	return Pa_IsFormatSupported( paInParams, paOutParams, sampleRate );

}

/*
 * Class:     com_portaudio_PortAudio
 * Method:    openStream
 * Signature: (Lcom/portaudio/BlockingStream;Lcom/portaudio/StreamParameters;Lcom/portaudio/StreamParameters;III)I
 */
JNIEXPORT void JNICALL Java_com_portaudio_PortAudio_openStream
  (JNIEnv *env, jclass clazz, jobject blockingStream,  jobject inParams, jobject outParams, jint sampleRate, jint framesPerBuffer, jint flags )
{
	int err;
	PaStreamParameters myInParams, *paInParams;
	PaStreamParameters myOutParams, *paOutParams;
	PaStream *stream;
	
	paInParams = jpa_FillStreamParameters(  env, inParams, &myInParams );
	paOutParams = jpa_FillStreamParameters(  env, outParams, &myOutParams );
	err = Pa_OpenStream( &stream, paInParams, paOutParams, sampleRate, framesPerBuffer, flags, NULL, NULL );
	if( jpa_CheckError( env, err ) == 0 )
	{
		jclass cls = (*env)->GetObjectClass(env, blockingStream);
		jpa_SetLongField( env, cls, blockingStream, "nativeStream", (jlong) stream );
		if( paInParams != NULL )
		{
			jpa_SetIntField( env, cls, blockingStream, "inputFormat", paInParams->sampleFormat );
		}
		if( paOutParams != NULL )
		{
			jpa_SetIntField( env, cls, blockingStream, "outputFormat", paOutParams->sampleFormat );
		}
	}
}
