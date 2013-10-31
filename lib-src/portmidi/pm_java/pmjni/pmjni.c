#include "portmidi.h"
#include "porttime.h"
#include "jportmidi_JportMidiApi.h"
#include <stdio.h>

// these macros assume JNIEnv *env is declared and valid:
//
#define CLASS(c, obj) jclass c = (*env)->GetObjectClass(env, obj)
#define ADDRESS_FID(fid, c) \
    jfieldID fid = (*env)->GetFieldID(env, c, "address", "J")
// Uses Java Long (64-bit) to make sure there is room to store a 
// pointer. Cast this to a C long (either 32 or 64 bit) to match
// the size of a pointer. Finally cast int to pointer. All this
// is supposed to avoid C compiler warnings and (worse) losing
// address bits.
#define PMSTREAM(obj, fid) ((PmStream *) (long) (*env)->GetLongField(env, obj, fid))
// Cast stream to long to convert integer to pointer, then expand
// integer to 64-bit jlong. This avoids compiler warnings.
#define SET_PMSTREAM(obj, fid, stream) \
    (*env)->SetLongField(env, obj, fid, (jlong) (long) stream)


/*
 * Method:    Pm_Initialize
 */
JNIEXPORT jint JNICALL Java_jportmidi_JPortMidiApi_Pm_1Initialize
  (JNIEnv *env, jclass cl)
{
    return Pm_Initialize();
}


/*
 * Method:    Pm_Terminate
 */
JNIEXPORT jint JNICALL Java_jportmidi_JPortMidiApi_Pm_1Terminate
  (JNIEnv *env, jclass cl)
{
    return Pm_Terminate();
}


/*
 * Method:    Pm_HasHostError
 */
JNIEXPORT jint JNICALL Java_jportmidi_JPortMidiApi_Pm_1HasHostError
  (JNIEnv *env, jclass cl, jobject jstream)
{
    CLASS(c, jstream);
    ADDRESS_FID(fid, c);
    return Pm_HasHostError(PMSTREAM(jstream, fid));
}


/*
 * Method:    Pm_GetErrorText
 */
JNIEXPORT jstring JNICALL Java_jportmidi_JPortMidiApi_Pm_1GetErrorText
  (JNIEnv *env, jclass cl, jint i)
{
    return (*env)->NewStringUTF(env, Pm_GetErrorText(i));
}


/*
 * Method:    Pm_GetHostErrorText
 */
JNIEXPORT jstring JNICALL Java_jportmidi_JPortMidiApi_Pm_1GetHostErrorText
  (JNIEnv *env, jclass cl)
{
    char msg[PM_HOST_ERROR_MSG_LEN];
    Pm_GetHostErrorText(msg, PM_HOST_ERROR_MSG_LEN);
    return (*env)->NewStringUTF(env, msg);
}


/*
 * Method:    Pm_CountDevices
 */
JNIEXPORT jint JNICALL Java_jportmidi_JPortMidiApi_Pm_1CountDevices
  (JNIEnv *env, jclass cl)
{
    return Pm_CountDevices();
}


/*
 * Method:    Pm_GetDefaultInputDeviceID
 */
JNIEXPORT jint JNICALL Java_jportmidi_JPortMidiApi_Pm_1GetDefaultInputDeviceID
  (JNIEnv *env, jclass cl)
{
    return Pm_GetDefaultInputDeviceID();
}


/*
 * Method:    Pm_GetDefaultOutputDeviceID
 */
JNIEXPORT jint JNICALL Java_jportmidi_JPortMidiApi_Pm_1GetDefaultOutputDeviceID
  (JNIEnv *env, jclass cl)
{
    return Pm_GetDefaultOutputDeviceID();
}


/*
 * Method:    Pm_GetDeviceInterf
 */
JNIEXPORT jstring JNICALL Java_jportmidi_JPortMidiApi_Pm_1GetDeviceInterf
  (JNIEnv *env, jclass cl, jint i)
{
    const PmDeviceInfo *info = Pm_GetDeviceInfo(i);
    if (!info) return NULL;
    return (*env)->NewStringUTF(env, info->interf);
}


/*
 * Method:    Pm_GetDeviceName
 */
JNIEXPORT jstring JNICALL Java_jportmidi_JPortMidiApi_Pm_1GetDeviceName
  (JNIEnv *env, jclass cl, jint i)
{
    const PmDeviceInfo *info = Pm_GetDeviceInfo(i);
    if (!info) return NULL;
    return (*env)->NewStringUTF(env, info->name);
}


/*
 * Method:    Pm_GetDeviceInput
 */
JNIEXPORT jboolean JNICALL Java_jportmidi_JPortMidiApi_Pm_1GetDeviceInput
  (JNIEnv *env, jclass cl, jint i)
{
    const PmDeviceInfo *info = Pm_GetDeviceInfo(i);
    if (!info) return (jboolean) 0;
    return (jboolean) info->input;
}


/*
 * Method:    Pm_GetDeviceOutput
 */
JNIEXPORT jboolean JNICALL Java_jportmidi_JPortMidiApi_Pm_1GetDeviceOutput
  (JNIEnv *env, jclass cl, jint i)
{
    const PmDeviceInfo *info = Pm_GetDeviceInfo(i);
    if (!info) return (jboolean) 0;
    return (jboolean) info->output;
}


/*
 * Method:    Pm_OpenInput
 */
JNIEXPORT jint JNICALL Java_jportmidi_JPortMidiApi_Pm_1OpenInput
  (JNIEnv *env, jclass cl, 
   jobject jstream, jint index, jstring extras, jint bufsiz)
{
    PmError rslt;
    PortMidiStream *stream;
    CLASS(c, jstream);
    ADDRESS_FID(fid, c);
    rslt = Pm_OpenInput(&stream, index, NULL, bufsiz, NULL, NULL);
    SET_PMSTREAM(jstream, fid, stream);
    return rslt;
}


/*
 * Method:    Pm_OpenOutput
 */
JNIEXPORT jint JNICALL Java_jportmidi_JPortMidiApi_Pm_1OpenOutput
  (JNIEnv *env, jclass cl, jobject jstream, jint index, jstring extras,
   jint bufsiz, jint latency)
{
    PmError rslt;
    PortMidiStream *stream;
    CLASS(c, jstream);
    ADDRESS_FID(fid, c);
    rslt = Pm_OpenOutput(&stream, index, NULL, bufsiz, NULL, NULL, latency);
    SET_PMSTREAM(jstream, fid, stream);
    return rslt;
}


/*
 * Method:    Pm_SetFilter
 */
JNIEXPORT jint JNICALL Java_jportmidi_JPortMidiApi_Pm_1SetFilter
  (JNIEnv *env, jclass cl, jobject jstream, jint filters)
{
    CLASS(c, jstream);
    ADDRESS_FID(fid, c);
    return Pm_SetFilter(PMSTREAM(jstream, fid), filters);
}


/*
 * Method:    Pm_SetChannelMask
 */
JNIEXPORT jint JNICALL Java_jportmidi_JPortMidiApi_Pm_1SetChannelMask
  (JNIEnv *env, jclass cl, jobject jstream, jint mask)
{
    CLASS(c, jstream);
    ADDRESS_FID(fid, c);
    return Pm_SetChannelMask(PMSTREAM(jstream, fid), mask);
}


/*
 * Method:    Pm_Abort
 */
JNIEXPORT jint JNICALL Java_jportmidi_JPortMidiApi_Pm_1Abort
  (JNIEnv *env, jclass cl, jobject jstream)
{
    CLASS(c, jstream);
    ADDRESS_FID(fid, c);
    return Pm_Abort(PMSTREAM(jstream, fid));
}


/*
 * Method:    Pm_Close
 */
JNIEXPORT jint JNICALL Java_jportmidi_JPortMidiApi_Pm_1Close
  (JNIEnv *env, jclass cl, jobject jstream)
{
    CLASS(c, jstream);
    ADDRESS_FID(fid, c);
    return Pm_Close(PMSTREAM(jstream, fid));
}


/*
 * Method:    Pm_Read
 */
JNIEXPORT jint JNICALL Java_jportmidi_JPortMidiApi_Pm_1Read
  (JNIEnv *env, jclass cl, jobject jstream, jobject jpmevent)
{
    CLASS(jstream_class, jstream);
    ADDRESS_FID(address_fid, jstream_class);
    jclass jpmevent_class = (*env)->GetObjectClass(env, jpmevent);
    jfieldID message_fid = 
            (*env)->GetFieldID(env, jpmevent_class, "message", "I");
    jfieldID timestamp_fid = 
            (*env)->GetFieldID(env, jpmevent_class, "timestamp", "I");
    PmEvent buffer;
    PmError rslt = Pm_Read(PMSTREAM(jstream, address_fid), &buffer, 1);
    (*env)->SetIntField(env, jpmevent, message_fid, buffer.message);
    (*env)->SetIntField(env, jpmevent, timestamp_fid, buffer.timestamp);
    return rslt;
}


/*
 * Method:    Pm_Poll
 */
JNIEXPORT jint JNICALL Java_jportmidi_JPortMidiApi_Pm_1Poll
        (JNIEnv *env, jclass cl, jobject jstream)
{
    CLASS(c, jstream);
    ADDRESS_FID(fid, c);
    return Pm_Poll(PMSTREAM(jstream, fid));
}


/*
 * Method:    Pm_Write
 */
JNIEXPORT jint JNICALL Java_jportmidi_JPortMidiApi_Pm_1Write
        (JNIEnv *env, jclass cl, jobject jstream, jobject jpmevent)
{
    CLASS(jstream_class, jstream);
    ADDRESS_FID(address_fid, jstream_class);
    jclass jpmevent_class = (*env)->GetObjectClass(env, jpmevent);
    jfieldID message_fid = 
            (*env)->GetFieldID(env, jpmevent_class, "message", "I");
    jfieldID timestamp_fid = 
            (*env)->GetFieldID(env, jpmevent_class, "timestamp", "I");
    // note that we call WriteShort because it's simpler than constructing
    // a buffer and passing it to Pm_Write
    return Pm_WriteShort(PMSTREAM(jstream, address_fid),
            (*env)->GetIntField(env, jpmevent, timestamp_fid),
            (*env)->GetIntField(env, jpmevent, message_fid));
}


/*
 * Method:    Pm_WriteShort
 */
JNIEXPORT jint JNICALL Java_jportmidi_JPortMidiApi_Pm_1WriteShort
  (JNIEnv *env, jclass cl, jobject jstream, jint when, jint msg)
{
    CLASS(c, jstream);
    ADDRESS_FID(fid, c);
    return Pm_WriteShort(PMSTREAM(jstream, fid), when, msg);
}


/*
 * Method:    Pm_WriteSysEx
 */
JNIEXPORT jint JNICALL Java_jportmidi_JPortMidiApi_Pm_1WriteSysEx
  (JNIEnv *env, jclass cl, jobject jstream, jint when, jbyteArray jmsg)
{
    CLASS(c, jstream);
    ADDRESS_FID(fid, c);
    jbyte *bytes = (*env)->GetByteArrayElements(env, jmsg, 0);
    PmError rslt = Pm_WriteSysEx(PMSTREAM(jstream, fid), when, 
                                 (unsigned char *) bytes);
    (*env)->ReleaseByteArrayElements(env, jmsg, bytes, 0);
    return rslt;
}

/*
 * Method:    Pt_TimeStart
 */
JNIEXPORT jint JNICALL Java_jportmidi_JPortMidiApi_Pt_1TimeStart
        (JNIEnv *env, jclass c, jint resolution)
{
    return Pt_Start(resolution, NULL, NULL);
}

/*
 * Method:    Pt_TimeStop
 */
JNIEXPORT jint JNICALL Java_jportmidi_JPortMidiApi_Pt_1TimeStop
        (JNIEnv *env, jclass c)
 {
     return Pt_Stop();
 }

/*
 * Method:    Pt_Time
 */
JNIEXPORT jint JNICALL Java_jportmidi_JPortMidiApi_Pt_1Time
        (JNIEnv *env, jclass c)
 {
     return Pt_Time();
 }

/*
 * Method:    Pt_TimeStarted
 */
JNIEXPORT jboolean JNICALL Java_jportmidi_JPortMidiApi_Pt_1TimeStarted
        (JNIEnv *env, jclass c)
{
    return Pt_Started();
}


