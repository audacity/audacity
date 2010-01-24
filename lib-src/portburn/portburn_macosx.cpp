/*
 * PortBurn
 * Mac OS X implementation
 *
 * Authors:
 *   Dominic Mazzoni, Leland Lucius
 *
 * With assistance from Apple's sample code:
 *   /Developer/Examples/DiscRecording/C/
 *
 * License: LGPL
 */

#include "portburn.h"
#include "portburn_staging.h"

#include <string.h>

#include <DiscRecording/DiscRecording.h>

typedef struct {
   OSStatus           err;
   CFArrayRef         deviceList;
   DRDeviceRef        device;
   CFMutableArrayRef  trackArray;
   DRBurnRef          burn;
   DREraseRef         erase;
   void              *staging;
   float              frac;
   int                verify;
   int                underrun;
   int                speed;
   int                test;
   int                eject;
   bool               gapless;
} PBHandle;

char *PortBurn_CStringFromCFString(CFStringRef cfname)
{
   char *name;
   CFIndex len;

   len = CFStringGetMaximumSizeForEncoding(CFStringGetLength(cfname),
                                           kCFStringEncodingASCII);
   name = (char *) malloc(len + 1);
   if (name == NULL) {
      return NULL;
   }

   name[0] = 0;
   CFStringGetCString(cfname, name, len + 1, kCFStringEncodingASCII);

   return name;
}

void *PortBurn_Open()
{
   PBHandle *h;

   h = (PBHandle *) calloc(1, sizeof(PBHandle));
   if (h == NULL) {
      return NULL;
   }

   h->frac = 0.0;

   h->test = pbTestDefault;
   h->verify = pbVerifyDefault;
   h->underrun = pbUnderrunDefault;
   h->eject = pbEjectDefault;
   h->gapless = pbGaplessDefault;
   h->speed = pbSpeedDefault;

   return h;
}

/* Cleanup */
void PortBurn_Close(void *handle)
{
   PBHandle *h = (PBHandle *)handle;

   if (h == NULL) {
      return;
   }

   if (h->device) {
      PortBurn_CloseDevice(h);
   }

   if (h->deviceList) {
      CFRelease(h->deviceList);
   }

   free(h);
}


/* Return a human-readable error string for the last operating system
   specific error (NOT human readable strings for the PortBurn error
   codes).  Caller should dispose of the returned string using free(). */
char *PortBurn_LastError(void *handle)
{
   PBHandle *h = (PBHandle *)handle;
   CFStringRef cferr;
   char *result = NULL;

   if (h == NULL) {
      return NULL;
   }

   cferr = DRCopyLocalizedStringForDiscRecordingError(h->err);
   if (cferr) {
      result = PortBurn_CStringFromCFString(cferr);
      CFRelease(cferr);
   }

   return result;
}

/* Get the number of devices capable of burning audio CDs.
   If the result is N, then calls to GetDeviceName and OpenDevice
   are guaranteed to be valid for indices from 0 up to N-1, until
   the next time you call GetNumDevices.  At that point, the list of
   devices will be rescanned, and may be different. */
int PortBurn_GetNumDevices(void *handle)
{
   PBHandle *h = (PBHandle *)handle;

   if (h == NULL) {
      return 0;
   }

   if (h->deviceList != NULL) {
      CFRelease(h->deviceList);
      h->deviceList = NULL;
   }

   h->deviceList = DRCopyDeviceArray();
   if (h->deviceList == NULL) {
      return 0;
   }

   return (int) CFArrayGetCount(h->deviceList);
}

/* Get the name of the device with a given index.  Only valid
   after a call to GetNumDevices. */
char *PortBurn_GetDeviceName(void *handle, int index)
{
   PBHandle *h = (PBHandle *)handle;
   CFDictionaryRef deviceInfo;
   CFStringRef bus;
   CFStringRef vendor;
   CFStringRef product;
   CFStringRef cfname;
   DRDeviceRef device;
   char *result;

   if (h == NULL) {
      return NULL;
   }

   if (h->deviceList == NULL) {
      return NULL;
   }

   if (index < 0 || index >= (int) CFArrayGetCount(h->deviceList)) {
      return NULL;
   }

   device = (DRDeviceRef) CFArrayGetValueAtIndex(h->deviceList, index);

   deviceInfo = DRDeviceCopyInfo(device);
   if (deviceInfo == NULL) {
      return NULL;
   }

   bus = (CFStringRef)CFDictionaryGetValue(deviceInfo,
                                           kDRDevicePhysicalInterconnectKey);

   if (CFEqual(bus, kDRDevicePhysicalInterconnectFireWire)) {
      bus = CFSTR("FireWire: ");
   }
   else if (CFEqual(bus, kDRDevicePhysicalInterconnectUSB)) {
      bus = CFSTR("USB: ");
   }
   else if (CFEqual(bus, kDRDevicePhysicalInterconnectATAPI)) {
      bus = CFSTR("ATAPI: ");
   }
   else if (CFEqual(bus, kDRDevicePhysicalInterconnectSCSI)) {
      bus = CFSTR("SCSI: ");
   }
   else {
      bus = CFSTR("");
   }

   vendor = (CFStringRef)
      CFDictionaryGetValue(deviceInfo, kDRDeviceVendorNameKey);
   product = (CFStringRef)
      CFDictionaryGetValue(deviceInfo, kDRDeviceProductNameKey);

   cfname = CFStringCreateWithFormat(NULL, NULL,
                                     CFSTR("%@%@ %@"),
                                     bus, vendor, product);
   CFRelease(deviceInfo);

   if (cfname == NULL) {
      return NULL;
   }

   result = PortBurn_CStringFromCFString(cfname);

   CFRelease(cfname);

   return result;
}

/* Open a particular device by index number.  Returns 0 on success;
   any nonzero value indicates an error, for example if the device is
   already open by some other program. */
int PortBurn_OpenDevice(void *handle, int index)
{
   PBHandle *h = (PBHandle *)handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   if (h->deviceList == NULL) {
      return pbErrMustCallGetNumDevices;
   }

   if (h->device != NULL) {
      return pbErrDeviceAlreadyOpen;
   }

   if (index < 0 || index >= (int) CFArrayGetCount(h->deviceList)) {
      return pbErrInvalidDeviceIndex;
   }

   h->device = (DRDeviceRef) CFArrayGetValueAtIndex(h->deviceList, index);

   /* This just indicates interest; it doesn't return an error. */
   DRDeviceAcquireMediaReservation(h->device);

   return pbSuccess;
}

/* Close a device */
int PortBurn_CloseDevice(void *handle)
{
   PBHandle *h = (PBHandle *)handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   if (h->device == NULL) {
      return pbErrDeviceNotOpen;
   }

   if (h->burn != NULL) {
      DRBurnAbort(h->burn);
      CFRelease(h->burn);
      h->burn = NULL;
   }

   if (h->erase != NULL) {
      CFRelease(h->erase);
      h->erase = NULL;
   }

   if (h->trackArray != NULL) {
      CFRelease(h->trackArray);
      h->trackArray = NULL;
   }

   if (h->staging != NULL) {
      PortBurn_CleanupStaging(h->staging);
      h->staging = NULL;
   }

   DRDeviceReleaseMediaReservation(h->device);

   h->device = NULL;

   h->frac = 0.0;

   h->test = pbTestDefault;
   h->verify = pbVerifyDefault;
   h->underrun = pbUnderrunDefault;
   h->eject = pbEjectDefault;
   h->gapless = pbGaplessDefault;
   h->speed = pbSpeedDefault;

   return pbSuccess;
}

/* Eject the media in the currently opened device */
int PortBurn_EjectDevice(void *handle)
{
   PBHandle *h = (PBHandle *)handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   if (h->device == NULL) {
      return pbErrDeviceNotOpen;
   }

   h->err = DRDeviceOpenTray(h->device);
   if (h->err == noErr) {
      return pbSuccess;  /* success */
   }

   h->err = DRDeviceEjectMedia(h->device);
   if (h->err == noErr) {
      return pbSuccess;  /* success */
   }

   return pbErrCannotEject;
}

/* Erase the media in the currently opened device */
int PortBurn_StartErasing(void *handle, int type)
{
   CFMutableDictionaryRef props;

   PBHandle *h = (PBHandle *)handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   if (h->device == NULL) {
      return pbErrDeviceNotOpen;
   }

   if (h->burn != NULL || h->erase != NULL) {
      return pbErrCannotStartErasing;
   }

   h->erase = DREraseCreate(h->device);
   if (h->erase == NULL) {
      return pbErrCannotPrepareToErase;
   }

   props = CFDictionaryCreateMutableCopy(NULL, 0, DREraseGetProperties(h->erase));
   if (props == NULL) {
      CFRelease(h->erase);
      h->erase = NULL;
      return pbErrCannotPrepareToErase;
   }

   CFDictionaryAddValue(props,
                        kDREraseTypeKey,
                        type ? kDREraseTypeComplete : kDREraseTypeQuick);

   DREraseSetProperties(h->erase, props);

   CFRelease(props);

   h->frac = 0.0;

   h->err = DREraseStart(h->erase);
   if (h->err != noErr) {
      CFRelease(h->erase);
      h->erase = NULL;
      return pbErrCannotStartErasing;
   }

   return pbSuccess;
}

/* 
*/

int PortBurn_GetEraseStatus(void *handle, float *out_fraction_complete)
{
   PBHandle *h = (PBHandle *)handle;
   CFDictionaryRef status;
   CFStringRef stateRef;
   CFNumberRef fracRef;
   float frac = 0.0;

   *out_fraction_complete = h->frac;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   if (h->erase == NULL) {
      return pbErrNotCurrentlyErasing;
   }

   status = DREraseCopyStatus(h->erase);
   if (status == NULL) {
      return pbErrCannotGetEraseStatus;
   }

   fracRef = (CFNumberRef) CFDictionaryGetValue(status, kDRStatusPercentCompleteKey);
   if (fracRef != NULL) {
      CFNumberGetValue(fracRef, kCFNumberFloatType, &frac);
   }

   stateRef = (CFStringRef) CFDictionaryGetValue(status, kDRStatusStateKey);
   if (stateRef == NULL) {
      /* Stick with the last percentage */
      return pbSuccess;
   }

   if (CFEqual(stateRef, kDRStatusStateNone)) {
      /* Stick with the last percentage */
      return pbSuccess;
   }

   if (CFEqual(stateRef, kDRStatusStateErasing)) {
      /* The fraction ("percentage") complete will be valid during
         the majority of this range */
      float newFrac = 0.0;
      if (frac > 0.0 && frac <= 1.0) {
         newFrac = frac;
      }

      /* Scale it to the range 0.05 - 0.99 */
      newFrac = 0.05 + 0.94 * newFrac;

      /* Only use that value if it's larger than the previous value */
      if (newFrac > h->frac) {
         h->frac = newFrac;
      }
   }

   if (CFEqual(stateRef, kDRStatusStateDone)) {
      /* Returning a fraction complete of 1.0 means we're done! */
      h->frac = 1.0;
      CFRelease(h->erase);
      h->erase = NULL;
   }

   if (CFEqual(stateRef, kDRStatusStateFailed)) {
      CFRelease(status);
      return pbErrEraseFailed;
   }

   *out_fraction_complete = h->frac;

   CFRelease(status);

   return pbSuccess;
}

/* */
int PortBurn_GetMediaState(void *handle, int *state)
{
   PBHandle *h = (PBHandle *)handle;
   CFDictionaryRef deviceStatus;
   CFStringRef mediaState;
   CFDictionaryRef mediaInfo;
   CFBooleanRef	bref;
   int mstate = pbMediaNotWritable;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   if (h->device == NULL) {
      return pbErrDeviceNotOpen;
   }

   /* First we check to see that we have blank media in the drive. */
   deviceStatus = DRDeviceCopyStatus(h->device);
   if (deviceStatus == NULL) {
      return pbErrCannotAccessDevice;
   }

   mediaState = (CFStringRef) CFDictionaryGetValue(deviceStatus,
                                                   kDRDeviceMediaStateKey);
   if (mediaState == NULL) {
      CFRelease(deviceStatus);
      return pbErrCannotAccessDevice;
   }

   if (!CFEqual(mediaState, kDRDeviceMediaStateMediaPresent)) {
      CFRelease(deviceStatus);
      *state = pbMediaNone;
      return pbSuccess;
   }

   mediaInfo = (CFDictionaryRef) CFDictionaryGetValue(deviceStatus,
                                                      kDRDeviceMediaInfoKey);
   bref = (CFBooleanRef) CFDictionaryGetValue(mediaInfo,
                                              kDRDeviceMediaIsBlankKey);

   if (bref != NULL && CFBooleanGetValue(bref)) {
      mstate |= pbMediaBlank;
   }

   bref = (CFBooleanRef) CFDictionaryGetValue(mediaInfo,
                                              kDRDeviceMediaIsErasableKey);

   if (bref != NULL && CFBooleanGetValue(bref)) {
      mstate |= pbMediaErasable;
   }

   bref = (CFBooleanRef) CFDictionaryGetValue(mediaInfo,
                                              kDRDeviceMediaIsAppendableKey);

   if (bref != NULL && CFBooleanGetValue(bref)) {
      mstate |= pbMediaAppendable;
   }

   bref = (CFBooleanRef) CFDictionaryGetValue(mediaInfo,
                                              kDRDeviceMediaIsOverwritableKey);

   if (bref != NULL && CFBooleanGetValue(bref)) {
      mstate |= pbMediaOverwritable;
   }

   CFRelease(deviceStatus);

   *state = mstate;

   return pbSuccess;
}

/* This indicates you're ready to start staging audio data for the
   currently opened device.  At this point you are committing to
   exclusive access to the CD burner, and this is the function that
   will fail if another program is using the device, or if there is
   no writable CD media in the device at this point.
   You should pass in the path to a temporary directory that has at
   least 700 MB of free space, to stage the audio, although note that
   not all implementations will make use of this directory. */
int PortBurn_StartStaging(void *handle, const char *tmpdir)
{
   PBHandle *h = (PBHandle *)handle;
   int state;
   int rc;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   if (h->device == NULL) {
      return pbErrDeviceNotOpen;
   }

   if (h->staging != NULL) {
      return pbErrAlreadyStagingOrBurning;
   }

   rc = PortBurn_GetMediaState(h, &state);
   if (rc != pbSuccess) {
      return rc;
   }

   /* check to see that we have blank media in the drive. */
   if (!(state & pbMediaBlank)) {
      return pbErrMediaIsNotBlank;
   }

   h->staging = PortBurn_TempDirStaging(tmpdir);
   if (h->staging == NULL) {
      return pbErrCannotCreateStagingDirectory;
   }

   h->trackArray = CFArrayCreateMutable(kCFAllocatorDefault, 0,
                                        &kCFTypeArrayCallBacks);

   return pbSuccess;
}

/* Start a new audio track.  Pass the name of the track, and the
   length in CD Audio frames (each frame is 1/75.0 of a second, exactly). */
int PortBurn_StartTrack(void *handle, const char *name)
{
   PBHandle *h = (PBHandle *)handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   if (h->staging == NULL) {
      return pbErrMustCallStartStaging;
   }

   if (PortBurn_StartStagingTrack(h->staging, name, FALSE) != 0) {
      return pbErrCannotCreateStagingFile;
   }

   return pbSuccess;
}

/* Add one frame of audio to the current track.  The buffer must be exactly
   1176 elements long, containing interleaved left and right audio samples.
   The values should be signed 16-bit numbers in the native endianness of
   this computer. */
int PortBurn_AddFrame(void *handle, short *buffer)
{
   PBHandle *h = (PBHandle *)handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   if (h->staging == NULL) {
      return pbErrMustCallStartStaging;
   }

   if (PortBurn_AddStagingFrame(h->staging, buffer) != 0) {
      return pbErrCannotWriteToStagingFile;
   }

   return pbSuccess;
}

/* Finish the current audio track. */
int PortBurn_EndTrack(void *handle)
{
   PBHandle *h = (PBHandle *)handle;
   DRAudioTrackRef track;
   Boolean isDirectory;
   const char *filename;
   FSRef fsref;
   int index;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   if (h->staging == NULL) {
      return pbErrMustCallStartStaging;
   }

   if (PortBurn_EndStagingTrack(h->staging) != 0) {
      return pbErrCannotStageTrack;
   }

   index = PortBurn_GetNumStagedTracks(h->staging);
   if (index <= 0) {
      return pbErrCannotStageTrack;
   }

   filename = PortBurn_GetStagedFilename(h->staging, index - 1);
   if (filename == NULL) {
      return pbErrCannotAccessStagedFile;
   }

   h->err = FSPathMakeRef((const UInt8*)filename, &fsref, &isDirectory);
   if (h->err != noErr) {
      return pbErrCannotAccessStagedFile;
   }

   if (isDirectory) {
      return pbErrCannotAccessStagedFile;
   }

   track = DRAudioTrackCreate(&fsref);
   if (track == NULL) {
      return pbErrCannotUseStagedFileForBurning;
   }

   if (h->gapless) {
      CFMutableDictionaryRef props;

      props = CFDictionaryCreateMutableCopy(NULL, 0, DRTrackGetProperties(track));
      if (props == NULL) {
         CFRelease(track);
         return pbErrCannotUseStagedFileForBurning;
      }

      SInt64 gap = 0;
      CFNumberRef num = CFNumberCreate(NULL, kCFNumberSInt64Type, &gap);
      if (num != NULL) {
         CFDictionarySetValue(props,
                              kDRPreGapLengthKey,
                              num);
         CFRelease(num);
      }
   
      DRTrackSetProperties(track, props);

      CFRelease(props);
   }

   CFArrayAppendValue(h->trackArray, track);

   CFRelease(track);

   return pbSuccess;
}

/* Begin burning the disc. */
int PortBurn_StartBurning(void *handle)
{
   CFMutableDictionaryRef props;

   PBHandle *h = (PBHandle *)handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   if (h->device == NULL) {
      return pbErrDeviceNotOpen;
   }

   if (h->burn != NULL || h->erase != NULL) {
      return pbErrCannotStartBurning;
   }

   h->burn = DRBurnCreate(h->device);
   if (h->burn == NULL) {
      return pbErrCannotPrepareToBurn;
   }

   props = CFDictionaryCreateMutableCopy(NULL, 0, DRBurnGetProperties(h->burn));
   if (props == NULL) {
      DRBurnAbort(h->burn);
      CFRelease(h->burn);
      h->burn = NULL;
      return pbErrCannotPrepareToBurn;
   }

   CFDictionaryAddValue(props,
                        kDRBurnCompletionActionKey,
                        h->eject ? kDRBurnCompletionActionEject : kDRBurnCompletionActionMount);
   CFDictionaryAddValue(props,
                        kDRBurnVerifyDiscKey,
                        h->verify ? kCFBooleanTrue : kCFBooleanFalse);
   CFDictionaryAddValue(props,
                        kDRBurnUnderrunProtectionKey,
                        h->underrun ? kCFBooleanTrue : kCFBooleanFalse);
   CFDictionaryAddValue(props,
                        kDRBurnTestingKey,
                        h->test ? kCFBooleanTrue : kCFBooleanFalse);

   if (h->speed != pbSpeedDefault) {
      float speed;
      if (h->speed == pbSpeedMax) {
         speed = kDRDeviceBurnSpeedMax;
      }
      else {
         speed = h->speed * kDRDeviceBurnSpeedCD1x;
      }

      CFNumberRef num = CFNumberCreate(NULL, kCFNumberFloatType, &speed);
      if (num != NULL) {
         CFDictionaryAddValue(props,
                              kDRBurnRequestedSpeedKey,
                              num);
         CFRelease(num);
      }
   }

   DRBurnSetProperties(h->burn, props);

   CFRelease(props);

   h->frac = 0.0;

   h->err = DRBurnWriteLayout(h->burn, h->trackArray);
   if (h->err != noErr) {
      DRBurnAbort(h->burn);
      CFRelease(h->burn);
      h->burn = NULL;
      return pbErrCannotStartBurning;
   }

   return pbSuccess;
}

/* Cancel if burning was in progress.  It might take a while for
   this to take effect; wait until GetStatus says 1.0 to close
   the device. */
int PortBurn_CancelBurning(void *handle)
{
   PBHandle *h = (PBHandle *)handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   if (h->burn == NULL) {
      return pbErrNotCurrentlyBurning;
   }

   DRBurnAbort(h->burn);

   return pbSuccess;
}

/* During burning, returns the fraction complete in the given
   float pointer, from 0.0 to 1.0.  If this function returns
   nonzero, the disc burning has failed and should be aborted.
   If *out_fraction_complete is equal to 1.0, the burning is done;
   you can call PortBurn_CloseDevice.
*/

int PortBurn_GetStatus(void *handle, float *out_fraction_complete)
{
   PBHandle *h = (PBHandle *)handle;
   CFDictionaryRef status;
   CFStringRef stateRef;
   CFNumberRef fracRef;
   CFNumberRef trackNumRef;
   float frac = 0.0;
   int trackNum = 0;

   *out_fraction_complete = h->frac;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   if (h->burn == NULL) {
      return pbErrNotCurrentlyBurning;
   }

   status = DRBurnCopyStatus(h->burn);
   if (status == NULL) {
      return pbErrCannotGetBurnStatus;
   }

   trackNumRef = (CFNumberRef) CFDictionaryGetValue(status, kDRStatusCurrentTrackKey);
   if (trackNumRef != NULL) {
      CFNumberGetValue(trackNumRef, kCFNumberIntType, &trackNum);
   }

   fracRef = (CFNumberRef) CFDictionaryGetValue(status, kDRStatusPercentCompleteKey);
   if (fracRef != NULL) {
      CFNumberGetValue(fracRef, kCFNumberFloatType, &frac);
   }

   stateRef = (CFStringRef) CFDictionaryGetValue(status, kDRStatusStateKey);

   if (stateRef == NULL) {
      /* Stick with the last percentage */
      return pbSuccess;
   }

   if (CFEqual(stateRef, kDRStatusStateNone)) {
      /* Stick with the last percentage */
      return pbSuccess;
   }

   if (CFEqual(stateRef, kDRStatusStatePreparing)) {
      /* This takes about 1 second */
      h->frac = 0.01;
   }

   if (CFEqual(stateRef, kDRStatusStateSessionOpen)) {
      /* This takes about 3 seconds */
      h->frac = 0.02;
   }

   if (CFEqual(stateRef, kDRStatusStateTrackOpen) ||
       CFEqual(stateRef, kDRStatusStateTrackWrite) ||
       CFEqual(stateRef, kDRStatusStateTrackClose) ||
       CFEqual(stateRef, kDRStatusStateSessionClose)) {
      /* The fraction ("percentage") complete will be valid during
         the majority of this range */
      float newFrac = 0.0;
      if (frac > 0.0 && frac <= 1.0) {
         newFrac = frac;
      }

      /* Scale it to the range 0.05 - 0.99 */
      newFrac = 0.05 + 0.94 * newFrac;

      /* Only use that value if it's larger than the previous value */
      if (newFrac > h->frac) {
         h->frac = newFrac;
      }
   }

   if (CFEqual(stateRef, kDRStatusStateDone)) {
      /* Returning a fraction complete of 1.0 means we're done! */
      h->frac = 1.0;
      CFRelease(h->burn);
      h->burn = NULL;
   }

   if (CFEqual(stateRef, kDRStatusStateFailed)) {
      CFRelease(status);
      return pbErrBurnFailed;
   }

   *out_fraction_complete = h->frac;

   CFRelease(status);

   return pbSuccess;
}

/* Get option value. */

int PortBurn_GetOption(void *handle, int option, int *value)
{
   PBHandle *h = (PBHandle *)handle;
   int ret = pbSuccess;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   switch (option)
   {
      case pbOptTest:
      {
         *value = h->test;
      }
      break;

      case pbOptVerify:
      {
         *value = h->verify;
      }
      break;

      case pbOptUnderrun:
      {
         *value = h->underrun;
      }
      break;

      case pbOptEject:
      {
         *value = h->eject;
      }
      break;

      case pbOptGapless:
      {
         *value = h->gapless;
      }
      break;

      case pbOptSpeed:
      {
      }
      break;

      default:
      {
         ret = pbErrInvalidOption;
      }
      break;
   }

   return ret;
}

int PortBurn_SetOption(void *handle, int option, int value)
{
   PBHandle *h = (PBHandle *)handle;
   int ret = pbSuccess;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   switch (option)
   {
      case pbOptTest:
      {
         h->test = value != 0;
      }
      break;

      case pbOptVerify:
      {
         h->verify = value != 0;
      }
      break;

      case pbOptUnderrun:
      {
         h->underrun = value != 0;
      }
      break;

      case pbOptEject:
      {
         h->eject = value != 0;
      }
      break;

      case pbOptGapless:
      {
         h->gapless = value;
      }
      break;

      case pbOptSpeed:
      {
         h->speed = value;
      }
      break;

      default:
      {
         ret = pbErrInvalidOption;
      }
      break;
   }

   return ret;
}

int PortBurn_GetSpeeds(void *handle, int *cnt, int *speeds[])
{
   PBHandle *h = (PBHandle *)handle;
   CFDictionaryRef deviceStatus;
   CFDictionaryRef mediaInfo;
   CFStringRef mediaState;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   if (h->device == NULL) {
      return pbErrDeviceNotOpen;
   }

   /* First we check to see that we have blank media in the drive. */
   deviceStatus = DRDeviceCopyStatus(h->device);
   if (deviceStatus == NULL) {
      return pbErrCannotAccessDevice;
   }

   mediaState = (CFStringRef) CFDictionaryGetValue(deviceStatus,
                                                   kDRDeviceMediaStateKey);
   if (mediaState == NULL) {
      CFRelease(deviceStatus);
      return pbErrCannotAccessDevice;
   }

   if (!CFEqual(mediaState, kDRDeviceMediaStateMediaPresent)) {
      CFRelease(deviceStatus);
      return pbErrNoMediaInDrive;
   }

   mediaInfo = (CFDictionaryRef) CFDictionaryGetValue(deviceStatus,
                                                      kDRDeviceMediaInfoKey);

   CFArrayRef sarray = (CFArrayRef)
      CFDictionaryGetValue(mediaInfo, kDRDeviceBurnSpeedsKey);

   if (sarray != NULL && CFArrayGetCount(sarray) != 0) {
      CFIndex ndx;
      *cnt = CFArrayGetCount(sarray);
      *speeds = (int *) malloc((*cnt + 1) * sizeof(int));
      for (ndx = 0; ndx < *cnt; ndx++) {
         float speed;

         CFNumberRef num = (CFNumberRef)
            CFArrayGetValueAtIndex(sarray, ndx);

         if (num != NULL) {
            CFNumberGetValue(num, kCFNumberFloatType, &speed);
            *speeds[ndx] = lrint(speed / kDRDeviceBurnSpeedCD1x);
         }
      }
   }
   CFRelease(deviceStatus);

   return pbSuccess;
}
