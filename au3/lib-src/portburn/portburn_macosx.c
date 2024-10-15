/*
 * PortBurn
 * Mac OS X implementation
 *
 * Authors:
 *   Dominic Mazzoni
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
   CFArrayRef         trackLayout;
   void              *staging;
   float              frac;
} PBHandle;

char *PortBurn_CStringFromCFString(CFStringRef cfname) {
   char *name;
   CFIndex len;

   len = CFStringGetMaximumSizeForEncoding(CFStringGetLength(cfname),
                                           kCFStringEncodingASCII);
   name = (char *)malloc(len + 1);
   name[0] = 0;
   CFStringGetCString(cfname, name, len + 1, kCFStringEncodingASCII);
   return name;
}

void *PortBurn_Open()
{   
   PBHandle *h;

   h = (PBHandle *)malloc(sizeof(PBHandle));
   memset(h, 0, sizeof(PBHandle));

   return h;
}

/* Cleanup */
void PortBurn_Close(void *handle) {
   PBHandle *h = (PBHandle *)handle;
   if (!h)
      return;

   if (h->deviceList)
      CFRelease(h->deviceList);

   free(h);
}


/* Return a human-readable error string for the last operating system
   specific error (NOT human readable strings for the PortBurn error
   codes).  Caller should dispose of the returned string using free(). */
const char *PortBurn_LastError(void *handle)
{
   PBHandle *h = (PBHandle *)handle;
   CFStringRef cferr;

   if (!h)
      return NULL;

   cferr = DRCopyLocalizedStringForDiscRecordingError(h->err);
   if (cferr) {
      char *result = PortBurn_CStringFromCFString(cferr);
      CFRelease(cferr);
      return result;
   }
   else
      return NULL;
}

/* Get the number of devices capable of burning audio CDs.
   If the result is N, then calls to GetDeviceName and OpenDevice
   are guaranteed to be valid for indices from 0 up to N-1, until
   the next time you call GetNumDevices.  At that point, the list of
   devices will be rescanned, and may be different. */
int PortBurn_GetNumDevices(void *handle)
{
   PBHandle *h = (PBHandle *)handle;

   if (!h)
      return 0;

   if (h->deviceList) {
      CFRelease(h->deviceList);
      h->deviceList = NULL;
   }

   h->deviceList = DRCopyDeviceArray();
   if (!h->deviceList)
      return 0;

   return (int)CFArrayGetCount(h->deviceList);
}

/* Get the name of the device with a given index.  Only valid
   after a call to GetNumDevices. */
char *PortBurn_GetDeviceName(void *handle, int index)
{
   PBHandle *h = (PBHandle *)handle;
   CFDictionaryRef deviceInfo;
   CFStringRef bus, vendor, product;
   CFStringRef cfname;
   char *result;

   if (!h)
      return NULL;

   if (!h->deviceList)
      return NULL;

   DRDeviceRef device;
   device = (DRDeviceRef)CFArrayGetValueAtIndex(h->deviceList, index);

   deviceInfo = DRDeviceCopyInfo(device);
   bus = (CFStringRef)CFDictionaryGetValue(deviceInfo,
                                           kDRDevicePhysicalInterconnectKey);
   if (CFEqual(bus, kDRDevicePhysicalInterconnectFireWire))
      bus = CFSTR("FireWire: ");
   else if (CFEqual(bus, kDRDevicePhysicalInterconnectUSB))
      bus = CFSTR("USB: ");
   else if (CFEqual(bus, kDRDevicePhysicalInterconnectATAPI))
      bus = CFSTR("ATAPI: ");
   else if (CFEqual(bus, kDRDevicePhysicalInterconnectSCSI))
      bus = CFSTR("SCSI: ");
   else
      bus = CFSTR("");

   vendor = CFDictionaryGetValue(deviceInfo, kDRDeviceVendorNameKey);
   product = CFDictionaryGetValue(deviceInfo, kDRDeviceProductNameKey);

   cfname = CFStringCreateWithFormat(NULL, NULL,
                                     CFSTR("%@%@ %@"),
                                     bus, vendor, product);

   result = PortBurn_CStringFromCFString(cfname);

   CFRelease(cfname);
   CFRelease(deviceInfo);

   return result;
}

/* Open a particular device by index number.  Returns 0 on success;
   any nonzero value indicates an error, for example if the device is
   already open by some other program. */
int PortBurn_OpenDevice(void *handle, int index)
{
   PBHandle *h = (PBHandle *)handle;
   if (!h)
      return pbErrNoHandle;

   if (!h->deviceList)
      return pbErrMustCallGetNumDevices;

   h->device = (DRDeviceRef)CFArrayGetValueAtIndex(h->deviceList, index);

   /* This just indicates interest; it doesn't return an error. */
   DRDeviceAcquireMediaReservation(h->device);

   return pbSuccess;
}

/* Close a device */
int PortBurn_CloseDevice(void *handle)
{
   PBHandle *h = (PBHandle *)handle;

   if (!h)
      return pbErrNoHandle;

   if (!h->device)
      return pbErrDeviceNotOpen;

   if (h->burn != NULL) {
      CFRelease(h->burn);
      h->burn = NULL;
   }

   if (h->trackArray) {
      CFRelease(h->trackArray);
      h->trackArray = NULL;
   }
 
   DRDeviceReleaseMediaReservation(h->device);
   CFRelease(h->device);
   h->device = NULL;

   if (h->staging) {
      PortBurn_CleanupStaging(h->staging);
      h->staging = NULL;
   }

   return pbSuccess;
}

/* Eject the media in the currently opened device */
int PortBurn_EjectDevice(void *handle)
{
   PBHandle *h = (PBHandle *)handle;

   if (!h)
      return pbErrNoHandle;

   if (!h->device)
      return pbErrDeviceNotOpen;

   h->err = DRDeviceOpenTray(h->device);
   if (noErr == h->err)
      return pbSuccess;  /* success */

   h->err = DRDeviceEjectMedia(h->device);
   if (noErr == h->err)
      return pbSuccess;  /* success */

   return pbErrCannotEject;
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
   CFDictionaryRef deviceStatus;
   CFStringRef mediaState;
   CFDictionaryRef mediaInfo;
   CFBooleanRef	blank;
   CFBooleanRef	writable;
   CFBooleanRef	reserved;

   if (!h)
      return pbErrNoHandle;

   if (!h->device)
      return pbErrDeviceNotOpen;

   if (h->staging)
      return pbErrAlreadyStagingOrBurning;

   /* First we check to see that we have blank media in the drive. */
   deviceStatus = DRDeviceCopyStatus(h->device);
   mediaState = (CFStringRef)CFDictionaryGetValue(deviceStatus,
                                                  kDRDeviceMediaStateKey);
   if (mediaState == NULL) {
      CFRelease(deviceStatus);
      return pbErrCannotAccessDevice;
   }

   if (!CFEqual(mediaState, kDRDeviceMediaStateMediaPresent)) {
      CFRelease(deviceStatus);
      return pbErrNoMediaInDrive;
   }

   mediaInfo = (CFDictionaryRef)CFDictionaryGetValue(deviceStatus,
                                                     kDRDeviceMediaInfoKey);
   blank = (CFBooleanRef)CFDictionaryGetValue(mediaInfo,
                                              kDRDeviceMediaIsBlankKey);

   if (blank == NULL || !CFBooleanGetValue(blank)) {
      CFRelease(deviceStatus);
      return pbErrMediaIsNotBlankAndWritable;
   }

   writable = (CFBooleanRef)CFDictionaryGetValue(mediaInfo,
                                                kDRDeviceMediaIsAppendableKey);

   if (writable == NULL || !CFBooleanGetValue(writable)) {
      CFRelease(deviceStatus);
      return pbErrMediaIsNotBlankAndWritable;
   }

   reserved = (CFBooleanRef)CFDictionaryGetValue(mediaInfo,
                                                 kDRDeviceMediaIsReservedKey);
   if (reserved == NULL || !CFBooleanGetValue(reserved)) {
      CFRelease(deviceStatus);
      return pbErrCannotReserveDevice;
   }
	
   CFRelease(deviceStatus);

   h->staging = PortBurn_TempDirStaging(tmpdir);

   if (!h->staging) {
      return pbErrCannotCreateStagingDirectory;
   }

   h->trackArray = CFArrayCreateMutable(kCFAllocatorDefault, 0,
                                        &kCFTypeArrayCallBacks);

   return pbSuccess;
}

/* Start a new audio track.  Pass the name of the track, and the
   length in CD Audio frames (each frame is 1/75.0 of a second, exactly). */
int PortBurn_StartTrack(void *handle, const char *name, int frames)
{
   PBHandle *h = (PBHandle *)handle;
   if (!h)
      return pbErrNoHandle;

   if (!h->staging)
      return pbErrMustCallStartStaging;

   if (0 == PortBurn_StartStagingTrack(h->staging, name, frames))
      return pbSuccess;
   else
      return pbErrCannotCreateStagingFile;
}

/* Add one frame of audio to the current track.  The buffer must be exactly
   1176 elements long, containing interleaved left and right audio samples.
   The values should be signed 16-bit numbers in the native endianness of
   this computer. */
int PortBurn_AddFrame(void *handle, short *buffer)
{
   PBHandle *h = (PBHandle *)handle;

   if (!h)
      return pbErrNoHandle;

   if (!h->staging)
      return pbErrMustCallStartTrack;

   if (0 == PortBurn_AddStagingFrame(h->staging, buffer))
      return pbSuccess;
   else
      return pbErrCannotWriteToStagingFile;
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

   if (!h)
      return pbErrNoHandle;

   if (!h->staging)
      return pbErrMustCallStartStaging;

   if (0 != PortBurn_EndStagingTrack(h->staging))
      return pbErrCannotStageTrack;

   index = PortBurn_GetNumStagedTracks(h->staging);
   if (index <= 0)
      return pbErrCannotStageTrack;

   filename = PortBurn_GetStagedFilename(h->staging, index - 1);
   printf("Filename: '%s'\n", filename);
   h->err = FSPathMakeRef((const UInt8*)filename, &fsref, &isDirectory);
   if (h->err != noErr)
      return pbErrCannotAccessStagedFile;
   if (isDirectory)
      return pbErrCannotAccessStagedFile;

   track = DRAudioTrackCreate(&fsref);

   CFArrayAppendValue(h->trackArray, track);
   CFRelease(track);

   if (track)
      return pbSuccess;
   else
      return pbErrCannotUseStagedFileForBurning;
}

/* Begin burning the disc. */
int PortBurn_StartBurning(void *handle)
{
   PBHandle *h = (PBHandle *)handle;

   if (!h)
      return pbErrNoHandle;

   if (!h->device)
      return pbErrDeviceNotOpen;

   h->burn = DRBurnCreate(h->device);
   if (!h->burn)
      return pbErrCannotPrepareToBurn;

   h->err = DRBurnWriteLayout(h->burn, h->trackArray);
   if (h->err != noErr) {
      DRBurnAbort(h->burn);
      CFRelease(h->burn);
      h->burn = NULL;
      return pbErrCannotStartBurning;
   }

   h->frac = 0.0;

   return pbSuccess;
}

/* Cancel if burning was in progress.  It might take a while for
   this to take effect; wait until GetStatus says 1.0 to close
   the device. */
int PortBurn_CancelBurning(void *handle)
{
   PBHandle *h = (PBHandle *)handle;

   if (!h)
      return pbErrNoHandle;

   if (!h->burn)
      return pbErrNotCurrentlyBurning;

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

   if (!h)
      return pbErrNoHandle;

   if (!h->burn)
      return pbErrNotCurrentlyBurning;

   status = DRBurnCopyStatus(h->burn);
   if (!status)
      return pbErrCannotGetBurnStatus;

   trackNumRef = (CFNumberRef)
      CFDictionaryGetValue(status, kDRStatusCurrentTrackKey);
   if (trackNumRef != NULL) {
      CFNumberGetValue(trackNumRef, kCFNumberIntType, &trackNum);
   }

   fracRef = (CFNumberRef)
      CFDictionaryGetValue(status, kDRStatusPercentCompleteKey);
   if (fracRef != NULL) {
      CFNumberGetValue(fracRef, kCFNumberFloatType, &frac);
   }

   stateRef = (CFStringRef)
      CFDictionaryGetValue(status, kDRStatusStateKey);
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
      if (frac > 0.0 && frac <= 1.0)
         newFrac = frac;

      /* Scale it to the range 0.05 - 0.99 */
      newFrac = 0.05 + 0.94 * newFrac;

      /* Only use that value if it's larger than the previous value */
      if (newFrac > h->frac)
         h->frac = newFrac;
   }

   if (CFEqual(stateRef, kDRStatusStateDone)) {
      /* Returning a fraction complete of 1.0 means we're done! */
      h->frac = 1.0;
   }

   if (CFEqual(stateRef, kDRStatusStateFailed)) {
      CFRelease(status);
      return pbErrBurnFailed;
   }

   *out_fraction_complete = h->frac;

   CFRelease(status);   
   return pbSuccess;
}
