/*
 * PortBurn
 * Windows XP IMAPI implementation
 *
 * Authors:
 *   Leland Lucius
 *
 * The following MSDN page was used as a guide:
 *   http://msdn2.microsoft.com/en-us/library/aa366236.aspx
 *
 * License: LGPL
 */

#include "portburn.h"

#define _WIN32_WINNT 0x0400

#include <windows.h>
#include <imapi.h>
#include <imapi2.h>

typedef struct {
   int   ver;
   void  *realhandle;
} PBHandle;

#define PBFUNC(r,f)              \
   extern r PortBurn_v1_ ## f;   \
   extern r PortBurn_v2_ ## f;

PBFUNC(void *, Open());
PBFUNC(void, Close(void *handle));
PBFUNC(char *, LastError(void *handle));
PBFUNC(int, GetNumDevices(void *handle));
PBFUNC(char *, GetDeviceName(void *handle, int index));
PBFUNC(int, OpenDevice(void *handle, int index));
PBFUNC(int, CloseDevice(void *handle));
PBFUNC(int, EjectDevice(void *handle));
PBFUNC(int, StartErasing(void *handle, int type));
PBFUNC(int, GetEraseStatus(void *handle, float *out_fraction_complete));
PBFUNC(int, StartStaging(void *handle, const char *tmpdir));
PBFUNC(int, StartTrack(void *handle, const char *name));
PBFUNC(int, AddFrame(void *handle, short *buffer));
PBFUNC(int, EndTrack(void *handle));
PBFUNC(int, StartBurning(void *handle));
PBFUNC(int, CancelBurning(void *handle));
PBFUNC(int, GetStatus(void *handle, float *out_fraction_complete));
PBFUNC(int, GetOption(void *handle, int option, int *value));
PBFUNC(int, SetOption(void *handle, int option, int value));
PBFUNC(int, GetSupportedSpeeds(void *handle, int *cnt, int *option[]));
PBFUNC(int, GetMediaState(void *handle, int *state));

enum {
   IMAPI_V1,
   IMAPI_V2,
   IMAPI_VERS
};

struct
{
   void *(*Open)();
   void (*Close)(void *handle);
   char *(*LastError)(void *handle);
   int (*GetNumDevices)(void *handle);
   char *(*GetDeviceName)(void *handle, int index);
   int (*OpenDevice)(void *handle, int index);
   int (*CloseDevice)(void *handle);
   int (*EjectDevice)(void *handle);
   int (*StartErasing)(void *handle, int type);
   int (*GetEraseStatus)(void *handle, float *out_fraction_complete);
   int (*StartStaging)(void *handle, const char *tmpdir);
   int (*StartTrack)(void *handle, const char *name);
   int (*AddFrame)(void *handle, short *buffer);
   int (*EndTrack)(void *handle);
   int (*StartBurning)(void *handle);
   int (*CancelBurning)(void *handle);
   int (*GetStatus)(void *handle, float *out_fraction_complete);
   int (*GetOption)(void *handle, int option, int *value);
   int (*SetOption)(void *handle, int option, int value);
   int (*GetSupportedSpeeds)(void *handle, int *cnt, int *option[]);
   int (*GetMediaState)(void *handle, int *state);
}
vectors[IMAPI_VERS] =
{
   {
      PortBurn_v1_Open,
      PortBurn_v1_Close,
      PortBurn_v1_LastError,
      PortBurn_v1_GetNumDevices,
      PortBurn_v1_GetDeviceName,
      PortBurn_v1_OpenDevice,
      PortBurn_v1_CloseDevice,
      PortBurn_v1_EjectDevice,
      PortBurn_v1_StartErasing,
      PortBurn_v1_GetEraseStatus,
      PortBurn_v1_StartStaging,
      PortBurn_v1_StartTrack,
      PortBurn_v1_AddFrame,
      PortBurn_v1_EndTrack,
      PortBurn_v1_StartBurning,
      PortBurn_v1_CancelBurning,
      PortBurn_v1_GetStatus,
      PortBurn_v1_GetOption,
      PortBurn_v1_SetOption,
      PortBurn_v1_GetSupportedSpeeds,
      PortBurn_v1_GetMediaState,
   },
   {
      PortBurn_v2_Open,
      PortBurn_v2_Close,
      PortBurn_v2_LastError,
      PortBurn_v2_GetNumDevices,
      PortBurn_v2_GetDeviceName,
      PortBurn_v2_OpenDevice,
      PortBurn_v2_CloseDevice,
      PortBurn_v2_EjectDevice,
      PortBurn_v2_StartErasing,
      PortBurn_v2_GetEraseStatus,
      PortBurn_v2_StartStaging,
      PortBurn_v2_StartTrack,
      PortBurn_v2_AddFrame,
      PortBurn_v2_EndTrack,
      PortBurn_v2_StartBurning,
      PortBurn_v2_CancelBurning,
      PortBurn_v2_GetStatus,
      PortBurn_v2_GetOption,
      PortBurn_v2_SetOption,
      PortBurn_v2_GetSupportedSpeeds,
      PortBurn_v2_GetMediaState,
   }
};
   
void *PortBurn_Open()
{   
   PBHandle *h;
   HRESULT hres;
   IDiscMaster *pV1;
   IDiscMaster2 *pV2;

   hres = CoInitializeEx(0, COINIT_APARTMENTTHREADED);
   if (FAILED(hres)) {
      return NULL;
   }

   h = (PBHandle *) HeapAlloc(GetProcessHeap(),
                              HEAP_ZERO_MEMORY,
                              sizeof(PBHandle));
   if (h == NULL) {
      CoUninitialize();
      return NULL;
   }

   hres = CoCreateInstance(__uuidof(MsftDiscMaster2),
                           NULL,
                           CLSCTX_ALL,
                           IID_IDiscMaster2,
                           (void **)&pV2);

   if (FAILED(hres)) {
      hres = CoCreateInstance(__uuidof(MSDiscMasterObj),
                              NULL,
                              CLSCTX_ALL,
                              IID_IDiscMaster,
                              (void **)&pV1);
      if (FAILED(hres)) {
         HeapFree(GetProcessHeap(), 0, h);
         CoUninitialize();
         return NULL;
      }
      pV1->Release();
      h->ver = IMAPI_V1;
   }
   else {
      pV2->Release();
      h->ver = IMAPI_V2;
   }

//h->ver = IMAPI_V1;

   h->realhandle = vectors[h->ver].Open();
   if (h->realhandle == NULL) {
         HeapFree(GetProcessHeap(), 0, h);
         CoUninitialize();
         return NULL;
   }

   return h;
}

/* Cleanup */
void PortBurn_Close(void *handle)
{
   PBHandle *h = (PBHandle *) handle;

   if (h == NULL) {
      return;
   }

   vectors[h->ver].Close(h->realhandle);

   HeapFree(GetProcessHeap(), 0, h);

   CoUninitialize();
}

char *PortBurn_LastError(void *handle)
{
   PBHandle *h = (PBHandle *) handle;

   if (h == NULL) {
      return NULL;
   }

   return vectors[h->ver].LastError(h->realhandle);
}

int PortBurn_GetNumDevices(void *handle)
{
   PBHandle *h = (PBHandle *) handle;

   if (h == NULL) {
      return 0;
   }

   return vectors[h->ver].GetNumDevices(h->realhandle);
}

/* Get the name of the device with a given index.  Only valid
   after a call to GetNumDevices. */
char *PortBurn_GetDeviceName(void *handle, int index)
{
   PBHandle *h = (PBHandle *) handle;

   if (h == NULL) {
      return NULL;
   }

   return vectors[h->ver].GetDeviceName(h->realhandle, index);
}

int PortBurn_OpenDevice(void *handle, int index)
{
   PBHandle *h = (PBHandle *) handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   return vectors[h->ver].OpenDevice(h->realhandle, index);
}

int PortBurn_CloseDevice(void *handle)
{
   PBHandle *h = (PBHandle *) handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   return vectors[h->ver].CloseDevice(h->realhandle);
}

int PortBurn_EjectDevice(void *handle)
{
   PBHandle *h = (PBHandle *) handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   return vectors[h->ver].EjectDevice(h->realhandle);
}

int PortBurn_StartStaging(void *handle, const char *tmpdir)
{
   PBHandle *h = (PBHandle *) handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   return vectors[h->ver].StartStaging(h->realhandle, tmpdir);
}

int PortBurn_StartTrack(void *handle, const char *name)
{
   PBHandle *h = (PBHandle *) handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   return vectors[h->ver].StartTrack(h->realhandle, name);
}

int PortBurn_AddFrame(void *handle, short *buffer)
{
   PBHandle *h = (PBHandle *) handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   return vectors[h->ver].AddFrame(h->realhandle, buffer);
}

int PortBurn_EndTrack(void *handle)
{
   PBHandle *h = (PBHandle *) handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   return vectors[h->ver].EndTrack(h->realhandle);
}

int PortBurn_StartBurning(void *handle)
{
   PBHandle *h = (PBHandle *) handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   return vectors[h->ver].StartBurning(h->realhandle);
}

int PortBurn_CancelBurning(void *handle)
{
   PBHandle *h = (PBHandle *) handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   return vectors[h->ver].CancelBurning(h->realhandle);
}

int PortBurn_GetStatus(void *handle, float *out_fraction_complete)
{
   PBHandle *h = (PBHandle *) handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   return vectors[h->ver].GetStatus(h->realhandle, out_fraction_complete);
}

int PortBurn_GetOption(void *handle, int option, int *value)
{
   PBHandle *h = (PBHandle *) handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   return vectors[h->ver].GetOption(h->realhandle, option, value);
}

int PortBurn_SetOption(void *handle, int option, int value)
{
   PBHandle *h = (PBHandle *) handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   return vectors[h->ver].SetOption(h->realhandle, option, value);
}

int PortBurn_GetSpeeds(void *handle, int *cnt, int *speeds[])
{
   return -1;
}

/* Erase the media in the currently opened device */
int PortBurn_StartErasing(void *handle, int type)
{
   PBHandle *h = (PBHandle *) handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   return vectors[h->ver].StartErasing(h->realhandle, type);
}

int PortBurn_GetEraseStatus(void *handle, float *out_fraction_complete)
{
   PBHandle *h = (PBHandle *) handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   return vectors[h->ver].GetEraseStatus(h->realhandle, out_fraction_complete);
}

/* */
int PortBurn_GetMediaState(void *handle, int *state)
{
   PBHandle *h = (PBHandle *) handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   return vectors[h->ver].GetMediaState(h->realhandle, state);
}
