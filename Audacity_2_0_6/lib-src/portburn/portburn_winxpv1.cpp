/*
 * PortBurn
 * Windows XP IMAPI implementation
 *
 * Authors:
 *   Dominic Mazzoni, Leland Lucius
 *
 * The following MSDN page was used as a guide:
 *   http://msdn2.microsoft.com/en-us/library/aa366236.aspx
 *
 * License: LGPL
 */

#include "portburn.h"

#define _WIN32_WINNT 0x0400

#include <windows.h>
#include <tchar.h>
#include <malloc.h>
#include <imapi.h>
#include <imapierror.h>
#include <stdio.h>

#define DEBUG(a) printf a

typedef struct {

   // Used for duration of portburn open session

   IGlobalInterfaceTable      *pGit;                     // Things that must be cleaned up at close
   IDiscMaster                *pDiscMaster;              //
   DWORD                       dwDiscMaster;             //
   IRedbookDiscMaster         *pRedbookDiscMaster;       //
   IEnumDiscRecorders         *pEnumDiscRecorders;       //
   HRESULT                     hres;
   bool                        verify;
   bool                        test;
   bool                        underrun;
   bool                        eject;
   bool                        gapless;
   int                         speed;

   // Used for duration of device open session

   IDiscRecorder              *pDiscRecorder;            // Things that must be cleaned up at close
   IDiscMasterProgressEvents  *pEvents;                  //
   HANDLE                      hThread;                  //
   UINT_PTR                    pnCookie;                 //
   float                       fraction;
   bool                        erasetype;
   bool                        burning;
   bool                        erasing;
   bool                        cancel;
} PBHandlev1;

class PortBurnProgress : public IDiscMasterProgressEvents
{
 public:
   static HRESULT STDMETHODCALLTYPE
      CreateInstance(void *handle, IDiscMasterProgressEvents** ppEvents);

 private:
   PortBurnProgress(void *handle);

   PBHandlev1 *h;

 protected:
   STDMETHOD(QueryInterface)(REFIID riid, LPVOID* ppv);
   STDMETHOD_(ULONG, AddRef)(VOID);
   STDMETHOD_(ULONG, Release)(VOID);

 protected:
   STDMETHOD(QueryCancel)(boolean *pbCancel);
   STDMETHOD(NotifyPnPActivity)(VOID);
   STDMETHOD(NotifyAddProgress)(LONG nCompletedSteps, LONG nTotalSteps);
   STDMETHOD(NotifyBlockProgress)(LONG nCompleted, LONG nTotal);
   STDMETHOD(NotifyTrackProgress)(LONG nCurrentTrack, LONG nTotalTracks);
   STDMETHOD(NotifyPreparingBurn)(LONG nEstimatedSeconds);
   STDMETHOD(NotifyClosingDisc)(LONG nEstimatedSeconds);
   STDMETHOD(NotifyBurnComplete)(HRESULT status);
   STDMETHOD(NotifyEraseComplete)(HRESULT status);

 private:
   ULONG m_cRefs;
};

PortBurnProgress::PortBurnProgress(void *handle)
{
   h = (PBHandlev1 *) handle;
   m_cRefs = 0;
}

STDMETHODIMP
PortBurnProgress::CreateInstance(void *handle, IDiscMasterProgressEvents **ppEvents)
{
   if (ppEvents == NULL) {
      return E_POINTER;
   }

   PortBurnProgress *pThis = new PortBurnProgress(handle);
   if (pThis == NULL) {
      return E_OUTOFMEMORY;
   }

   return pThis->QueryInterface(IID_IDiscMasterProgressEvents,
                                (VOID**) ppEvents);
}

STDMETHODIMP
PortBurnProgress::QueryInterface(REFIID riid, LPVOID* ppv)
{
   if (ppv == NULL) {
      return E_POINTER;
   }

   *ppv = NULL;

   if ((riid != IID_IUnknown) && (riid != IID_IDiscMasterProgressEvents)) {
      return E_NOINTERFACE;
   }

   *ppv = this;
   AddRef();

   return S_OK;
}

STDMETHODIMP_(ULONG)
PortBurnProgress::AddRef(VOID)
{
   return InterlockedIncrement((LONG*) &m_cRefs);
}

STDMETHODIMP_(ULONG)
PortBurnProgress::Release(VOID)
{
   ULONG cRef = InterlockedDecrement((LONG*) &m_cRefs);

   if (cRef == 0) {
      delete this;
   }

   return cRef;
}

STDMETHODIMP_(HRESULT)
PortBurnProgress::QueryCancel(boolean *pbCancel)
{
   DEBUG(("QueryCancel() -> %d\n", h->cancel));

   if (pbCancel != NULL) {
      *pbCancel = h->cancel;
   }

   return S_OK;
}

STDMETHODIMP_(HRESULT)
PortBurnProgress::NotifyPnPActivity(VOID)
{
   DEBUG(("NotifyPnActivity\n"));

   return S_OK;
}

STDMETHODIMP_(HRESULT)
PortBurnProgress::NotifyAddProgress(LONG nCompletedSteps, LONG nTotalSteps)
{
   DEBUG(("NotifyAddProgress %d %d\n", nCompletedSteps, nTotalSteps));

   return S_OK;
}

STDMETHODIMP_(HRESULT)
PortBurnProgress::NotifyBlockProgress(LONG nCompleted, LONG nTotal)
{
   DEBUG(("NotifyBlockProgress %d %d\n", nCompleted, nTotal));

   /* This should never actually reach 1.0; we wait until the BurnComplete
      message for a 1.0 */
   h->fraction = nCompleted / (float) (nTotal + 1);

   return S_OK;
}

STDMETHODIMP_(HRESULT)
PortBurnProgress::NotifyTrackProgress(LONG nCurrentTrack, LONG nTotalTracks)
{
   DEBUG(("NotifyTrackProgress %d %d\n", nCurrentTrack, nTotalTracks));

   return S_OK;
}

STDMETHODIMP_(HRESULT)
PortBurnProgress::NotifyPreparingBurn(LONG nEstimatedSeconds)
{
   DEBUG(("NotifyPreparingBurn %d\n", nEstimatedSeconds));

   return S_OK;
}

STDMETHODIMP_(HRESULT)
PortBurnProgress::NotifyClosingDisc(LONG nEstimatedSeconds)
{
   DEBUG(("NotifyClosingDisc %d\n", nEstimatedSeconds));

   return S_OK;
}

STDMETHODIMP_(HRESULT)
PortBurnProgress::NotifyBurnComplete(HRESULT status)
{
   DEBUG(("NotifyBurnComplete %08x\n", status));

   h->fraction = 1.0;

   return S_OK;
}

STDMETHODIMP_(HRESULT)
PortBurnProgress::NotifyEraseComplete(HRESULT status)
{
   DEBUG(("NotifyEraseComplete %08x\n", status));

   h->fraction = 1.0;

   return S_OK;
}

/* Recording Thread */
static DWORD WINAPI PortBurn_v1_RecordDisc(LPVOID lpParam)
{
   PBHandlev1 *h = (PBHandlev1 *)lpParam;
   IDiscMaster *pDiscMaster;

   h->hres = CoInitializeEx(0, COINIT_APARTMENTTHREADED);
   if (SUCCEEDED(h->hres)) {
      h->hres = h->pGit->GetInterfaceFromGlobal(h->dwDiscMaster,
                                                IID_IDiscMaster,
                                               (void**) &pDiscMaster);
      if (SUCCEEDED(h->hres)) {
         h->cancel = false;
         h->fraction = 0.0;

         h->hres = pDiscMaster->RecordDisc(h->test,
                                           h->eject);

         pDiscMaster->Release();
      }

      CoUninitialize();
   }

   h->fraction = 1.0;

   if (FAILED(h->hres)) {
      return pbErrBurnFailed;
   }

   return pbSuccess;
}

/* Erasing Thread */
static DWORD WINAPI PortBurn_v1_EraseDisc(LPVOID lpParam)
{
   PBHandlev1 *h = (PBHandlev1 *)lpParam;
   IDiscMaster *pDiscMaster;
   IDiscRecorder *pDiscRecorder;

   h->hres = CoInitializeEx(0, COINIT_APARTMENTTHREADED);
   if (SUCCEEDED(h->hres)) {
      h->hres = h->pGit->GetInterfaceFromGlobal(h->dwDiscMaster,
                                                IID_IDiscMaster,
                                               (void**) &pDiscMaster);
      if (SUCCEEDED(h->hres)) {
         h->hres = pDiscMaster->GetActiveDiscRecorder(&pDiscRecorder);
         if (SUCCEEDED(h->hres)) {
            h->cancel = false;
            h->fraction = 0.99f; // No erase progress in v1

            h->hres = pDiscRecorder->OpenExclusive();
            if (SUCCEEDED(h->hres)) {
               h->hres = pDiscRecorder->Erase(h->erasetype);
               pDiscRecorder->Close();
            }

            pDiscRecorder->Release();
         }
      }

      pDiscMaster->Release();

      CoUninitialize();
   }

   h->fraction = 1.0;

   if (FAILED(h->hres)) {
      return pbErrEraseFailed;
   }

   return pbSuccess;
}

/* */
static HRESULT MasterTerm(PBHandlev1 *h)
{
   if (h->pEnumDiscRecorders != NULL) {
      h->pEnumDiscRecorders->Release();
      h->pEnumDiscRecorders = NULL;
   }

   if (h->pRedbookDiscMaster != NULL) {
      h->pRedbookDiscMaster->Release();
      h->pRedbookDiscMaster = NULL;
   }

   h->pDiscMaster->Close();

   return h->hres;
}

/* */
static HRESULT MasterInit(PBHandlev1 *h)
{
   if (h->pRedbookDiscMaster != NULL) {
      MasterTerm(h);
   }

   h->hres = h->pDiscMaster->Open();
   if (FAILED(h->hres)) {
      return S_OK;
   }

   h->hres = h->pDiscMaster->
               SetActiveDiscMasterFormat(IID_IRedbookDiscMaster,
                                         (void **)&h->pRedbookDiscMaster);

   if (FAILED(h->hres)) {
      return MasterTerm(h);
   }

   h->hres = h->pDiscMaster->EnumDiscRecorders(&h->pEnumDiscRecorders);
   if (FAILED(h->hres)) {
      return MasterTerm(h);
   }

   if (h->pDiscRecorder != NULL) {
      h->hres = h->pDiscMaster->SetActiveDiscRecorder(h->pDiscRecorder);
      if (FAILED(h->hres)) {
         return MasterTerm(h);
      }
   }

   return h->hres;
}
void PortBurn_v1_Close(void *handle);
void *PortBurn_v1_Open()
{   
   PBHandlev1 *h;

#if defined(_DEBUG)
AllocConsole();
freopen("CONOUT$", "w", stdout);
#endif

   h = (PBHandlev1 *) HeapAlloc(GetProcessHeap(),
                              HEAP_ZERO_MEMORY,
                              sizeof(PBHandlev1));
   if (h == NULL) {
      return NULL;
   }

   h->dwDiscMaster = -1;

   h->test = pbTestDefault;
   h->verify = pbVerifyDefault;
   h->underrun = pbUnderrunDefault;
   h->eject = pbEjectDefault;
   h->gapless = pbGaplessDefault;
   h->speed = pbSpeedDefault;

   h->hres = CoCreateInstance(CLSID_StdGlobalInterfaceTable,
                              NULL,
                              CLSCTX_INPROC_SERVER,
                              IID_PPV_ARGS(&h->pGit));
   if (FAILED(h->hres)) {
      PortBurn_v1_Close(h);
      return NULL;
   }

   h->hres = CoCreateInstance(__uuidof(MSDiscMasterObj),
                              NULL,
                              CLSCTX_ALL,
                              IID_PPV_ARGS(&h->pDiscMaster));
   if (FAILED(h->hres)) {
      PortBurn_v1_Close(h);
      return NULL;
   }

   h->hres = h->pGit->RegisterInterfaceInGlobal(h->pDiscMaster,
                                                IID_IDiscMaster,
                                                &h->dwDiscMaster);
   if (FAILED(h->hres)) {
      PortBurn_v1_Close(h);
      return NULL;
   }

   h->hres = MasterInit(h);
   if (FAILED(h->hres)) {
      PortBurn_v1_Close(h);
      return NULL;
   }

   return h;
}

/* Cleanup */
void PortBurn_v1_Close(void *handle)
{
   PBHandlev1 *h = (PBHandlev1 *) handle;

   if (h == NULL) {
      return;
   }

   MasterTerm(h);

   if (h->dwDiscMaster != -1) {
      h->pGit->RevokeInterfaceFromGlobal(h->dwDiscMaster);
   }

   if (h->pDiscMaster != NULL) {
      h->pDiscMaster->Release();
   }

   if (h->pGit != NULL) {
      h->pGit->Release();
   }

   HeapFree(GetProcessHeap(), 0, h);
}

/* Return a human-readable error string for the last operating system
   specific error (NOT human readable strings for the PortBurn error
   codes).  Caller should dispose of the returned string using free(). */
char *PortBurn_v1_LastError(void *handle)
{
   PBHandlev1 *h = (PBHandlev1 *) handle;
   HRESULT hr;
   LPTSTR windowsErrorString = NULL;
   char *errorString = NULL;
   int len;

   if (h == NULL) {
      return NULL;
   }

   /* Have Windows allocate a buffer for us and format the error
      message in windowsErrorString */
   hr = h->hres;
   if (HRESULT_FACILITY(hr) == FACILITY_WINDOWS) {
      hr = HRESULT_CODE(hr);
   }

   len = FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
                       NULL, hr, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                       (LPTSTR) &windowsErrorString, 0, NULL);
   if (windowsErrorString == NULL) {
      windowsErrorString = (LPTSTR) LocalAlloc(LPTR, 128 * sizeof(TCHAR));
      if (windowsErrorString == NULL) {
         return NULL;
      }

      len = _stprintf_s(windowsErrorString, 128, L"HRESULT = %08x", hr);
   }

   /* Convert the string */
   errorString = (char *) malloc(len + 1);
   if (errorString != NULL) {
      errorString[0] = '\0';
      WideCharToMultiByte(CP_ACP, 0, windowsErrorString, len, errorString, len, NULL, NULL);
      errorString[len] = '\0';
   }

   LocalFree(windowsErrorString);

   return errorString;
}

/* Get the number of devices capable of burning audio CDs.
   If the result is N, then calls to GetDeviceName and OpenDevice
   are guaranteed to be valid for indices from 0 up to N-1, until
   the next time you call GetNumDevices.  At that point, the list of
   devices will be rescanned, and may be different. */
int PortBurn_v1_GetNumDevices(void *handle)
{
   PBHandlev1 *h = (PBHandlev1 *) handle;
   IDiscRecorder *pDiscRecorder;
   ULONG fetched;
   int count = 0;

   if (h == NULL) {
      return 0;
   }

   if (h->pEnumDiscRecorders != NULL) {
      h->pEnumDiscRecorders->Release();
      h->pEnumDiscRecorders = NULL;
   }

   h->hres = h->pDiscMaster->EnumDiscRecorders(&h->pEnumDiscRecorders);
   if (FAILED(h->hres)) {
      return 0;
   }

   while (h->pEnumDiscRecorders->Next(1, &pDiscRecorder, &fetched) == S_OK) {
      pDiscRecorder->Release();
      count++;
   }

   return count;
}

/* Get the name of the device with a given index.  Only valid
   after a call to GetNumDevices. */
char *PortBurn_v1_GetDeviceName(void *handle, int index)
{
   PBHandlev1 *h = (PBHandlev1 *) handle;
   IDiscRecorder *pDiscRecorder;
   BSTR bVendor = NULL;
   BSTR bProduct = NULL;
   BSTR bRevision = NULL;
   ULONG fetched;
   TCHAR *wname;
   char *name;
   int len;

   if (h == NULL) {
      return NULL;
   }

   h->hres = S_OK;

   if (index < 0 || index >= PortBurn_v1_GetNumDevices(h)) {
      h->hres = E_INVALIDARG;
      return NULL;
   }

   h->pEnumDiscRecorders->Reset();
   if (index != 0) {
      h->pEnumDiscRecorders->Skip(index - 1);
   }

   h->hres = h->pEnumDiscRecorders->Next(1, &pDiscRecorder, &fetched);
   if (FAILED(h->hres)) {
      return NULL;
   }

   h->hres = pDiscRecorder->GetDisplayNames(&bVendor, &bProduct, &bRevision);

   pDiscRecorder->Release();

   if (FAILED(h->hres)) {
      return NULL;
   }

   len = SysStringLen(bVendor) + 1 +
         SysStringLen(bProduct) + 1 +
         SysStringLen(bRevision);

   wname = (LPWSTR)alloca((len + 1) * sizeof(TCHAR));

   _stprintf_s(wname,
               (len + 1),
               _T("%s %s %s"),
               (LPCTSTR) bVendor,
               (LPCTSTR) bProduct,
               (LPCTSTR) bRevision);

   SysFreeString(bVendor);
   SysFreeString(bProduct);
   SysFreeString(bRevision);

   name = (char *) malloc(len + 1);
   if (name == NULL) {
      return NULL;
   }

   name[0] = '\0';
   WideCharToMultiByte(CP_ACP, 0, wname, len, name, len, NULL, NULL);
   name[len] = '\0';

   return name;
}

/* Open a particular device by index number.  Returns 0 on success;
   any nonzero value indicates an error, for example if the device is
   already open by some other program. */
int PortBurn_v1_OpenDevice(void *handle, int index)
{
   PBHandlev1 *h = (PBHandlev1 *) handle;
   ULONG fetched;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   h->hres = S_OK;

   if (h->pDiscRecorder != NULL) {
      h->hres = E_POINTER;
      return pbErrDeviceAlreadyOpen;
   }

   if (index < 0 || index >= PortBurn_v1_GetNumDevices(h)) {
      h->hres = E_INVALIDARG;
      return pbErrInvalidDeviceIndex;
   }

   h->pEnumDiscRecorders->Reset();
   if (index != 0) {
      h->pEnumDiscRecorders->Skip(index - 1);
   }

   h->hres = h->pEnumDiscRecorders->Next(1, &h->pDiscRecorder, &fetched);
   if (FAILED(h->hres)) {
      return pbErrCannotAccessDevice;
   }

   h->hres = h->pDiscMaster->SetActiveDiscRecorder(h->pDiscRecorder);
   if (FAILED(h->hres)) {
      h->pDiscRecorder->Release();
      h->pDiscRecorder = NULL;
      return pbErrCannotReserveDevice;
   }

   return pbSuccess;
}

/* Close a device */
int PortBurn_v1_CloseDevice(void *handle)
{
   PBHandlev1 *h = (PBHandlev1 *) handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   h->hres = S_OK;

   if (h->hThread != NULL) {
      h->cancel = true;
      WaitForSingleObject(h->hThread, INFINITE);
      CloseHandle(h->hThread);
      h->hThread = NULL;
   }

   if (h->pnCookie != NULL) {
      h->pDiscMaster->ProgressUnadvise(h->pnCookie);
      h->pnCookie = NULL;
   }

   if (h->pEvents != NULL) {
      h->pEvents->Release();
      h->pEvents = NULL;
   }

   if (h->pDiscRecorder != NULL) {
      h->pDiscRecorder->Close();
      h->pDiscRecorder->Release();
      h->pDiscRecorder = NULL;
   }

   h->burning = false;
   h->erasing = false;
   h->cancel = false;
   h->fraction = 0.0;

   h->test = pbTestDefault;
   h->verify = pbVerifyDefault;
   h->underrun = pbUnderrunDefault;
   h->eject = pbEjectDefault;
   h->gapless = pbGaplessDefault;
   h->speed = pbSpeedDefault;

   return pbSuccess;
}

/* Eject the media in the currently opened device */
int PortBurn_v1_EjectDevice(void *handle)
{
   PBHandlev1 *h = (PBHandlev1 *) handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   h->hres = S_OK;

   if (h->pDiscRecorder == NULL) {
      return pbErrDeviceNotOpen;
   }

   h->hres = h->pDiscRecorder->OpenExclusive();
   if (FAILED(h->hres)) {
      return pbErrCannotReserveDevice;
   }

   h->hres = h->pDiscRecorder->Eject();

   h->pDiscRecorder->Close();

   if (FAILED(h->hres)) {
      return pbErrCannotEject;
   }

   return pbSuccess;
}

/* This indicates you're ready to start staging audio data for the
   currently opened device.  At this point you are committing to
   exclusive access to the CD burner, and this is the function that
   will fail if another program is using the device, or if there is
   no writable CD media in the device at this point. */
int PortBurn_v1_StartStaging(void *handle, const char *tmpdir)
{
   PBHandlev1 *h = (PBHandlev1 *) handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   h->hres = S_OK;

   if (h->pDiscRecorder == NULL) {
      return pbErrDeviceNotOpen;
   }

   if (h->pEvents != NULL) {
      return pbErrAlreadyStagingOrBurning;
   }

   h->hres = PortBurnProgress::CreateInstance(h, &h->pEvents);
   if (FAILED(h->hres)) {
      return pbErrCannotPrepareToBurn;
   }

   h->hres = h->pDiscMaster->ProgressAdvise(h->pEvents,
                                            &h->pnCookie);
   if (FAILED(h->hres)) {
      h->pEvents->Release();
      h->pEvents = NULL;
      return pbErrCannotPrepareToBurn;
   }

   return pbSuccess;
}

/* Start a new audio track.  Pass the name of the track, and the
   length in CD Audio frames (each frame is 1/75.0 of a second, exactly). */
int PortBurn_v1_StartTrack(void *handle, const char *name)
{
   PBHandlev1 *h = (PBHandlev1 *) handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   h->hres = S_OK;
 
   if (h->pDiscRecorder == NULL) {
      return pbErrDeviceNotOpen;
   }

   // this fails if the disc isn't writable...
   h->hres = h->pRedbookDiscMaster->CreateAudioTrack(0);
   if (FAILED(h->hres)) {
      return pbErrCannotStageTrack;
   }

   return pbSuccess;
}

/* Add one frame of audio to the current track.  The buffer must be exactly
   1176 elements long, containing interleaved left and right audio samples.
   The values should be signed 16-bit numbers in the native endianness of
   this computer. */
int PortBurn_v1_AddFrame(void *handle, short *buffer)
{
   PBHandlev1 *h = (PBHandlev1 *) handle;
   int oneBlockByteCount = 1176 * 2;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   h->hres = S_OK;

   if (h->pDiscRecorder == NULL) {
      return pbErrDeviceNotOpen;
   }

   h->hres = h->pRedbookDiscMaster->AddAudioTrackBlocks((byte *)buffer,
                                                         oneBlockByteCount);
   if (FAILED(h->hres)) {
      return pbErrCannotWriteToStagingFile;
   }

   return pbSuccess;
}

/* Finish the current audio track. */
int PortBurn_v1_EndTrack(void *handle)
{
   PBHandlev1 *h = (PBHandlev1 *) handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   h->hres = S_OK;

   if (h->pDiscRecorder == NULL) {
      return pbErrDeviceNotOpen;
   }

   h->hres = h->pRedbookDiscMaster->CloseAudioTrack();
   if (FAILED(h->hres)) {
      return pbErrCannotUseStagedFileForBurning;
   }

   return pbSuccess;
}

/* Begin burning the disc. */
int PortBurn_v1_StartBurning(void *handle)
{
   PBHandlev1 *h = (PBHandlev1 *) handle;
   IPropertyStorage *prop;
   PROPSPEC    spec[30];
   PROPVARIANT var[30];
   DWORD dwID;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   h->hres = S_OK;

   if (h->pDiscRecorder == NULL) {
      return pbErrDeviceNotOpen;
   }

   h->hres = h->pDiscRecorder->GetRecorderProperties(&prop);
   if (FAILED(h->hres)) {
      return pbErrCannotStartBurning;
   }

   spec[0].ulKind = PRSPEC_LPWSTR;
   spec[0].lpwstr = L"EnableBufferUnderrunFree";
   var[0].vt      = VT_I4;
   var[0].lVal    = h->underrun;

   spec[1].ulKind = PRSPEC_LPWSTR;
   spec[1].lpwstr = L"AudioGapSize";
   var[1].vt      = VT_I4;
   var[1].lVal    = h->gapless ? 0 : 150;

   spec[2].ulKind = PRSPEC_LPWSTR;
   spec[2].lpwstr = L"WriteSpeed";
   var[2].vt      = VT_I4;
   var[2].lVal    = h->speed;

   h->hres = prop->WriteMultiple(3, spec, var, PID_FIRST_USABLE);

   prop->Release();

   if (FAILED(h->hres)) {
      return pbErrCannotStartBurning;
   }

   h->burning = true;

   h->hThread = CreateThread(NULL,
                             0,
                             PortBurn_v1_RecordDisc,
                             h,
                             0,
                             &dwID);
   if (h->hThread == NULL) {
      h->burning = false;
      return pbErrCannotStartBurning;
   }

   return pbSuccess;
}

/* Cancel if burning was in progress.  It might take a while for
   this to take effect; wait until GetStatus says 1.0 to close
   the device. */
int PortBurn_v1_CancelBurning(void *handle)
{
   PBHandlev1 *h = (PBHandlev1 *) handle;
   float frac = 0.0;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   h->hres = S_OK;

   if (h->pDiscRecorder == NULL) {
      return pbErrDeviceNotOpen;
   }

   if (h->burning == false) {
      return pbErrNotCurrentlyBurning;
   }

   h->cancel = true;

   return pbSuccess;
}

/* During burning, returns the fraction complete in the given
   float pointer, from 0.0 to 1.0.  If this function returns
   nonzero, the disc burning has failed and should be aborted.
   If *out_fraction_complete is equal to 1.0, the burning is done;
   you can call PortBurn_CloseDevice.
*/
int PortBurn_v1_GetStatus(void *handle, float *out_fraction_complete)
{
   PBHandlev1 *h = (PBHandlev1 *) handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   h->hres = S_OK;

   if (h->pDiscRecorder == NULL) {
      return pbErrDeviceNotOpen;
   }

   if (h->burning == false) {
      return pbErrNotCurrentlyBurning;
   }

   MSG msg;
   if (PeekMessage(&msg, NULL, NULL, NULL, PM_REMOVE)) {
      TranslateMessage(&msg);
      DispatchMessage(&msg);
   }

   WaitForSingleObject(h->hThread, 100);

   *out_fraction_complete = h->fraction;

   if (h->fraction == 1.0) {
      h->burning = false;
      h->cancel = true;
      WaitForSingleObject(h->hThread, INFINITE);
      CloseHandle(h->hThread);
      h->hThread = NULL;
   }

   return pbSuccess;
}

/* Get option value. */
int PortBurn_v1_GetOption(void *handle, int option, int *value)
{
   PBHandlev1 *h = (PBHandlev1 *) handle;
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

int PortBurn_v1_SetOption(void *handle, int option, int value)
{
   PBHandlev1 *h = (PBHandlev1 *) handle;
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
         h->gapless = value != 0;
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

/* Erase the media in the currently opened device */
int PortBurn_v1_StartErasing(void *handle, int type)
{
   PBHandlev1 *h = (PBHandlev1 *) handle;
   DWORD dwID;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   h->hres = S_OK;

   if (h->pDiscRecorder == NULL) {
      return pbErrDeviceNotOpen;
   }

   if (h->hThread != NULL) {
      return pbErrCannotStartErasing;
   }

   h->erasetype = (type == pbEraseQuick ? false : true);

   h->erasing = true;

   h->hThread = CreateThread(NULL,
                             0,
                             PortBurn_v1_EraseDisc,
                             h,
                             0,
                             &dwID);
   if (h->hThread == NULL) {
      h->erasing = false;
      return pbErrCannotStartErasing;
   }

   return pbSuccess;
}

int PortBurn_v1_GetEraseStatus(void *handle, float *out_fraction_complete)
{
   PBHandlev1 *h = (PBHandlev1 *) handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   h->hres = S_OK;

   if (h->pDiscRecorder == NULL) {
      return pbErrDeviceNotOpen;
   }

   if (h->erasing == false) {
      return pbErrNotCurrentlyErasing;
   }

   WaitForSingleObject(h->hThread, 100);

   *out_fraction_complete = h->fraction;

   if (h->fraction == 1.0) {
      h->erasing = false;
      WaitForSingleObject(h->hThread, INFINITE);
      CloseHandle(h->hThread);
      h->hThread = NULL;

      h->hres = MasterInit(h);
      if (FAILED(h->hres)) {
         return pbErrEraseFailed;
      }
   }

   return pbSuccess;
}

/* */
int PortBurn_v1_GetMediaState(void *handle, int *state)
{
   PBHandlev1 *h = (PBHandlev1 *) handle;
   BYTE sessions;
   BYTE last;
   ULONG start;
   ULONG next;
   ULONG free;
   LONG type;
   LONG flags;
   int mstate;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   h->hres = S_OK;

   if (h->pDiscRecorder == NULL) {
      return pbErrDeviceNotOpen;
   }

   h->hres = h->pDiscRecorder->OpenExclusive();
   if (FAILED(h->hres)) {
      return pbErrCannotReserveDevice;
   }

   // Only doing this to detect empty drive
   h->hres = h->pDiscRecorder->QueryMediaInfo(&sessions, &last, &start, &next, &free);
   if (h->hres == IMAPI_E_MEDIUM_NOTPRESENT) {
      h->pDiscRecorder->Close();
      *state = pbMediaNone;
      return pbSuccess;
   } 

   h->hres = h->pDiscRecorder->QueryMediaType(&type, &flags);

   h->pDiscRecorder->Close();

   if (h->hres == IMAPI_E_MEDIUM_NOTPRESENT) {
      *state = pbMediaNone;
      return pbSuccess;
   } 
   else if (FAILED(h->hres)) {
      return pbErrCannotAccessDevice;
   }

   mstate = 0;

   if (flags & MEDIA_BLANK) {
      mstate |= pbMediaBlank;
   }

   if (flags & MEDIA_RW) {
      mstate |= pbMediaErasable;
   }

   if (flags & MEDIA_WRITABLE) {
      mstate |= pbMediaAppendable;
      mstate |= pbMediaOverwritable;
   }

   *state = mstate;

   return pbSuccess;
}

int PortBurn_v1_GetSupportedSpeeds(void *handle, int *cnt, int *speeds[])
{
   *cnt = 0;
   *speeds = NULL;
   return pbErrNoHandle;
}
