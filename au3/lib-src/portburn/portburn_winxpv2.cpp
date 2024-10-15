/*
 * PortBurn
 * Windows XP IMAPI v2 implementation
 *
 * Authors:
 *   Leland Lucius
 *
 * License: LGPL
 */

#include "portburn.h"

#define _WIN32_WINNT 0x0400

#include <windows.h>
#include <tchar.h>
#include <malloc.h>
#include <imapi2.h>
#include <imapi2error.h>
#include <Objidl.h>
#include <ObjBase.h>
#include <stdio.h>

#define DEBUG(a) printf a

typedef struct {

   // Used for duration of portburn open session

   BSTR                        client;       // Things that must be cleaned up at close

   HRESULT                     hres;
   bool                        verify;
   bool                        test;
   bool                        underrun;
   bool                        eject;
   bool                        gapless;
   int                         speed;

   // Used for duration of device open session

   IStorage                   *pStorage;     //
   IStream                    *pMarshall;    //
   IStream                    *pStream;      // Things that must be cleaned up at close
   HANDLE                      hThread;      //
   BSTR                        diskid;       //

   float                       fraction;
   int                         curtrack;
   int                         threadres;
   bool                        burning;
   bool                        erasing;
   bool                        cancel;
   VARIANT_BOOL                fullerase;
} PBHandlev2;

/// ==================================================================
/// ==================================================================
/// ==================================================================
/// ==================================================================
/// ==================================================================

class WriteEventsTAO:public DDiscFormat2TrackAtOnceEvents
{
public:
   WriteEventsTAO(PBHandlev2 *h)
   {
      LPTYPELIB pTypeLib;
      HRESULT hres;

      mH = h;
      mRefs = 0;
      mCookie = -1;
      mConnectionPoint = NULL;
      mConnectionPointContainer = NULL;
      mTypeInfo = NULL;
      mAdjust = 0;

      hres = LoadRegTypeLib(LIBID_IMAPILib2,                                                                    
                            IMAPILib2_MajorVersion,
                            IMAPILib2_MinorVersion,                                                         
                            LOCALE_SYSTEM_DEFAULT,
                            &pTypeLib);                                                                         
      if (FAILED(hres)) {
         return;
      }

      hres = pTypeLib->GetTypeInfoOfGuid(__uuidof(DDiscFormat2TrackAtOnceEvents), &mTypeInfo);

      pTypeLib->Release();

      if (FAILED(hres)) {
         return;
      }

      AddRef();
   }

   ~WriteEventsTAO()
   {
      Disconnect();

      if (mTypeInfo != NULL) {
         mTypeInfo->Release();
      }
   }

   void Connect(IUnknown *pConnectTo)
   {
      HRESULT hres;

      hres = pConnectTo->QueryInterface(__uuidof(IConnectionPointContainer),
                                        (void **)&mConnectionPointContainer);
      if (FAILED(hres)) {
         return;
      }

      hres = mConnectionPointContainer->FindConnectionPoint(__uuidof(DDiscFormat2TrackAtOnceEvents),
                                                            &mConnectionPoint);
      if (FAILED(hres)) {
         Disconnect();
         return;
      }

      hres = mConnectionPoint->Advise((IUnknown*)this, &mCookie);                                                
      if (FAILED(hres)) {
         Disconnect();
         return;
      }
   }

   void Disconnect()
   {
      if (mCookie != -1) {
         mConnectionPoint->Unadvise(mCookie);
         mCookie = -1;
      }

      if (mConnectionPoint != NULL) {
         mConnectionPoint->Release();
         mConnectionPoint = NULL;
      }

      if (mConnectionPointContainer != NULL) {
         mConnectionPointContainer->Release();
         mConnectionPointContainer = NULL;
      }
   }

   // IUnknown

   STDMETHODIMP QueryInterface(REFIID riid, LPVOID* ppv)
   {
      if (ppv == NULL) {
         return E_POINTER;
      }

      *ppv = NULL;

      if (riid == IID_IUnknown || riid == IID_IDispatch || riid == IID_DDiscFormat2TrackAtOnceEvents) {
         *ppv = this;
         AddRef();
         return S_OK;
      }

      return E_NOINTERFACE;
   }

   STDMETHODIMP_(ULONG) AddRef(VOID)
   {
      return InterlockedIncrement((LONG*) &mRefs);
   }

   STDMETHODIMP_(ULONG) Release(VOID)
   {
      ULONG ref = InterlockedDecrement((LONG*) &mRefs);

      if (ref == 0) {
         delete this;
      }

      return ref;
   }

   // IDispatch

   STDMETHODIMP GetTypeInfoCount(UINT *pctinfo)
   {
      *pctinfo = 1;
      return S_OK;
   }

   STDMETHODIMP GetTypeInfo(UINT iTInfo, LCID lcid, ITypeInfo **ppTInfo)
   {
      if (ppTInfo == NULL) {
         return E_POINTER;
      }

      if (iTInfo != 0) {
         return DISP_E_BADINDEX;
      }

      mTypeInfo->AddRef();

      *ppTInfo = mTypeInfo;

      return S_OK;
   }

   STDMETHODIMP GetIDsOfNames(REFIID riid, LPOLESTR *rgszNames, UINT cNames, LCID lcid, DISPID *rgDispId)
   {
      return DispGetIDsOfNames(mTypeInfo, rgszNames, cNames, rgDispId);
   }
        
   STDMETHODIMP Invoke(DISPID dispIdMember,
                       REFIID riid,
                       LCID lcid,
                       WORD wFlags,
                       DISPPARAMS *pDispParams,
                       VARIANT *pVarResult,
                       EXCEPINFO *pExcepInfo,
                       UINT *puArgErr)
   {
      return DispInvoke(this,
                        mTypeInfo,
                        dispIdMember,
                        wFlags,
                        pDispParams,
                        pVarResult,
                        pExcepInfo,
                        puArgErr);                                   
   }

   // DDiscFormat2WriteEvents

   STDMETHODIMP Update(IDispatch *object, IDispatch *progress)
   {
      IDiscFormat2TrackAtOnceEventArgs *pEventArgs;
      IMAPI_FORMAT2_TAO_WRITE_ACTION action;
      LONG track = -1;
      LONG elapsed = -1;
      LONG remaining = -1;
      LONG sectors = -1;
      LONG start = -1;
      LONG last = -1;
      HRESULT hres;

      hres = progress->QueryInterface(IID_PPV_ARGS(&pEventArgs));
      if (SUCCEEDED(hres)) {
         float fraction;

         hres = pEventArgs->get_CurrentTrackNumber(&track);
         hres = pEventArgs->get_CurrentAction(&action);
         hres = pEventArgs->get_ElapsedTime(&elapsed);
         hres = pEventArgs->get_RemainingTime(&remaining);
// need to figure out best progress indicators
         hres = pEventArgs->get_SectorCount(&sectors);
         hres = pEventArgs->get_StartLba(&start);
         hres = pEventArgs->get_LastWrittenLba(&last);

         fraction = (float) (last - start) / (float) sectors;

         printf("track %d action = %d elapsed %d remaining %d\n", track, action, elapsed, remaining);

//         fraction = (elapsed + mAdjust) / (float) (remaining + elapsed + mAdjust);

         // Never set fraction to 1.0.  Screws up synchro between thread and user.
         if (fraction >= 1.0) {
            fraction = (float) 0.99;
         }

         mH->fraction = fraction;

         pEventArgs->Release();
      }

      if (mH->cancel) {
         IDiscFormat2TrackAtOnce *pDiscFormat;

         hres = object->QueryInterface(IID_PPV_ARGS(&pDiscFormat));
         if (SUCCEEDED(hres)) {

            hres = pDiscFormat->CancelAddTrack();
            if (SUCCEEDED(hres)) {
               mH->cancel = false;
            }

            pDiscFormat->Release();

            return S_OK;
         }
      }

      return S_OK;
   }

private:
   PBHandlev2 *mH;
   LPTYPEINFO mTypeInfo;
   ULONG mRefs;
   DWORD mCookie;
   IConnectionPoint *mConnectionPoint;
   IConnectionPointContainer *mConnectionPointContainer;
   int mAdjust;
};

/* Track At Once burning thread */
DWORD WINAPI PortBurn_v2_RecordDiscTAO(LPVOID lpParam)
{
   PBHandlev2 *h = (PBHandlev2 *) lpParam;
   IDiscRecorder2 *pDiscRecorder = NULL;
   IDiscFormat2TrackAtOnce *pDiscFormat = NULL;
   WriteEventsTAO *pWriteEvents = NULL;
   IStorage *pStorage = NULL;
   IMalloc *pMalloc = NULL;
   IEnumSTATSTG *pEnum = NULL;
   bool prepared = false;

   h->cancel = false;
   h->fraction = 0.0;

   h->hres = CoInitializeEx(0, COINIT_APARTMENTTHREADED);
   if (FAILED(h->hres)) {
      h->threadres = pbErrBurnFailed;
      h->fraction = 1.0;
      return S_OK;
   }

   h->hres = CoCreateInstance(__uuidof(MsftDiscRecorder2),
                              NULL,
                              CLSCTX_ALL,
                              IID_PPV_ARGS(&pDiscRecorder));
   if (FAILED(h->hres)) {
      goto done;
   }

   h->hres = pDiscRecorder->InitializeDiscRecorder(h->diskid);
   if (FAILED(h->hres)) {
      goto done;
   }

   h->hres = CoCreateInstance(__uuidof(MsftDiscFormat2TrackAtOnce),
                              NULL,
                              CLSCTX_ALL,
                              IID_PPV_ARGS(&pDiscFormat));
   if (FAILED(h->hres)) {
      goto done;
   }

   h->hres = pDiscFormat->put_ClientName(h->client);
   if (FAILED(h->hres)) {
      goto done;
   }

   h->hres = pDiscFormat->put_Recorder(pDiscRecorder);
   if (FAILED(h->hres)) {
      goto done;
   }

   pWriteEvents = new WriteEventsTAO(h);
   if (pWriteEvents == NULL) {
      goto done;
   }

   pWriteEvents->Connect(pDiscFormat);

   h->hres = pDiscFormat->PrepareMedia();
   if (FAILED(h->hres)) {
      goto done;
   }
   prepared = true;

   VARIANT_BOOL underrun = (h->underrun ? VARIANT_FALSE : VARIANT_TRUE);
   h->hres = pDiscFormat->put_BufferUnderrunFreeDisabled(underrun);
   if (FAILED(h->hres)) {
      goto done;
   }

   h->hres = CoGetMalloc(1, &pMalloc);
   if (FAILED(h->hres)) {
      goto done;
   }

   h->hres = CoGetInterfaceAndReleaseStream(h->pMarshall,
                                            __uuidof(IStorage),
                                            (void **) &pStorage);
   h->pMarshall = NULL;    // stream is always released regardless of failure
   if (FAILED(h->hres)) {
      goto done;
   }

   h->hres = pStorage->EnumElements(0, NULL, NULL, &pEnum);
   if (FAILED(h->hres)) {
      goto done;
   }

   STATSTG stat;
   ULONG fetched;
   h->hres = pEnum->Next(1, &stat, &fetched);

   while (h->hres == S_OK) {
      IStream *pStream;

      if (stat.pwcsName == NULL) {
         h->hres = E_POINTER;
         goto done;
      }

      _tprintf(L"stat name = %s\n", stat.pwcsName);
      h->hres = pStorage->OpenStream(stat.pwcsName,
                                     NULL,
                                     STGM_SHARE_EXCLUSIVE |
                                     STGM_READ,
                                     NULL,
                                     &pStream);

      pMalloc->Free(stat.pwcsName);

      if (FAILED(h->hres)) {
         goto done;
      }

      h->hres = pDiscFormat->AddAudioTrack(pStream);

      pStream->Release();
      pStream = NULL;

      if (FAILED(h->hres)) {
         goto done;
      }

      h->hres = pEnum->Next(1, &stat, &fetched);
   }

   h->hres = pDiscFormat->ReleaseMedia();
   prepared = false;

done:

   if (FAILED(h->hres)) {
      h->threadres = pbErrBurnFailed;
   }
   else {
      h->threadres = pbSuccess;
   }

   if (prepared) {
      pDiscFormat->ReleaseMedia();
   }

   if (pMalloc != NULL) {
      pMalloc->Release();
   }

   if (pEnum != NULL) {
      pEnum->Release();
   }

   if (pStorage != NULL) {
      pStorage->Release();
   }

   if (pWriteEvents != NULL) {
      pWriteEvents->Disconnect();
      pWriteEvents->Release();
   }

   if (pDiscFormat != NULL) {
      pDiscFormat->put_Recorder(NULL);
      pDiscFormat->put_ClientName(NULL);
      pDiscFormat->Release();
   }

   if (pDiscRecorder != NULL) {
      if (h->eject) {
         pDiscRecorder->EjectMedia();
      }

      pDiscRecorder->Release();
   }

   CoUninitialize();

   h->fraction = 1.0;

   return S_OK;
}

/// ==================================================================
/// ==================================================================
/// ==================================================================
/// ==================================================================
/// ==================================================================

class WriteEventsDAO:public DDiscFormat2RawCDEvents
{
public:
   WriteEventsDAO(PBHandlev2 *h)
   {
      LPTYPELIB pTypeLib;
      HRESULT hres;

      mH = h;
      mRefs = 0;
      mCookie = -1;
      mConnectionPoint = NULL;
      mConnectionPointContainer = NULL;
      mTypeInfo = NULL;
      mAdjust = 0;

      hres = LoadRegTypeLib(LIBID_IMAPILib2,                                                                    
                            IMAPILib2_MajorVersion,
                            IMAPILib2_MinorVersion,                                                         
                            LOCALE_SYSTEM_DEFAULT,
                            &pTypeLib);                                                                         
      if (FAILED(hres)) {
         return;
      }

      hres = pTypeLib->GetTypeInfoOfGuid(__uuidof(DDiscFormat2RawCDEvents), &mTypeInfo);

      pTypeLib->Release();

      if (FAILED(hres)) {
         return;
      }

      AddRef();
   }

   ~WriteEventsDAO()
   {
      Disconnect();

      if (mTypeInfo != NULL) {
         mTypeInfo->Release();
      }
   }

   void Connect(IUnknown *pConnectTo)
   {
      HRESULT hres;

      hres = pConnectTo->QueryInterface(__uuidof(IConnectionPointContainer),
                                        (void **)&mConnectionPointContainer);
      if (FAILED(hres)) {
         return;
      }

      hres = mConnectionPointContainer->FindConnectionPoint(__uuidof(DDiscFormat2RawCDEvents),
                                                            &mConnectionPoint);
      if (FAILED(hres)) {
         Disconnect();
         return;
      }

      hres = mConnectionPoint->Advise((IUnknown*)this, &mCookie);                                                
      if (FAILED(hres)) {
         Disconnect();
         return;
      }
   }

   void Disconnect()
   {
      if (mCookie != -1) {
         mConnectionPoint->Unadvise(mCookie);
         mCookie = -1;
      }

      if (mConnectionPoint != NULL) {
         mConnectionPoint->Release();
         mConnectionPoint = NULL;
      }

      if (mConnectionPointContainer != NULL) {
         mConnectionPointContainer->Release();
         mConnectionPointContainer = NULL;
      }
   }

   // IUnknown

   STDMETHODIMP QueryInterface(REFIID riid, LPVOID* ppv)
   {
      if (ppv == NULL) {
         return E_POINTER;
      }

      *ppv = NULL;

      if (riid == IID_IUnknown || riid == IID_IDispatch || riid == IID_DDiscFormat2RawCDEvents) {
         *ppv = this;
         AddRef();
         return S_OK;
      }

      return E_NOINTERFACE;
   }

   STDMETHODIMP_(ULONG) AddRef(VOID)
   {
      return InterlockedIncrement((LONG*) &mRefs);
   }

   STDMETHODIMP_(ULONG) Release(VOID)
   {
      ULONG ref = InterlockedDecrement((LONG*) &mRefs);

      if (ref == 0) {
         delete this;
      }

      return ref;
   }

   // IDispatch

   STDMETHODIMP GetTypeInfoCount(UINT *pctinfo)
   {
      *pctinfo = 1;
      return S_OK;
   }

   STDMETHODIMP GetTypeInfo(UINT iTInfo, LCID lcid, ITypeInfo **ppTInfo)
   {
      if (ppTInfo == NULL) {
         return E_POINTER;
      }

      if (iTInfo != 0) {
         return DISP_E_BADINDEX;
      }

      mTypeInfo->AddRef();

      *ppTInfo = mTypeInfo;

      return S_OK;
   }

   STDMETHODIMP GetIDsOfNames(REFIID riid, LPOLESTR *rgszNames, UINT cNames, LCID lcid, DISPID *rgDispId)
   {
      return DispGetIDsOfNames(mTypeInfo, rgszNames, cNames, rgDispId);
   }
        
   STDMETHODIMP Invoke(DISPID dispIdMember,
                       REFIID riid,
                       LCID lcid,
                       WORD wFlags,
                       DISPPARAMS *pDispParams,
                       VARIANT *pVarResult,
                       EXCEPINFO *pExcepInfo,
                       UINT *puArgErr)
   {
      return DispInvoke(this,
                        mTypeInfo,
                        dispIdMember,
                        wFlags,
                        pDispParams,
                        pVarResult,
                        pExcepInfo,
                        puArgErr);                                   
   }

   // DDiscFormat2RawCDEvents

   STDMETHODIMP Update(IDispatch *object, IDispatch *progress)
   {
      IDiscFormat2RawCDEventArgs *pEventArgs;
      LONG sectors = -1;
      LONG start = -1;
      LONG last = -1;
      HRESULT hres;

      hres = progress->QueryInterface(IID_PPV_ARGS(&pEventArgs));
      if (SUCCEEDED(hres)) {
         float fraction;

         hres = pEventArgs->get_SectorCount(&sectors);
         hres = pEventArgs->get_StartLba(&start);
         hres = pEventArgs->get_LastWrittenLba(&last);

         fraction = (float) (last - start) / (float) sectors;

//         printf("sectors %d startlba %d lastlba %d frac %f\n", sectors, start, last, fraction);

         // Never set fraction to 1.0.  Screws up synchro between thread and user.
         if (fraction >= 1.0) {
            fraction = (float) 0.99;
         }

         mH->fraction = fraction;

         pEventArgs->Release();
      }

      if (mH->cancel) {
         IDiscFormat2RawCD *pDiscFormat;

         hres = object->QueryInterface(IID_PPV_ARGS(&pDiscFormat));
         if (SUCCEEDED(hres)) {

            hres = pDiscFormat->CancelWrite();
            if (SUCCEEDED(hres)) {
               mH->cancel = false;
            }

            pDiscFormat->Release();

            return S_OK;
         }
      }

      return S_OK;
   }

private:
   PBHandlev2 *mH;
   LPTYPEINFO mTypeInfo;
   ULONG mRefs;
   DWORD mCookie;
   IConnectionPoint *mConnectionPoint;
   IConnectionPointContainer *mConnectionPointContainer;
   int mAdjust;
};

/* Track At Once burning thread */
DWORD WINAPI PortBurn_v2_RecordDiscDAO(LPVOID lpParam)
{
   PBHandlev2 *h = (PBHandlev2 *) lpParam;
   IDiscRecorder2 *pDiscRecorder = NULL;
   IDiscFormat2RawCD *pDiscFormat = NULL;
   IRawCDImageCreator *pDiscImage = NULL;
   WriteEventsDAO *pWriteEvents = NULL;
   IStorage *pStorage = NULL;
   IMalloc *pMalloc = NULL;
   IEnumSTATSTG *pEnum = NULL;
   bool prepared = false;

   h->cancel = false;
   h->fraction = 0.0;

   h->hres = CoInitializeEx(0, COINIT_APARTMENTTHREADED);
   if (FAILED(h->hres)) {
      h->threadres = pbErrBurnFailed;
      h->fraction = 1.0;
      return S_OK;
   }

   h->hres = CoCreateInstance(__uuidof(MsftDiscRecorder2),
                              NULL,
                              CLSCTX_ALL,
                              IID_PPV_ARGS(&pDiscRecorder));
   if (FAILED(h->hres)) {
      goto done;
   }

   h->hres = pDiscRecorder->InitializeDiscRecorder(h->diskid);
   if (FAILED(h->hres)) {
      goto done;
   }

   h->hres = CoCreateInstance(__uuidof(MsftDiscFormat2RawCD),
                              NULL,
                              CLSCTX_ALL,
                              IID_PPV_ARGS(&pDiscFormat));
   if (FAILED(h->hres)) {
      goto done;
   }

   h->hres = pDiscFormat->put_ClientName(h->client);
   if (FAILED(h->hres)) {
      goto done;
   }

   h->hres = pDiscFormat->put_Recorder(pDiscRecorder);
   if (FAILED(h->hres)) {
      goto done;
   }

   pWriteEvents = new WriteEventsDAO(h);
   if (pWriteEvents == NULL) {
      goto done;
   }

   pWriteEvents->Connect(pDiscFormat);

   h->hres = pDiscFormat->PrepareMedia();
   if (FAILED(h->hres)) {
      goto done;
   }
   prepared = true;

   VARIANT_BOOL underrun = (h->underrun ? VARIANT_FALSE : VARIANT_TRUE);
   h->hres = pDiscFormat->put_BufferUnderrunFreeDisabled(underrun);
   if (FAILED(h->hres)) {
      goto done;
   }

   h->hres = CoCreateInstance(__uuidof(MsftRawCDImageCreator),
                              NULL,
                              CLSCTX_ALL,
                              IID_PPV_ARGS(&pDiscImage));
   if (FAILED(h->hres)) {
      goto done;
   }

   VARIANT_BOOL gapless = (h->gapless ? VARIANT_FALSE : VARIANT_TRUE);
   h->hres = pDiscImage->put_DisableGaplessAudio(gapless);
   if (FAILED(h->hres)) {
      goto done;
   }

   h->hres = CoGetMalloc(1, &pMalloc);
   if (FAILED(h->hres)) {
      goto done;
   }

   h->hres = CoGetInterfaceAndReleaseStream(h->pMarshall,
                                            __uuidof(IStorage),
                                            (void **) &pStorage);
   h->pMarshall = NULL;    // stream is always released regardless of failure
   if (FAILED(h->hres)) {
      goto done;
   }

   h->hres = pStorage->EnumElements(0, NULL, NULL, &pEnum);
   if (FAILED(h->hres)) {
      goto done;
   }

   STATSTG stat;
   ULONG fetched;
   h->hres = pEnum->Next(1, &stat, &fetched);

   while (h->hres == S_OK) {
      IStream *pStream;

      if (stat.pwcsName == NULL) {
         h->hres = E_POINTER;
         goto done;
      }

      _tprintf(L"stat name = %s\n", stat.pwcsName);
      h->hres = pStorage->OpenStream(stat.pwcsName,
                                     NULL,
                                     STGM_SHARE_EXCLUSIVE |
                                     STGM_READ,
                                     NULL,
                                     &pStream);

      pMalloc->Free(stat.pwcsName);

      if (FAILED(h->hres)) {
         goto done;
      }

      LONG tindex = 0;
      h->hres = pDiscImage->AddTrack(IMAPI_CD_SECTOR_AUDIO, pStream, &tindex);

      pStream->Release();
      pStream = NULL;

      if (FAILED(h->hres)) {
         goto done;
      }

      h->hres = pEnum->Next(1, &stat, &fetched);
   }

   IStream *pStream;
   h->hres = pDiscImage->CreateResultImage(&pStream);
   if (SUCCEEDED(h->hres)) {
      h->hres = pDiscFormat->WriteMedia(pStream);

      pStream->Release();
   }

   if (FAILED(h->hres)) {
      goto done;
   }

   h->hres = pDiscFormat->ReleaseMedia();
   prepared = false;

done:

   if (FAILED(h->hres)) {
      h->threadres = pbErrBurnFailed;
   }
   else {
      h->threadres = pbSuccess;
   }

   if (prepared) {
      pDiscFormat->ReleaseMedia();
   }

   if (pMalloc != NULL) {
      pMalloc->Release();
   }

   if (pEnum != NULL) {
      pEnum->Release();
   }

   if (pStorage != NULL) {
      pStorage->Release();
   }

   if (pDiscImage != NULL) {
      pDiscImage->Release();
   }

   if (pWriteEvents != NULL) {
      pWriteEvents->Disconnect();
      pWriteEvents->Release();
   }

   if (pDiscFormat != NULL) {
      pDiscFormat->put_Recorder(NULL);
      pDiscFormat->put_ClientName(NULL);
      pDiscFormat->Release();
   }

   if (pDiscRecorder != NULL) {
      if (h->eject) {
         pDiscRecorder->EjectMedia();
      }

      pDiscRecorder->Release();
   }

   CoUninitialize();

   h->fraction = 1.0;

   return S_OK;
}

/// ==================================================================
/// ==================================================================
/// ==================================================================
/// ==================================================================
/// ==================================================================

class EraseEvents:public DDiscFormat2EraseEvents
{
public:
   EraseEvents(PBHandlev2 *h)
   {
      LPTYPELIB pTypeLib;
      HRESULT hres;

      mH = h;
      mRefs = 0;
      mCookie = -1;
      mConnectionPoint = NULL;
      mConnectionPointContainer = NULL;

      hres = LoadRegTypeLib(LIBID_IMAPILib2,                                                                    
                            IMAPILib2_MajorVersion,
                            IMAPILib2_MinorVersion,                                                         
                            LOCALE_SYSTEM_DEFAULT,
                            &pTypeLib);                                                                         
      if (FAILED(hres)) {
         return;
      }

      hres = pTypeLib->GetTypeInfoOfGuid(IID_DDiscFormat2EraseEvents, &mTypeInfo);

      pTypeLib->Release();

      if (FAILED(hres)) {
         return;
      }

      AddRef();
   }

   ~EraseEvents()
   {
      Disconnect();

      if (mTypeInfo != NULL) {
         mTypeInfo->Release();
      }
   }

   void Connect(IUnknown *pConnectTo)
   {
      HRESULT hres;

      hres = pConnectTo->QueryInterface(IID_IConnectionPointContainer,
                                        (void **)&mConnectionPointContainer);
      if (FAILED(hres)) {
         return;
      }

      hres = mConnectionPointContainer->FindConnectionPoint(IID_DDiscFormat2EraseEvents,
                                                            &mConnectionPoint);
      if (FAILED(hres)) {
         Disconnect();
         return;
      }

      hres = mConnectionPoint->Advise((IUnknown*)this, &mCookie);                                                
      if (FAILED(hres)) {
         Disconnect();
         return;
      }
   }

   void Disconnect()
   {
      if (mCookie != -1) {
         mConnectionPoint->Unadvise(mCookie);
         mCookie = -1;
      }

      if (mConnectionPoint != NULL) {
         mConnectionPoint->Release();
         mConnectionPoint = NULL;
      }

      if (mConnectionPointContainer != NULL) {
         mConnectionPointContainer->Release();
         mConnectionPointContainer = NULL;
      }
   }

   // IUnknown

   STDMETHODIMP QueryInterface(REFIID riid, LPVOID* ppv)
   {
      if (ppv == NULL) {
         return E_POINTER;
      }

      *ppv = NULL;

      if (riid == IID_IUnknown || riid == IID_IDispatch || riid == IID_DDiscFormat2EraseEvents) {
         *ppv = this;
         AddRef();
         return S_OK;
      }

      return E_NOINTERFACE;
   }

   STDMETHODIMP_(ULONG) AddRef(VOID)
   {
      return InterlockedIncrement((LONG*) &mRefs);
   }

   STDMETHODIMP_(ULONG) Release(VOID)
   {
      ULONG ref = InterlockedDecrement((LONG*) &mRefs);

      if (ref == 0) {
         delete this;
      }

      return ref;
   }

   // IDispatch

   STDMETHODIMP GetTypeInfoCount(UINT *pctinfo)
   {
      *pctinfo = 1;
      return S_OK;
   }

   STDMETHODIMP GetTypeInfo(UINT iTInfo, LCID lcid, ITypeInfo **ppTInfo)
   {
      if (ppTInfo == NULL) {
         return E_POINTER;
      }

      if (iTInfo != 0) {
         return DISP_E_BADINDEX;
      }

      mTypeInfo->AddRef();

      *ppTInfo = mTypeInfo;

      return S_OK;
   }

   STDMETHODIMP GetIDsOfNames(REFIID riid, LPOLESTR *rgszNames, UINT cNames, LCID lcid, DISPID *rgDispId)
   {
      return DispGetIDsOfNames(mTypeInfo, rgszNames, cNames, rgDispId);
   }
        
   STDMETHODIMP Invoke(DISPID dispIdMember,
                       REFIID riid,
                       LCID lcid,
                       WORD wFlags,
                       DISPPARAMS *pDispParams,
                       VARIANT *pVarResult,
                       EXCEPINFO *pExcepInfo,
                       UINT *puArgErr)
   {
      return DispInvoke(this,
                        mTypeInfo,
                        dispIdMember,
                        wFlags,
                        pDispParams,
                        pVarResult,
                        pExcepInfo,
                        puArgErr);                                   
   }

   // DDiscFormat2EraseEvents

   STDMETHODIMP Update(IDispatch *object,
                       LONG elapsedSeconds,
                       LONG estimatedTotalSeconds)
   {
      float fraction = elapsedSeconds / (float) estimatedTotalSeconds;

      // Never set fraction to 1.0.  Screws up synchro between thread and user.
      if (fraction >= 1.0) {
         fraction = (float) 0.99;
      }

      mH->fraction = fraction;

      return S_OK;
   }

private:
   PBHandlev2 *mH;
   LPTYPEINFO mTypeInfo;
   ULONG mRefs;
   DWORD mCookie;
   IConnectionPoint *mConnectionPoint;
   IConnectionPointContainer *mConnectionPointContainer;
};

/* Erasing Thread */
DWORD WINAPI PortBurn_v2_EraseDisc(LPVOID lpParam)
{
   PBHandlev2 *h = (PBHandlev2 *) lpParam;
   IDiscRecorder2 *pDiscRecorder = NULL;
   IDiscFormat2Erase *pDiscErase = NULL;
   EraseEvents *pEraseEvents = NULL;
   bool mcndisabled = false;

   h->hres = CoInitializeEx(0, COINIT_APARTMENTTHREADED);
   if (FAILED(h->hres)) {
      h->threadres = pbErrEraseFailed;
      h->fraction = 1.0;
      return S_OK;
   }

   h->hres = CoCreateInstance(__uuidof(MsftDiscRecorder2),
                              NULL,
                              CLSCTX_ALL,
                              IID_PPV_ARGS(&pDiscRecorder));
   if (FAILED(h->hres)) {
      goto done;
   }

   h->hres = pDiscRecorder->InitializeDiscRecorder(h->diskid);
   if (FAILED(h->hres)) {
      goto done;
   }

   h->hres = pDiscRecorder->DisableMcn();
   if (FAILED(h->hres)) {
      goto done;
   }
   mcndisabled = true;

   h->hres = CoCreateInstance(__uuidof(MsftDiscFormat2Erase),
                              NULL,
                              CLSCTX_ALL,
                              IID_PPV_ARGS(&pDiscErase));
   if (FAILED(h->hres)) {
      goto done;
   }

   h->hres = pDiscErase->put_ClientName(h->client);
   if (FAILED(h->hres)) {
      goto done;
   }

   h->hres = pDiscErase->put_Recorder(pDiscRecorder);
   if (FAILED(h->hres)) {
      goto done;
   }

   h->hres = pDiscErase->put_FullErase(h->fullerase);
   if (FAILED(h->hres)) {
      goto done;
   }

   pEraseEvents = new EraseEvents(h);
   if (pEraseEvents == NULL) {
      goto done;
   }

   pEraseEvents->Connect(pDiscErase);

   h->cancel = false;
   h->fraction = 0.0;

   h->hres = pDiscErase->EraseMedia();

   if (SUCCEEDED(h->hres)) {
      IDiscRecorder2Ex *pDiscRecorderEx;
      int cnt = 50;
      bool blank = false;

      h->hres = pDiscRecorder->QueryInterface(__uuidof(IDiscRecorder2Ex),
                                              (void **) &pDiscRecorderEx);
      if (SUCCEEDED(h->hres)) {
         while (!blank && cnt--) {
            BYTE *info;
            ULONG_IMAPI2_DISC_INFORMATION size;

            SleepEx(100, true);

            h->hres = pDiscRecorderEx->GetDiscInformation(&info, &size);
            if (FAILED(h->hres)) {
               break;
            }

            blank = ((info[2] & 0x03) == 0);

            CoTaskMemFree(info);
         }

         pDiscRecorderEx->Release();
      }
   }

done:

   if (FAILED(h->hres)) {
      h->threadres = pbErrEraseFailed;
   }
   else {
      h->threadres = pbSuccess;
   }

   if (pEraseEvents != NULL) {
      pEraseEvents->Disconnect();
      pEraseEvents->Release();
   }

   if (pDiscErase != NULL) {
      pDiscErase->put_ClientName(NULL);
      pDiscErase->put_Recorder(NULL);
      pDiscErase->Release();
   }

   if (mcndisabled) {
      pDiscRecorder->EnableMcn();
   }

   if (pDiscRecorder != NULL) {
      pDiscRecorder->Release();
   }

   CoUninitialize();

   h->fraction = 1.0;

   return S_OK;
}

/// ==================================================================
/// ==================================================================
/// ==================================================================
/// ==================================================================
/// ==================================================================

static BSTR get_diskid(void *handle, int index)
{
   PBHandlev2 *h = (PBHandlev2 *) handle;
   IDiscMaster2 *pDiscMaster = NULL;
   BSTR bID = NULL;
   LONG count;

   if (h == NULL) {
      return NULL;
   }

   h->hres = S_OK;

   h->hres = CoCreateInstance(__uuidof(MsftDiscMaster2),
                              NULL,
                              CLSCTX_ALL,
                              IID_PPV_ARGS(&pDiscMaster));
   if (FAILED(h->hres)) {
      goto done;
   }

   h->hres = pDiscMaster->get_Count(&count);
   if (FAILED(h->hres)) {
      goto done;
   }

   if (index < 0 || index >= count) {
      h->hres = E_INVALIDARG;
      goto done;
   }

   h->hres = pDiscMaster->get_Item(index, &bID);
   if (FAILED(h->hres)) {
      goto done;
   }

done:

   if (pDiscMaster != NULL) {
      pDiscMaster->Release();
   }

   return bID;
}

static IDiscRecorder2 *get_recorder(void *handle, int index)
{
   PBHandlev2 *h = (PBHandlev2 *) handle;
   IDiscRecorder2 *pDiscRecorder = NULL;
   BSTR bID = NULL;

   if (h == NULL) {
      return NULL;
   }

   h->hres = S_OK;

   bID = get_diskid(handle, index);
   if (bID == NULL) {
      goto done;
   }

   h->hres = CoCreateInstance(__uuidof(MsftDiscRecorder2),
                              NULL,
                              CLSCTX_ALL,
                              IID_PPV_ARGS(&pDiscRecorder));
   if (FAILED(h->hres)) {
      goto done;
   }

   h->hres = pDiscRecorder->InitializeDiscRecorder(bID);
   if (FAILED(h->hres)) {
      pDiscRecorder->Release();
      pDiscRecorder = NULL;
   }

done:

   if (bID != NULL) {
      SysFreeString(bID);
   }

   return pDiscRecorder;
}

// ----------------------------------------------------------------------------

void *PortBurn_v2_Open()
{   
   PBHandlev2 *h;
   WCHAR name[255];

#if defined(_DEBUG)
   AllocConsole();
   freopen("CONOUT$", "w", stdout);
#endif

   h = (PBHandlev2 *) HeapAlloc(GetProcessHeap(),
                              HEAP_ZERO_MEMORY,
                              sizeof(PBHandlev2));
   if (h == NULL) {
      return NULL;
   }

   h->test = pbTestDefault;
   h->verify = pbVerifyDefault;
   h->underrun = pbUnderrunDefault;
   h->eject = pbEjectDefault;
   h->gapless = pbGaplessDefault;
   h->speed = pbSpeedDefault;

   _stprintf_s(name, 255, L"portburn_client_%d", GetTickCount());

   h->client = SysAllocString(name);
   if (h->client == NULL) {
      HeapFree(GetProcessHeap(), 0, h);
      return NULL;
   }

   return h;
}

/* Cleanup */
int PortBurn_v2_CloseDevice(void *handle);
void PortBurn_v2_Close(void *handle)
{
   PBHandlev2 *h = (PBHandlev2 *) handle;

   if (h == NULL) {
      return;
   }

   PortBurn_v2_CloseDevice(h);

   if (h->client != NULL) {
      SysFreeString(h->client);
      h->client = NULL;
   }

   HeapFree(GetProcessHeap(), 0, h);
}

/* Return a human-readable error string for the last operating system
   specific error (NOT human readable strings for the PortBurn error
   codes).  Caller should dispose of the returned string using free(). */
char *PortBurn_v2_LastError(void *handle)
{
   PBHandlev2 *h = (PBHandlev2 *) handle;
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
int PortBurn_v2_GetNumDevices(void *handle)
{
   PBHandlev2 *h = (PBHandlev2 *) handle;
   IDiscMaster2 *pDiscMaster;
   LONG count = 0;

   if (h == NULL) {
      return NULL;
   }

   h->hres = CoCreateInstance(__uuidof(MsftDiscMaster2),
                              NULL,
                              CLSCTX_ALL,
                              IID_PPV_ARGS(&pDiscMaster));
   if (SUCCEEDED(h->hres)) {
      h->hres = pDiscMaster->get_Count(&count);

      pDiscMaster->Release();
   }

   return count;
}

/* Get the name of the device with a given index.  Only valid
   after a call to GetNumDevices. */
char *PortBurn_v2_GetDeviceName(void *handle, int index)
{
   PBHandlev2 *h = (PBHandlev2 *) handle;
   IDiscRecorder2 *pDiscRecorder = NULL;
   BSTR bVendor = NULL;
   BSTR bProduct = NULL;
   BSTR bRevision = NULL;
   char *name = NULL;
   TCHAR *wname;
   int len;

   if (h == NULL) {
      return NULL;
   }

   h->hres = S_OK;

   pDiscRecorder = get_recorder(h, index);
   if (pDiscRecorder == NULL) {
      goto done;
   }

   h->hres = pDiscRecorder->get_VendorId(&bVendor);
   if (FAILED(h->hres)) {
      goto done;
   }

   h->hres = pDiscRecorder->get_ProductId(&bProduct);
   if (FAILED(h->hres)) {
      goto done;
   }

   h->hres = pDiscRecorder->get_ProductRevision(&bRevision);
   if (FAILED(h->hres)) {
      goto done;
   }

   len = SysStringLen(bVendor) + 1 +
         SysStringLen(bProduct) + 1 +
         SysStringLen(bRevision);

   name = (char *) malloc(len + 1);
   if (name == NULL) {
      h->hres = E_OUTOFMEMORY;
      goto done;
   }

   wname = (LPWSTR) alloca((len + 1) * sizeof(wchar_t));
   _stprintf_s(wname,
               (len + 1),
               _T("%s %s %s"),
               (LPCWSTR) bVendor,
               (LPCWSTR) bProduct,
               (LPCWSTR) bRevision);

   name[0] = '\0';
   WideCharToMultiByte(CP_ACP, 0, wname, len, name, len, NULL, NULL);
   name[len] = '\0';

done:

   if (bRevision != NULL) {
      SysFreeString(bRevision);
   }

   if (bProduct != NULL) {
      SysFreeString(bProduct);
   }

   if (bVendor != NULL) {
      SysFreeString(bVendor);
   }

   if (pDiscRecorder != NULL) {
      pDiscRecorder->Release();
   }

   return name;
}

/* Open a particular device by index number.  Returns 0 on success;
   any nonzero value indicates an error, for example if the device is
   already open by some other program. */
int PortBurn_v2_OpenDevice(void *handle, int index)
{
   PBHandlev2 *h = (PBHandlev2 *) handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   h->hres = S_OK;

   if (h->diskid != NULL) {
      return pbErrDeviceAlreadyOpen;
   }

   h->diskid = get_diskid(handle, index);
   if (h->diskid == NULL) {
      return pbErrCannotReserveDevice;
   }

   return pbSuccess;
}

/* Close a device */
int PortBurn_v2_CloseDevice(void *handle)
{
   PBHandlev2 *h = (PBHandlev2 *) handle;

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

   if (h->pStream != NULL) {
      h->pStream->Release();
      h->pStream = NULL;
   }

   if (h->pMarshall != NULL) {
      IStorage *pStorage;
      h->hres = CoGetInterfaceAndReleaseStream(h->pMarshall,
                                               __uuidof(IStorage),
                                               (void **) &pStorage);
      h->pMarshall = NULL;
   }

   if (h->pStorage != NULL) {
      h->pStorage->Release();
      h->pStorage = NULL;
   }

   if (h->diskid != NULL) {
      SysFreeString(h->diskid);
      h->diskid = NULL;
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
int PortBurn_v2_EjectDevice(void *handle)
{
   PBHandlev2 *h = (PBHandlev2 *) handle;
   IDiscRecorder2 *pDiscRecorder = NULL;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   h->hres = S_OK;

   if (h->diskid == NULL) {
      return pbErrDeviceNotOpen;
   }

   h->hres = CoCreateInstance(__uuidof(MsftDiscRecorder2),
                              NULL,
                              CLSCTX_ALL,
                              IID_PPV_ARGS(&pDiscRecorder));
   if (FAILED(h->hres)) {
      return pbErrCannotEject;
   }

   h->hres = pDiscRecorder->InitializeDiscRecorder(h->diskid);
   if (FAILED(h->hres)) {
      pDiscRecorder->Release();
      return pbErrCannotEject;
   }

   h->hres = pDiscRecorder->EjectMedia();

   pDiscRecorder->Release();

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
int PortBurn_v2_StartStaging(void *handle, const char *tmpdir)
{
   PBHandlev2 *h = (PBHandlev2 *) handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   h->hres = S_OK;

   if (h->diskid == NULL) {
      return pbErrDeviceNotOpen;
   }

   if (h->pStorage != NULL) {
      return pbErrAlreadyStagingOrBurning;
   }

   h->curtrack = -1;

   h->hres = StgCreateStorageEx(h->client,   // NULL,
                                STGM_CREATE |
                                STGM_SHARE_EXCLUSIVE |
                                STGM_READWRITE |
                                STGM_DELETEONRELEASE,
                                STGFMT_STORAGE,
                                0,
                                NULL,
                                NULL,
                                IID_IStorage,
                                (void **) &h->pStorage);
   if (FAILED(h->hres)) {
      return pbErrCannotCreateStagingDirectory;
   }

   h->hres = CoMarshalInterThreadInterfaceInStream(__uuidof(IStorage),
                                                   h->pStorage,
                                                   &h->pMarshall);
   if (FAILED(h->hres)) {
      h->pStorage->Release();
      h->pStorage = NULL;
      return pbErrCannotCreateStagingDirectory;
   }

   return pbSuccess;
}

/* Start a new audio track.  Pass the name of the track, and the
   length in CD Audio frames (each frame is 1/75.0 of a second, exactly). */
int PortBurn_v2_StartTrack(void *handle, const char *name)
{
   PBHandlev2 *h = (PBHandlev2 *) handle;
   WCHAR wname[255];
   int curtrack;
printf("start track\n");
   if (h == NULL) {
      return pbErrNoHandle;
   }

   h->hres = S_OK;
 
   if (h->pStorage == NULL) {
      return pbErrMustCallStartStaging;
   }

   curtrack = h->curtrack + 1;
   if (curtrack == 99) {
      return pbErrCannotStageTrack;
   }

   _stprintf_s(wname, 255, L"track #%d", curtrack);

   h->hres = h->pStorage->CreateStream(wname,
                                       STGM_CREATE |
                                       STGM_SHARE_EXCLUSIVE |
                                       STGM_READWRITE,
                                       0,
                                       0,
                                       &h->pStream);
   if (FAILED(h->hres)) {
      return pbErrCannotCreateStagingFile;
   }

   h->curtrack = curtrack;

   return pbSuccess;
}

/* Add one frame of audio to the current track.  The buffer must be exactly
   1176 elements long, containing interleaved left and right audio samples.
   The values should be signed 16-bit numbers in the native endianness of
   this computer. */
int PortBurn_v2_AddFrame(void *handle, short *buffer)
{
   PBHandlev2 *h = (PBHandlev2 *) handle;
   int oneBlockByteCount = 1176 * 2;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   h->hres = S_OK;

   if (h->pStream == NULL) {
      return pbErrMustCallStartTrack;
   }

   h->hres = h->pStream->Write(buffer, oneBlockByteCount, NULL);
   if (FAILED(h->hres)) {
      return pbErrCannotWriteToStagingFile;
   }

   return pbSuccess;
}

/* Finish the current audio track. */
int PortBurn_v2_EndTrack(void *handle)
{
   PBHandlev2 *h = (PBHandlev2 *) handle;
   LARGE_INTEGER zero = {0};

   if (h == NULL) {
      return pbErrNoHandle;
   }

   h->hres = S_OK;

   if (h->pStream == NULL) {
      return pbErrMustCallStartTrack;
   }

   h->pStream->Release();
   h->pStream = NULL;

   return pbSuccess;
}

/* Begin burning the disc. */
int PortBurn_v2_StartBurning(void *handle)
{
   PBHandlev2 *h = (PBHandlev2 *) handle;
   DWORD dwID;

   if (h == NULL) {
      return pbErrNoHandle;
   }
printf("starting burn\n");
   h->hres = S_OK;

   if (h->curtrack < 0) {
      return pbErrMustCallStartTrack;
   }

   if (h->hThread != NULL) {
      return pbErrAlreadyStagingOrBurning;
   }

   h->burning = true;

   LPTHREAD_START_ROUTINE start;
#if 1
   IDiscFormat2RawCD *pDiscFormat;
   h->hres = CoCreateInstance(__uuidof(MsftDiscFormat2RawCD),
                              NULL,
                              CLSCTX_ALL,
                              IID_PPV_ARGS(&pDiscFormat));
   if (SUCCEEDED(h->hres)) {
      pDiscFormat->Release();
      start = PortBurn_v2_RecordDiscDAO;
   }
   else {
      start = PortBurn_v2_RecordDiscTAO;
   }
#else
   start = PortBurn_v2_RecordDiscTAO;
#endif

   h->hThread = CreateThread(NULL,
                             0,
                             start,
                             h,
                             0,
                             &dwID);
   if (h->hThread == NULL) {
      return pbErrCannotStartBurning;
   }

   return pbSuccess;
}

/* Cancel if burning was in progress.  It might take a while for
   this to take effect; wait until GetStatus says 1.0 to close
   the device. */
int PortBurn_v2_CancelBurning(void *handle)
{
   PBHandlev2 *h = (PBHandlev2 *) handle;
   float frac = 0.0;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   h->hres = S_OK;

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
int PortBurn_v2_GetStatus(void *handle, float *out_fraction_complete)
{
   PBHandlev2 *h = (PBHandlev2 *) handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   h->hres = S_OK;

   if (h->hThread == NULL) {
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
      WaitForSingleObject(h->hThread, INFINITE);
      CloseHandle(h->hThread);
      h->hThread = NULL;
   }

   return pbSuccess;
}

/* Get option value. */
int PortBurn_v2_GetOption(void *handle, int option, int *value)
{
   PBHandlev2 *h = (PBHandlev2 *) handle;
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

int PortBurn_v2_SetOption(void *handle, int option, int value)
{
   PBHandlev2 *h = (PBHandlev2 *) handle;
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
int PortBurn_v2_StartErasing(void *handle, int type)
{
   PBHandlev2 *h = (PBHandlev2 *) handle;
   DWORD dwID;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   h->hres = S_OK;

   if (h->hThread != NULL) {
      return pbErrCannotStartErasing;
   }

   h->fullerase = (type == pbEraseQuick ? VARIANT_FALSE : VARIANT_TRUE);

   h->erasing = true;

   h->hThread = CreateThread(NULL,
                             0,
                             PortBurn_v2_EraseDisc,
                             h,
                             0,
                             &dwID);
   if (h->hThread == NULL) {
      return pbErrCannotStartErasing;
   }

   return pbSuccess;
}

int PortBurn_v2_GetEraseStatus(void *handle, float *out_fraction_complete)
{
   PBHandlev2 *h = (PBHandlev2 *) handle;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   h->hres = S_OK;

   if (h->diskid == NULL) {
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
   }

   return pbSuccess;
}

/* */
int PortBurn_v2_GetMediaState(void *handle, int *state)
{
   PBHandlev2 *h = (PBHandlev2 *) handle;
   IDiscRecorder2 *pDiscRecorder;
   IDiscFormat2Data *pDiscFormat;
   IMAPI_FORMAT2_DATA_MEDIA_STATE status;
   int mstate;

   if (h == NULL) {
      return pbErrNoHandle;
   }

   h->hres = S_OK;
   mstate = 0;

   if (h->diskid == NULL) {
      return pbErrDeviceNotOpen;
   }

   h->hres = CoCreateInstance(__uuidof(MsftDiscRecorder2),
                              NULL,
                              CLSCTX_ALL,
                              IID_PPV_ARGS(&pDiscRecorder));
   if (FAILED(h->hres)) {
      return pbErrCannotAccessDevice;
   }

   h->hres = pDiscRecorder->InitializeDiscRecorder(h->diskid);
   if (FAILED(h->hres)) {
      pDiscRecorder->Release();
      return pbErrCannotAccessDevice;
   }

   h->hres = CoCreateInstance(__uuidof(MsftDiscFormat2Data),
                              NULL,
                              CLSCTX_ALL,
                              IID_PPV_ARGS(&pDiscFormat));
   if (FAILED(h->hres)) {
      pDiscRecorder->Release();
      return pbErrCannotAccessDevice;
   }

   h->hres = pDiscFormat->put_Recorder(pDiscRecorder);
   if (FAILED(h->hres)) {
      pDiscFormat->Release();
      pDiscRecorder->Release();
      return pbErrCannotAccessDevice;
   }

   h->hres = pDiscFormat->get_CurrentMediaStatus(&status);

   pDiscFormat->put_Recorder(NULL);
   pDiscFormat->Release();
   pDiscRecorder->Release();

   if (h->hres == E_IMAPI_RECORDER_MEDIA_NO_MEDIA) {
      *state = pbMediaNone;
      return pbSuccess;
   } 

   if (FAILED(h->hres)) {
      return pbErrCannotAccessDevice;
   }

   if (status & IMAPI_FORMAT2_DATA_MEDIA_STATE_BLANK) {
      mstate |= pbMediaBlank;
   }

   if (!(status & IMAPI_FORMAT2_DATA_MEDIA_STATE_WRITE_PROTECTED)) {
      mstate |= pbMediaErasable;
   }

   if (status & IMAPI_FORMAT2_DATA_MEDIA_STATE_APPENDABLE) {
      mstate |= pbMediaAppendable;
   }

   if (status & IMAPI_FORMAT2_DATA_MEDIA_STATE_OVERWRITE_ONLY) {
      mstate |= pbMediaOverwritable;
   }

   *state = mstate;

   return pbSuccess;
}

int PortBurn_v2_GetSupportedSpeeds(void *handle, int *cnt, int *speeds[])
{
   *cnt = 0;
   *speeds = NULL;
   return pbErrNoHandle;
}
