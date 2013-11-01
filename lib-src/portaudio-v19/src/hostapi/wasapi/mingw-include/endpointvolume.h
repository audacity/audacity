

/* this ALWAYS GENERATED file contains the definitions for the interfaces */


 /* File created by MIDL compiler version 7.00.0499 */
/* Compiler settings for endpointvolume.idl:
    Oicf, W1, Zp8, env=Win32 (32b run)
    protocol : dce , ms_ext, c_ext, robust
    error checks: allocation ref bounds_check enum stub_data 
    VC __declspec() decoration level: 
         __declspec(uuid()), __declspec(selectany), __declspec(novtable)
         DECLSPEC_UUID(), MIDL_INTERFACE()
*/
//@@MIDL_FILE_HEADING(  )

#pragma warning( disable: 4049 )  /* more than 64k source lines */


/* verify that the <rpcndr.h> version is high enough to compile this file*/
#ifndef __REQUIRED_RPCNDR_H_VERSION__
#define __REQUIRED_RPCNDR_H_VERSION__ 500
#endif

/* verify that the <rpcsal.h> version is high enough to compile this file*/
#ifndef __REQUIRED_RPCSAL_H_VERSION__
#define __REQUIRED_RPCSAL_H_VERSION__ 100
#endif

#include "rpc.h"
#include "rpcndr.h"

#ifndef __RPCNDR_H_VERSION__
#error this stub requires an updated version of <rpcndr.h>
#endif // __RPCNDR_H_VERSION__

#ifndef COM_NO_WINDOWS_H
#include "windows.h"
#include "ole2.h"
#endif /*COM_NO_WINDOWS_H*/

#ifndef __endpointvolume_h__
#define __endpointvolume_h__

#if defined(_MSC_VER) && (_MSC_VER >= 1020)
#pragma once
#endif

/* Forward Declarations */ 

#ifndef __IAudioEndpointVolumeCallback_FWD_DEFINED__
#define __IAudioEndpointVolumeCallback_FWD_DEFINED__
typedef interface IAudioEndpointVolumeCallback IAudioEndpointVolumeCallback;
#endif 	/* __IAudioEndpointVolumeCallback_FWD_DEFINED__ */


#ifndef __IAudioEndpointVolume_FWD_DEFINED__
#define __IAudioEndpointVolume_FWD_DEFINED__
typedef interface IAudioEndpointVolume IAudioEndpointVolume;
#endif 	/* __IAudioEndpointVolume_FWD_DEFINED__ */


#ifndef __IAudioMeterInformation_FWD_DEFINED__
#define __IAudioMeterInformation_FWD_DEFINED__
typedef interface IAudioMeterInformation IAudioMeterInformation;
#endif 	/* __IAudioMeterInformation_FWD_DEFINED__ */


/* header files for imported files */
#include "unknwn.h"
#include "devicetopology.h"

#ifdef __cplusplus
extern "C"{
#endif 


/* interface __MIDL_itf_endpointvolume_0000_0000 */
/* [local] */ 

typedef struct AUDIO_VOLUME_NOTIFICATION_DATA
    {
    GUID guidEventContext;
    BOOL bMuted;
    float fMasterVolume;
    UINT nChannels;
    float afChannelVolumes[ 1 ];
    } 	AUDIO_VOLUME_NOTIFICATION_DATA;

typedef struct AUDIO_VOLUME_NOTIFICATION_DATA *PAUDIO_VOLUME_NOTIFICATION_DATA;

#define   ENDPOINT_HARDWARE_SUPPORT_VOLUME    0x00000001
#define   ENDPOINT_HARDWARE_SUPPORT_MUTE      0x00000002
#define   ENDPOINT_HARDWARE_SUPPORT_METER     0x00000004


extern RPC_IF_HANDLE __MIDL_itf_endpointvolume_0000_0000_v0_0_c_ifspec;
extern RPC_IF_HANDLE __MIDL_itf_endpointvolume_0000_0000_v0_0_s_ifspec;

#ifndef __IAudioEndpointVolumeCallback_INTERFACE_DEFINED__
#define __IAudioEndpointVolumeCallback_INTERFACE_DEFINED__

/* interface IAudioEndpointVolumeCallback */
/* [unique][helpstring][nonextensible][uuid][local][object] */ 


EXTERN_C const IID IID_IAudioEndpointVolumeCallback;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("657804FA-D6AD-4496-8A60-352752AF4F89")
    IAudioEndpointVolumeCallback : public IUnknown
    {
    public:
        virtual HRESULT STDMETHODCALLTYPE OnNotify( 
            PAUDIO_VOLUME_NOTIFICATION_DATA pNotify) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IAudioEndpointVolumeCallbackVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IAudioEndpointVolumeCallback * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ 
            __RPC__deref_out  void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IAudioEndpointVolumeCallback * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IAudioEndpointVolumeCallback * This);
        
        HRESULT ( STDMETHODCALLTYPE *OnNotify )( 
            IAudioEndpointVolumeCallback * This,
            PAUDIO_VOLUME_NOTIFICATION_DATA pNotify);
        
        END_INTERFACE
    } IAudioEndpointVolumeCallbackVtbl;

    interface IAudioEndpointVolumeCallback
    {
        CONST_VTBL struct IAudioEndpointVolumeCallbackVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IAudioEndpointVolumeCallback_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IAudioEndpointVolumeCallback_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IAudioEndpointVolumeCallback_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IAudioEndpointVolumeCallback_OnNotify(This,pNotify)	\
    ( (This)->lpVtbl -> OnNotify(This,pNotify) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IAudioEndpointVolumeCallback_INTERFACE_DEFINED__ */


#ifndef __IAudioEndpointVolume_INTERFACE_DEFINED__
#define __IAudioEndpointVolume_INTERFACE_DEFINED__

/* interface IAudioEndpointVolume */
/* [unique][helpstring][nonextensible][uuid][local][object] */ 


EXTERN_C const IID IID_IAudioEndpointVolume;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("5CDF2C82-841E-4546-9722-0CF74078229A")
    IAudioEndpointVolume : public IUnknown
    {
    public:
        virtual /* [helpstring] */ HRESULT STDMETHODCALLTYPE RegisterControlChangeNotify( 
            /* [in] */ 
            __in  IAudioEndpointVolumeCallback *pNotify) = 0;
        
        virtual /* [helpstring] */ HRESULT STDMETHODCALLTYPE UnregisterControlChangeNotify( 
            /* [in] */ 
            __in  IAudioEndpointVolumeCallback *pNotify) = 0;
        
        virtual /* [helpstring] */ HRESULT STDMETHODCALLTYPE GetChannelCount( 
            /* [out] */ 
            __out  UINT *pnChannelCount) = 0;
        
        virtual /* [helpstring] */ HRESULT STDMETHODCALLTYPE SetMasterVolumeLevel( 
            /* [in] */ 
            __in  float fLevelDB,
            /* [unique][in] */ LPCGUID pguidEventContext) = 0;
        
        virtual /* [helpstring] */ HRESULT STDMETHODCALLTYPE SetMasterVolumeLevelScalar( 
            /* [in] */ 
            __in  float fLevel,
            /* [unique][in] */ LPCGUID pguidEventContext) = 0;
        
        virtual /* [helpstring] */ HRESULT STDMETHODCALLTYPE GetMasterVolumeLevel( 
            /* [out] */ 
            __out  float *pfLevelDB) = 0;
        
        virtual /* [helpstring] */ HRESULT STDMETHODCALLTYPE GetMasterVolumeLevelScalar( 
            /* [out] */ 
            __out  float *pfLevel) = 0;
        
        virtual /* [helpstring] */ HRESULT STDMETHODCALLTYPE SetChannelVolumeLevel( 
            /* [in] */ 
            __in  UINT nChannel,
            float fLevelDB,
            /* [unique][in] */ LPCGUID pguidEventContext) = 0;
        
        virtual /* [helpstring] */ HRESULT STDMETHODCALLTYPE SetChannelVolumeLevelScalar( 
            /* [in] */ 
            __in  UINT nChannel,
            float fLevel,
            /* [unique][in] */ LPCGUID pguidEventContext) = 0;
        
        virtual /* [helpstring] */ HRESULT STDMETHODCALLTYPE GetChannelVolumeLevel( 
            /* [in] */ 
            __in  UINT nChannel,
            /* [out] */ 
            __out  float *pfLevelDB) = 0;
        
        virtual /* [helpstring] */ HRESULT STDMETHODCALLTYPE GetChannelVolumeLevelScalar( 
            /* [in] */ 
            __in  UINT nChannel,
            /* [out] */ 
            __out  float *pfLevel) = 0;
        
        virtual /* [helpstring] */ HRESULT STDMETHODCALLTYPE SetMute( 
            /* [in] */ 
            __in  BOOL bMute,
            /* [unique][in] */ LPCGUID pguidEventContext) = 0;
        
        virtual /* [helpstring] */ HRESULT STDMETHODCALLTYPE GetMute( 
            /* [out] */ 
            __out  BOOL *pbMute) = 0;
        
        virtual /* [helpstring] */ HRESULT STDMETHODCALLTYPE GetVolumeStepInfo( 
            /* [out] */ 
            __out  UINT *pnStep,
            /* [out] */ 
            __out  UINT *pnStepCount) = 0;
        
        virtual /* [helpstring] */ HRESULT STDMETHODCALLTYPE VolumeStepUp( 
            /* [unique][in] */ LPCGUID pguidEventContext) = 0;
        
        virtual /* [helpstring] */ HRESULT STDMETHODCALLTYPE VolumeStepDown( 
            /* [unique][in] */ LPCGUID pguidEventContext) = 0;
        
        virtual /* [helpstring] */ HRESULT STDMETHODCALLTYPE QueryHardwareSupport( 
            /* [out] */ 
            __out  DWORD *pdwHardwareSupportMask) = 0;
        
        virtual /* [helpstring] */ HRESULT STDMETHODCALLTYPE GetVolumeRange( 
            /* [out] */ 
            __out  float *pflVolumeMindB,
            /* [out] */ 
            __out  float *pflVolumeMaxdB,
            /* [out] */ 
            __out  float *pflVolumeIncrementdB) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IAudioEndpointVolumeVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IAudioEndpointVolume * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ 
            __RPC__deref_out  void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IAudioEndpointVolume * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IAudioEndpointVolume * This);
        
        /* [helpstring] */ HRESULT ( STDMETHODCALLTYPE *RegisterControlChangeNotify )( 
            IAudioEndpointVolume * This,
            /* [in] */ 
            __in  IAudioEndpointVolumeCallback *pNotify);
        
        /* [helpstring] */ HRESULT ( STDMETHODCALLTYPE *UnregisterControlChangeNotify )( 
            IAudioEndpointVolume * This,
            /* [in] */ 
            __in  IAudioEndpointVolumeCallback *pNotify);
        
        /* [helpstring] */ HRESULT ( STDMETHODCALLTYPE *GetChannelCount )( 
            IAudioEndpointVolume * This,
            /* [out] */ 
            __out  UINT *pnChannelCount);
        
        /* [helpstring] */ HRESULT ( STDMETHODCALLTYPE *SetMasterVolumeLevel )( 
            IAudioEndpointVolume * This,
            /* [in] */ 
            __in  float fLevelDB,
            /* [unique][in] */ LPCGUID pguidEventContext);
        
        /* [helpstring] */ HRESULT ( STDMETHODCALLTYPE *SetMasterVolumeLevelScalar )( 
            IAudioEndpointVolume * This,
            /* [in] */ 
            __in  float fLevel,
            /* [unique][in] */ LPCGUID pguidEventContext);
        
        /* [helpstring] */ HRESULT ( STDMETHODCALLTYPE *GetMasterVolumeLevel )( 
            IAudioEndpointVolume * This,
            /* [out] */ 
            __out  float *pfLevelDB);
        
        /* [helpstring] */ HRESULT ( STDMETHODCALLTYPE *GetMasterVolumeLevelScalar )( 
            IAudioEndpointVolume * This,
            /* [out] */ 
            __out  float *pfLevel);
        
        /* [helpstring] */ HRESULT ( STDMETHODCALLTYPE *SetChannelVolumeLevel )( 
            IAudioEndpointVolume * This,
            /* [in] */ 
            __in  UINT nChannel,
            float fLevelDB,
            /* [unique][in] */ LPCGUID pguidEventContext);
        
        /* [helpstring] */ HRESULT ( STDMETHODCALLTYPE *SetChannelVolumeLevelScalar )( 
            IAudioEndpointVolume * This,
            /* [in] */ 
            __in  UINT nChannel,
            float fLevel,
            /* [unique][in] */ LPCGUID pguidEventContext);
        
        /* [helpstring] */ HRESULT ( STDMETHODCALLTYPE *GetChannelVolumeLevel )( 
            IAudioEndpointVolume * This,
            /* [in] */ 
            __in  UINT nChannel,
            /* [out] */ 
            __out  float *pfLevelDB);
        
        /* [helpstring] */ HRESULT ( STDMETHODCALLTYPE *GetChannelVolumeLevelScalar )( 
            IAudioEndpointVolume * This,
            /* [in] */ 
            __in  UINT nChannel,
            /* [out] */ 
            __out  float *pfLevel);
        
        /* [helpstring] */ HRESULT ( STDMETHODCALLTYPE *SetMute )( 
            IAudioEndpointVolume * This,
            /* [in] */ 
            __in  BOOL bMute,
            /* [unique][in] */ LPCGUID pguidEventContext);
        
        /* [helpstring] */ HRESULT ( STDMETHODCALLTYPE *GetMute )( 
            IAudioEndpointVolume * This,
            /* [out] */ 
            __out  BOOL *pbMute);
        
        /* [helpstring] */ HRESULT ( STDMETHODCALLTYPE *GetVolumeStepInfo )( 
            IAudioEndpointVolume * This,
            /* [out] */ 
            __out  UINT *pnStep,
            /* [out] */ 
            __out  UINT *pnStepCount);
        
        /* [helpstring] */ HRESULT ( STDMETHODCALLTYPE *VolumeStepUp )( 
            IAudioEndpointVolume * This,
            /* [unique][in] */ LPCGUID pguidEventContext);
        
        /* [helpstring] */ HRESULT ( STDMETHODCALLTYPE *VolumeStepDown )( 
            IAudioEndpointVolume * This,
            /* [unique][in] */ LPCGUID pguidEventContext);
        
        /* [helpstring] */ HRESULT ( STDMETHODCALLTYPE *QueryHardwareSupport )( 
            IAudioEndpointVolume * This,
            /* [out] */ 
            __out  DWORD *pdwHardwareSupportMask);
        
        /* [helpstring] */ HRESULT ( STDMETHODCALLTYPE *GetVolumeRange )( 
            IAudioEndpointVolume * This,
            /* [out] */ 
            __out  float *pflVolumeMindB,
            /* [out] */ 
            __out  float *pflVolumeMaxdB,
            /* [out] */ 
            __out  float *pflVolumeIncrementdB);
        
        END_INTERFACE
    } IAudioEndpointVolumeVtbl;

    interface IAudioEndpointVolume
    {
        CONST_VTBL struct IAudioEndpointVolumeVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IAudioEndpointVolume_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IAudioEndpointVolume_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IAudioEndpointVolume_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IAudioEndpointVolume_RegisterControlChangeNotify(This,pNotify)	\
    ( (This)->lpVtbl -> RegisterControlChangeNotify(This,pNotify) ) 

#define IAudioEndpointVolume_UnregisterControlChangeNotify(This,pNotify)	\
    ( (This)->lpVtbl -> UnregisterControlChangeNotify(This,pNotify) ) 

#define IAudioEndpointVolume_GetChannelCount(This,pnChannelCount)	\
    ( (This)->lpVtbl -> GetChannelCount(This,pnChannelCount) ) 

#define IAudioEndpointVolume_SetMasterVolumeLevel(This,fLevelDB,pguidEventContext)	\
    ( (This)->lpVtbl -> SetMasterVolumeLevel(This,fLevelDB,pguidEventContext) ) 

#define IAudioEndpointVolume_SetMasterVolumeLevelScalar(This,fLevel,pguidEventContext)	\
    ( (This)->lpVtbl -> SetMasterVolumeLevelScalar(This,fLevel,pguidEventContext) ) 

#define IAudioEndpointVolume_GetMasterVolumeLevel(This,pfLevelDB)	\
    ( (This)->lpVtbl -> GetMasterVolumeLevel(This,pfLevelDB) ) 

#define IAudioEndpointVolume_GetMasterVolumeLevelScalar(This,pfLevel)	\
    ( (This)->lpVtbl -> GetMasterVolumeLevelScalar(This,pfLevel) ) 

#define IAudioEndpointVolume_SetChannelVolumeLevel(This,nChannel,fLevelDB,pguidEventContext)	\
    ( (This)->lpVtbl -> SetChannelVolumeLevel(This,nChannel,fLevelDB,pguidEventContext) ) 

#define IAudioEndpointVolume_SetChannelVolumeLevelScalar(This,nChannel,fLevel,pguidEventContext)	\
    ( (This)->lpVtbl -> SetChannelVolumeLevelScalar(This,nChannel,fLevel,pguidEventContext) ) 

#define IAudioEndpointVolume_GetChannelVolumeLevel(This,nChannel,pfLevelDB)	\
    ( (This)->lpVtbl -> GetChannelVolumeLevel(This,nChannel,pfLevelDB) ) 

#define IAudioEndpointVolume_GetChannelVolumeLevelScalar(This,nChannel,pfLevel)	\
    ( (This)->lpVtbl -> GetChannelVolumeLevelScalar(This,nChannel,pfLevel) ) 

#define IAudioEndpointVolume_SetMute(This,bMute,pguidEventContext)	\
    ( (This)->lpVtbl -> SetMute(This,bMute,pguidEventContext) ) 

#define IAudioEndpointVolume_GetMute(This,pbMute)	\
    ( (This)->lpVtbl -> GetMute(This,pbMute) ) 

#define IAudioEndpointVolume_GetVolumeStepInfo(This,pnStep,pnStepCount)	\
    ( (This)->lpVtbl -> GetVolumeStepInfo(This,pnStep,pnStepCount) ) 

#define IAudioEndpointVolume_VolumeStepUp(This,pguidEventContext)	\
    ( (This)->lpVtbl -> VolumeStepUp(This,pguidEventContext) ) 

#define IAudioEndpointVolume_VolumeStepDown(This,pguidEventContext)	\
    ( (This)->lpVtbl -> VolumeStepDown(This,pguidEventContext) ) 

#define IAudioEndpointVolume_QueryHardwareSupport(This,pdwHardwareSupportMask)	\
    ( (This)->lpVtbl -> QueryHardwareSupport(This,pdwHardwareSupportMask) ) 

#define IAudioEndpointVolume_GetVolumeRange(This,pflVolumeMindB,pflVolumeMaxdB,pflVolumeIncrementdB)	\
    ( (This)->lpVtbl -> GetVolumeRange(This,pflVolumeMindB,pflVolumeMaxdB,pflVolumeIncrementdB) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IAudioEndpointVolume_INTERFACE_DEFINED__ */


#ifndef __IAudioMeterInformation_INTERFACE_DEFINED__
#define __IAudioMeterInformation_INTERFACE_DEFINED__

/* interface IAudioMeterInformation */
/* [unique][helpstring][nonextensible][uuid][local][object] */ 


EXTERN_C const IID IID_IAudioMeterInformation;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C02216F6-8C67-4B5B-9D00-D008E73E0064")
    IAudioMeterInformation : public IUnknown
    {
    public:
        virtual /* [helpstring] */ HRESULT STDMETHODCALLTYPE GetPeakValue( 
            /* [out] */ float *pfPeak) = 0;
        
        virtual /* [helpstring] */ HRESULT STDMETHODCALLTYPE GetMeteringChannelCount( 
            /* [out] */ 
            __out  UINT *pnChannelCount) = 0;
        
        virtual /* [helpstring] */ HRESULT STDMETHODCALLTYPE GetChannelsPeakValues( 
            /* [in] */ UINT32 u32ChannelCount,
            /* [size_is][out] */ float *afPeakValues) = 0;
        
        virtual /* [helpstring] */ HRESULT STDMETHODCALLTYPE QueryHardwareSupport( 
            /* [out] */ 
            __out  DWORD *pdwHardwareSupportMask) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IAudioMeterInformationVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IAudioMeterInformation * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ 
            __RPC__deref_out  void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IAudioMeterInformation * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IAudioMeterInformation * This);
        
        /* [helpstring] */ HRESULT ( STDMETHODCALLTYPE *GetPeakValue )( 
            IAudioMeterInformation * This,
            /* [out] */ float *pfPeak);
        
        /* [helpstring] */ HRESULT ( STDMETHODCALLTYPE *GetMeteringChannelCount )( 
            IAudioMeterInformation * This,
            /* [out] */ 
            __out  UINT *pnChannelCount);
        
        /* [helpstring] */ HRESULT ( STDMETHODCALLTYPE *GetChannelsPeakValues )( 
            IAudioMeterInformation * This,
            /* [in] */ UINT32 u32ChannelCount,
            /* [size_is][out] */ float *afPeakValues);
        
        /* [helpstring] */ HRESULT ( STDMETHODCALLTYPE *QueryHardwareSupport )( 
            IAudioMeterInformation * This,
            /* [out] */ 
            __out  DWORD *pdwHardwareSupportMask);
        
        END_INTERFACE
    } IAudioMeterInformationVtbl;

    interface IAudioMeterInformation
    {
        CONST_VTBL struct IAudioMeterInformationVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IAudioMeterInformation_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IAudioMeterInformation_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IAudioMeterInformation_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IAudioMeterInformation_GetPeakValue(This,pfPeak)	\
    ( (This)->lpVtbl -> GetPeakValue(This,pfPeak) ) 

#define IAudioMeterInformation_GetMeteringChannelCount(This,pnChannelCount)	\
    ( (This)->lpVtbl -> GetMeteringChannelCount(This,pnChannelCount) ) 

#define IAudioMeterInformation_GetChannelsPeakValues(This,u32ChannelCount,afPeakValues)	\
    ( (This)->lpVtbl -> GetChannelsPeakValues(This,u32ChannelCount,afPeakValues) ) 

#define IAudioMeterInformation_QueryHardwareSupport(This,pdwHardwareSupportMask)	\
    ( (This)->lpVtbl -> QueryHardwareSupport(This,pdwHardwareSupportMask) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IAudioMeterInformation_INTERFACE_DEFINED__ */


/* Additional Prototypes for ALL interfaces */

/* end of Additional Prototypes */

#ifdef __cplusplus
}
#endif

#endif



