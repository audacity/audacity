#
# Audacity: A Digital Audio Editor
#
declare_module(vst_sdk_3)

if(MUSE_MODULE_VST_VST3_SDK_PATH)
    set(VST3_SDK_PATH ${MUSE_MODULE_VST_VST3_SDK_PATH})
else()

    # If not set MUSE_MODULE_VST_VST3_SDK_PATH
    # download vst3sdk source

    set(REMOTE_ROOT_URL https://raw.githubusercontent.com/musescore/muse_deps/main)
    set(remote_url ${REMOTE_ROOT_URL}/vst3sdk/v3.7.12_build_20)
    set(local_path ${PROJECT_BINARY_DIR}/_deps/vst3sdk)
    if (NOT EXISTS ${local_path}/vst3sdk.cmake)
        file(MAKE_DIRECTORY ${local_path})
        file(DOWNLOAD ${remote_url}/vst3sdk.cmake ${local_path}/vst3sdk.cmake
            HTTPHEADER "Cache-Control: no-cache"
        )
    endif()

    include(${local_path}/vst3sdk.cmake)

    # func from ${name}.cmake)
    cmake_language(CALL vst3sdk_Populate ${remote_url} ${local_path} "source" "" "")

    set(VST3_SDK_PATH ${local_path})

endif()

include(GetPlatformInfo)
include(GetCompilerInfo)
include(GetBuildType)

if (BUILD_IS_DEBUG)
    set(MODULE_DEF -DDEVELOPMENT)
else()
    set(MODULE_DEF -DRELEASE)
endif()

set(MODULE_USE_UNITY_NONE ON)

set(MODULE_USE_PCH_DISABLED ON)

if (OS_IS_WIN)
    set(PLATFORM_SRC
        ${VST3_SDK_PATH}/public.sdk/source/vst/hosting/module_win32.cpp
        ${VST3_SDK_PATH}/public.sdk/source/common/threadchecker_win32.cpp
        ${VST3_SDK_PATH}/public.sdk/source/common/systemclipboard_win32.cpp
        ${VST3_SDK_PATH}/public.sdk/source/common/systemclipboard.h
        ${VST3_SDK_PATH}/public.sdk/source/common/commonstringconvert.cpp
        ${VST3_SDK_PATH}/public.sdk/source/common/commonstringconvert.h
        )
elseif(OS_IS_LIN)
    set(PLATFORM_SRC
        ${VST3_SDK_PATH}/public.sdk/source/vst/hosting/module_linux.cpp
        ${VST3_SDK_PATH}/public.sdk/source/common/threadchecker_linux.cpp
        )
elseif(OS_IS_MAC)
    set(PLATFORM_SRC
        ${VST3_SDK_PATH}/public.sdk/source/vst/hosting/module_mac.mm
        ${VST3_SDK_PATH}/public.sdk/source/common/threadchecker_mac.mm
        )
endif()

set(VST_BASE_SRC
    ${VST3_SDK_PATH}/base/source/baseiids.cpp
    ${VST3_SDK_PATH}/base/source/classfactoryhelpers.h
    ${VST3_SDK_PATH}/base/source/fbuffer.cpp
    ${VST3_SDK_PATH}/base/source/fbuffer.h
    ${VST3_SDK_PATH}/base/source/fcleanup.h
    ${VST3_SDK_PATH}/base/source/fcommandline.h
    ${VST3_SDK_PATH}/base/source/fdebug.cpp
    ${VST3_SDK_PATH}/base/source/fdebug.h
    ${VST3_SDK_PATH}/base/source/fdynlib.cpp
    ${VST3_SDK_PATH}/base/source/fdynlib.h
    ${VST3_SDK_PATH}/base/source/fobject.cpp
    ${VST3_SDK_PATH}/base/source/fobject.h
    ${VST3_SDK_PATH}/base/source/fstreamer.cpp
    ${VST3_SDK_PATH}/base/source/fstreamer.h
    ${VST3_SDK_PATH}/base/source/fstring.cpp
    ${VST3_SDK_PATH}/base/source/fstring.h
    ${VST3_SDK_PATH}/base/source/updatehandler.cpp
    ${VST3_SDK_PATH}/base/source/updatehandler.h
    ${VST3_SDK_PATH}/base/source/timer.cpp
    ${VST3_SDK_PATH}/base/source/timer.h
    ${VST3_SDK_PATH}/base/thread/include/fcondition.h
    ${VST3_SDK_PATH}/base/thread/include/flock.h
    ${VST3_SDK_PATH}/base/thread/source/fcondition.cpp
    ${VST3_SDK_PATH}/base/thread/source/flock.cpp
    )

set(VST_PLUGIN_INTERFACES_SRC
    ${VST3_SDK_PATH}/pluginterfaces/base/conststringtable.cpp
    ${VST3_SDK_PATH}/pluginterfaces/base/conststringtable.h
    ${VST3_SDK_PATH}/pluginterfaces/base/coreiids.cpp
    ${VST3_SDK_PATH}/pluginterfaces/base/falignpop.h
    ${VST3_SDK_PATH}/pluginterfaces/base/falignpush.h
    ${VST3_SDK_PATH}/pluginterfaces/base/fplatform.h
    ${VST3_SDK_PATH}/pluginterfaces/base/fstrdefs.h
    ${VST3_SDK_PATH}/pluginterfaces/base/ftypes.h
    ${VST3_SDK_PATH}/pluginterfaces/base/funknown.cpp
    ${VST3_SDK_PATH}/pluginterfaces/base/funknown.h
    ${VST3_SDK_PATH}/pluginterfaces/base/futils.h
    ${VST3_SDK_PATH}/pluginterfaces/base/fvariant.h
    ${VST3_SDK_PATH}/pluginterfaces/base/geoconstants.h
    ${VST3_SDK_PATH}/pluginterfaces/base/ibstream.h
    ${VST3_SDK_PATH}/pluginterfaces/base/icloneable.h
    ${VST3_SDK_PATH}/pluginterfaces/base/ierrorcontext.h
    ${VST3_SDK_PATH}/pluginterfaces/base/ipersistent.h
    ${VST3_SDK_PATH}/pluginterfaces/base/ipluginbase.h
    ${VST3_SDK_PATH}/pluginterfaces/base/istringresult.h
    ${VST3_SDK_PATH}/pluginterfaces/base/iupdatehandler.h
    ${VST3_SDK_PATH}/pluginterfaces/base/keycodes.h
    ${VST3_SDK_PATH}/pluginterfaces/base/pluginbasefwd.h
    ${VST3_SDK_PATH}/pluginterfaces/base/smartpointer.h
    ${VST3_SDK_PATH}/pluginterfaces/base/typesizecheck.h
    ${VST3_SDK_PATH}/pluginterfaces/base/ucolorspec.h
    ${VST3_SDK_PATH}/pluginterfaces/base/ustring.cpp
    ${VST3_SDK_PATH}/pluginterfaces/base/ustring.h
    ${VST3_SDK_PATH}/pluginterfaces/vst/ivstmidicontrollers.h
    )

set(VST_SDK_COMMON_SRC
    ${VST3_SDK_PATH}/public.sdk/source/common/commoniids.cpp
    ${VST3_SDK_PATH}/public.sdk/source/common/commonstringconvert.cpp
    ${VST3_SDK_PATH}/public.sdk/source/common/commonstringconvert.h
    ${VST3_SDK_PATH}/public.sdk/source/common/openurl.cpp
    ${VST3_SDK_PATH}/public.sdk/source/common/openurl.h
    ${VST3_SDK_PATH}/public.sdk/source/common/threadchecker.h
    ${VST3_SDK_PATH}/public.sdk/source/common/pluginview.cpp
    ${VST3_SDK_PATH}/public.sdk/source/common/pluginview.h
    ${VST3_SDK_PATH}/public.sdk/source/common/memorystream.cpp
    ${VST3_SDK_PATH}/public.sdk/source/common/memorystream.h
    ${VST3_SDK_PATH}/public.sdk/source/main/pluginfactory.cpp
    ${VST3_SDK_PATH}/public.sdk/source/main/pluginfactory.h
    ${VST3_SDK_PATH}/public.sdk/source/vst/vstpresetfile.cpp
    ${VST3_SDK_PATH}/public.sdk/source/vst/vstpresetfile.h
    )

set(VST_SDK_HOSTING_SRC
    ${VST3_SDK_PATH}/public.sdk/source/vst/hosting/connectionproxy.cpp
    ${VST3_SDK_PATH}/public.sdk/source/vst/hosting/connectionproxy.h
    ${VST3_SDK_PATH}/public.sdk/source/vst/hosting/eventlist.cpp
    ${VST3_SDK_PATH}/public.sdk/source/vst/hosting/eventlist.h
    ${VST3_SDK_PATH}/public.sdk/source/vst/hosting/hostclasses.cpp
    ${VST3_SDK_PATH}/public.sdk/source/vst/hosting/hostclasses.h
    ${VST3_SDK_PATH}/public.sdk/source/vst/hosting/module.cpp
    ${VST3_SDK_PATH}/public.sdk/source/vst/hosting/module.h
    ${VST3_SDK_PATH}/public.sdk/source/vst/hosting/parameterchanges.cpp
    ${VST3_SDK_PATH}/public.sdk/source/vst/hosting/parameterchanges.h
    ${VST3_SDK_PATH}/public.sdk/source/vst/hosting/pluginterfacesupport.cpp
    ${VST3_SDK_PATH}/public.sdk/source/vst/hosting/pluginterfacesupport.h
    ${VST3_SDK_PATH}/public.sdk/source/vst/hosting/processdata.cpp
    ${VST3_SDK_PATH}/public.sdk/source/vst/hosting/processdata.h
    ${VST3_SDK_PATH}/public.sdk/source/vst/utility/optional.h
    ${VST3_SDK_PATH}/public.sdk/source/vst/utility/stringconvert.cpp
    ${VST3_SDK_PATH}/public.sdk/source/vst/utility/stringconvert.h
    ${VST3_SDK_PATH}/public.sdk/source/vst/utility/uid.h
    ${VST3_SDK_PATH}/public.sdk/source/vst/utility/versionparser.h
    ${VST3_SDK_PATH}/public.sdk/source/vst/vstinitiids.cpp
    ${VST3_SDK_PATH}/public.sdk/source/vst/hosting/plugprovider.cpp
    ${VST3_SDK_PATH}/public.sdk/source/vst/hosting/plugprovider.h
    )

set(MODULE_SRC
    ${VST_BASE_SRC}
    ${VST_PLUGIN_INTERFACES_SRC}
    ${VST_SDK_COMMON_SRC}
    ${VST_SDK_HOSTING_SRC}
    ${PLATFORM_SRC}
    )

set(MODULE_INCLUDE
    ${VST3_SDK_PATH}
    )

set(MODULE_USE_PCH OFF)
set(MODULE_USE_UNITY OFF)

setup_module()

target_no_warning(${MODULE} -w)

if (CC_IS_CLANG)
    set_property(TARGET ${MODULE} APPEND_STRING PROPERTY COMPILE_FLAGS "-fobjc-arc")
endif()
