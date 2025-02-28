#include "AudacityVst3HostApplication.h"

#include <pluginterfaces/vst/ivstaudioprocessor.h>
#include <pluginterfaces/vst/ivstcomponent.h>
#include <pluginterfaces/vst/ivsteditcontroller.h>
#include <public.sdk/source/vst/hosting/hostclasses.h>
#include <public.sdk/source/vst/utility/stringconvert.h>

#include <algorithm>
#include <memory>
#include <string>
#include <pluginterfaces/vst/ivstunits.h>

using namespace Steinberg;
using namespace Steinberg::Vst;

// This class was copied from VST3 SDK 3.7.3 hostclasses.h
class AudacityHostAttribute final
{
public:
    enum Type
    {
        kInteger,
        kFloat,
        kString,
        kBinary
    };

    explicit AudacityHostAttribute(int64 value)
        : size(0)
        , type(kInteger)
    {
        v.intValue = value;
    }

    explicit AudacityHostAttribute(double value)
        : size(0)
        , type(kFloat)
    {
        v.floatValue = value;
    }

    /** size is in code unit (count of TChar) */
    explicit AudacityHostAttribute(const TChar* value, uint32 sizeInCodeUnit)
        : size(sizeInCodeUnit)
        , type(kString)
    {
        v.stringValue = new TChar[sizeInCodeUnit];
        memcpy(v.stringValue, value, sizeInCodeUnit * sizeof(TChar));
    }

    explicit AudacityHostAttribute(const void* value, uint32 sizeInBytes)
        : size(sizeInBytes)
        , type(kBinary)
    {
        v.binaryValue = new char[sizeInBytes];
        memcpy(v.binaryValue, value, sizeInBytes);
    }

    ~AudacityHostAttribute()
    {
        if (size) {
            delete[] v.binaryValue;
        }
    }

    int64 intValue() const
    {
        return v.intValue;
    }

    double floatValue() const
    {
        return v.floatValue;
    }

    /** sizeInCodeUnit is in code unit (count of TChar) */
    const TChar* stringValue(uint32& sizeInCodeUnit)
    {
        sizeInCodeUnit = size;
        return v.stringValue;
    }

    const void* binaryValue(uint32& sizeInBytes)
    {
        sizeInBytes = size;
        return v.binaryValue;
    }

    Type getType() const
    {
        return type;
    }

protected:
    union v
    {
        int64 intValue;
        double floatValue;
        TChar* stringValue;
        char* binaryValue;
    } v;
    uint32 size;
    Type type;
};
// This class was copied from VST3 SDK 3.7.3 hostclasses.h
class AudacityHostAttributeList : public IAttributeList
{
public:
    AudacityHostAttributeList()
    {
        FUNKNOWN_CTOR
    }

    virtual ~AudacityHostAttributeList() { FUNKNOWN_DTOR }

    tresult PLUGIN_API setInt(AttrID aid, int64 value) SMTG_OVERRIDE
    {
        if (!aid) {
            return kInvalidArgument;
        }
        removeAttrID(aid);
        list[aid] = std::make_unique<AudacityHostAttribute>(value);
        return kResultTrue;
    }

    tresult PLUGIN_API getInt(AttrID aid, int64& value) SMTG_OVERRIDE
    {
        if (!aid) {
            return kInvalidArgument;
        }
        auto it = list.find(aid);
        if (it != list.end() && it->second) {
            value = it->second->intValue();
            return kResultTrue;
        }
        return kResultFalse;
    }

    tresult PLUGIN_API setFloat(AttrID aid, double value) SMTG_OVERRIDE
    {
        if (!aid) {
            return kInvalidArgument;
        }
        removeAttrID(aid);
        list[aid] = std::make_unique<AudacityHostAttribute>(value);
        return kResultTrue;
    }

    tresult PLUGIN_API getFloat(AttrID aid, double& value) SMTG_OVERRIDE
    {
        if (!aid) {
            return kInvalidArgument;
        }
        auto it = list.find(aid);
        if (it != list.end() && it->second) {
            value = it->second->floatValue();
            return kResultTrue;
        }
        return kResultFalse;
    }

    tresult PLUGIN_API setString(AttrID aid, const TChar* string) SMTG_OVERRIDE
    {
        if (!aid) {
            return kInvalidArgument;
        }
        removeAttrID(aid);
        // + 1 for the null-terminate
        auto length = tstrlen(string);
        list[aid] = std::make_unique<AudacityHostAttribute>(string, length + 1);
        return kResultTrue;
    }

    tresult PLUGIN_API getString(AttrID aid, TChar* string, uint32 sizeInBytes)
    SMTG_OVERRIDE
    {
        if (!aid) {
            return kInvalidArgument;
        }

        auto it = list.find(aid);
        if (it != list.end() && it->second) {
            uint32 sizeInCodeUnit = 0;
            const TChar* _string = it->second->stringValue(sizeInCodeUnit);
            memcpy(
                string, _string,
                std::min<uint32>(sizeInCodeUnit * sizeof(TChar), sizeInBytes));
            return kResultTrue;
        }

        return kResultFalse;
    }

    tresult PLUGIN_API
    setBinary(AttrID aid, const void* data, uint32 sizeInBytes) SMTG_OVERRIDE
    {
        if (!aid) {
            return kInvalidArgument;
        }
        removeAttrID(aid);
        list[aid] = std::make_unique<AudacityHostAttribute>(data, sizeInBytes);
        return kResultTrue;
    }

    tresult PLUGIN_API
    getBinary(AttrID aid, const void*& data, uint32& sizeInBytes) SMTG_OVERRIDE
    {
        if (!aid) {
            return kInvalidArgument;
        }
        auto it = list.find(aid);
        if (it != list.end() && it->second) {
            data = it->second->binaryValue(sizeInBytes);
            return kResultTrue;
        }
        sizeInBytes = 0;
        return kResultFalse;
    }

    DECLARE_FUNKNOWN_METHODS
protected:
    void removeAttrID(AttrID aid)
    {
        if (!aid) {
            return;
        }

        auto it = list.find(aid);
        if (it != list.end()) {
            list.erase(it);
        }
    }

    std::map<std::string, std::unique_ptr<AudacityHostAttribute> > list;
};

IMPLEMENT_FUNKNOWN_METHODS(
    AudacityHostAttributeList, IAttributeList, IAttributeList::iid)

class AudacityHostMessage : public IMessage
{
public:
    AudacityHostMessage()
    {
        FUNKNOWN_CTOR
    }

    virtual ~AudacityHostMessage()
    {
        FUNKNOWN_DTOR
    }

    const char* PLUGIN_API getMessageID() SMTG_OVERRIDE
    {
        return mMessageId.c_str();
    }

    void PLUGIN_API setMessageID(const char* messageID) SMTG_OVERRIDE
    {
        if (messageID != nullptr) {
            mMessageId = messageID;
        } else {
            mMessageId.clear();
        }
    }

    IAttributeList* PLUGIN_API getAttributes() SMTG_OVERRIDE
    {
        if (!mAattributeList) {
            mAattributeList = safenew AudacityHostAttributeList;
        }

        return mAattributeList.get();
    }

    DECLARE_FUNKNOWN_METHODS
protected:
    std::string mMessageId;
    IPtr<AudacityHostAttributeList> mAattributeList;
};

IMPLEMENT_FUNKNOWN_METHODS(AudacityHostMessage, IMessage, IMessage::iid)

AudacityVst3HostApplication::AudacityVst3HostApplication() { FUNKNOWN_CTOR }

AudacityVst3HostApplication::~AudacityVst3HostApplication() { FUNKNOWN_DTOR }

Steinberg::Vst::IHostApplication& AudacityVst3HostApplication::Get()
{
    static AudacityVst3HostApplication instance;
    return instance;
}

Steinberg::tresult PLUGIN_API
AudacityVst3HostApplication::queryInterface(const char* _iid, void** obj)
{
    QUERY_INTERFACE(_iid, obj, FUnknown::iid, IHostApplication)
    QUERY_INTERFACE(_iid, obj, IHostApplication::iid, IHostApplication)
    QUERY_INTERFACE(_iid, obj, IPlugInterfaceSupport::iid, IPlugInterfaceSupport)

    * obj = nullptr;
    return Steinberg::kResultFalse;
}

Steinberg::uint32 PLUGIN_API AudacityVst3HostApplication::addRef()
{
    return 1;
}

Steinberg::uint32 PLUGIN_API AudacityVst3HostApplication::release()
{
    return 1;
}

Steinberg::tresult PLUGIN_API
AudacityVst3HostApplication::getName(Steinberg::Vst::String128 name)
{
    return VST3::StringConvert::convert("Audacity VST3 host application", name)
           ? Steinberg::kResultTrue
           : Steinberg::kInternalError;
}

Steinberg::tresult PLUGIN_API AudacityVst3HostApplication::createInstance(
    Steinberg::TUID cid, Steinberg::TUID _iid, void** obj)
{
    using namespace Steinberg;

    FUID classID(FUID::fromTUID(cid));
    FUID interfaceID(FUID::fromTUID(_iid));
    if (classID == Vst::IMessage::iid && interfaceID == Vst::IMessage::iid) {
        *obj = safenew AudacityHostMessage;
        return kResultTrue;
    } else if (
        classID == Vst::IAttributeList::iid
        && interfaceID == Vst::IAttributeList::iid) {
        *obj = safenew AudacityHostAttributeList;
        return kResultTrue;
    }
    *obj = nullptr;
    return kResultFalse;
}

Steinberg::tresult AudacityVst3HostApplication::isPlugInterfaceSupported(
    const Steinberg::TUID _iid)
{
    static auto supportedInterfaces = {
        Steinberg::Vst::IComponent::iid,
        Steinberg::Vst::IAudioProcessor::iid,
        Steinberg::Vst::IEditController::iid,
        Steinberg::Vst::IConnectionPoint::iid,
        Steinberg::Vst::IUnitInfo::iid
    };

    auto uid = Steinberg::FUID::fromTUID(_iid);
    if (
        std::find(supportedInterfaces.begin(), supportedInterfaces.end(), uid)
        != supportedInterfaces.end()) {
        return Steinberg::kResultTrue;
    }
    return Steinberg::kResultFalse;
}
