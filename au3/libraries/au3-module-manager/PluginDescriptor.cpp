/**********************************************************************

  Audacity: A Digital Audio Editor

  PluginDescriptor.cpp

  Split from PluginManager.cpp

**********************************************************************/

///////////////////////////////////////////////////////////////////////////////
//
// Plugindescriptor
//
///////////////////////////////////////////////////////////////////////////////

#include "PluginDescriptor.h"

#include "EffectInterface.h"
#include "ModuleManager.h"
#include "XMLWriter.h"

namespace {
constexpr auto AttrID = "id";
constexpr auto AttrPath = "path";
constexpr auto AttrName = "name";
constexpr auto AttrVendor = "vendor";
constexpr auto AttrVersion = "version";
constexpr auto AttrEffectFamily = "effect_family";
constexpr auto AttrProviderID = "provider";

constexpr auto AttrType = "type";
constexpr auto AttrEnabled = "enabled";
constexpr auto AttrValid = "valid";

constexpr auto AttrEffectType = "effect_type";
constexpr auto AttrEffectDefault = "effect_default";
constexpr auto AttrEffectRealtime = "effect_realtime";
constexpr auto AttrEffectAutomatable = "effect_automatable";
constexpr auto AttrEffectInteractive = "effect_interactive";
}

PluginType PluginDescriptor::GetPluginType() const
{
    return mPluginType;
}

const PluginID& PluginDescriptor::GetID() const
{
    return mID;
}

const PluginID& PluginDescriptor::GetProviderID() const
{
    return mProviderID;
}

const PluginPath& PluginDescriptor::GetPath() const
{
    return mPath;
}

const ComponentInterfaceSymbol& PluginDescriptor::GetSymbol() const
{
    return mSymbol;
}

const wxString& PluginDescriptor::GetUntranslatedVersion() const
{
    return mVersion;
}

const wxString& PluginDescriptor::GetVendor() const
{
    return mVendor;
}

bool PluginDescriptor::IsEnabled() const
{
    return mEnabled;
}

bool PluginDescriptor::IsValid() const
{
    return mValid;
}

void PluginDescriptor::SetPluginType(PluginType type)
{
    mPluginType = type;
}

void PluginDescriptor::SetID(const PluginID& ID)
{
    mID = ID;
}

void PluginDescriptor::SetProviderID(const PluginID& providerID)
{
    mProviderID = providerID;
}

void PluginDescriptor::SetPath(const PluginPath& path)
{
    mPath = path;
}

void PluginDescriptor::SetSymbol(const ComponentInterfaceSymbol& symbol)
{
    mSymbol = symbol;
}

void PluginDescriptor::SetVersion(const wxString& version)
{
    mVersion = version;
}

void PluginDescriptor::SetVendor(const wxString& vendor)
{
    mVendor = vendor;
}

void PluginDescriptor::SetEnabled(bool enable)
{
    mEnabled = enable;
}

void PluginDescriptor::SetValid(bool valid)
{
    mValid = valid;
}

// Effects

wxString PluginDescriptor::GetEffectFamily() const
{
    return mEffectFamily;
}

EffectType PluginDescriptor::GetEffectType() const
{
    return mEffectType;
}

bool PluginDescriptor::IsEffectInteractive() const
{
    return mEffectInteractive;
}

bool PluginDescriptor::IsEffectDefault() const
{
    return mEffectDefault;
}

bool PluginDescriptor::IsEffectLegacy() const
{
    return mEffectLegacy;
}

bool PluginDescriptor::IsEffectRealtime() const
{
    return mEffectRealtime != EffectDefinitionInterface::RealtimeSince::Never;
}

bool PluginDescriptor::IsEffectAutomatable() const
{
    return mEffectAutomatable;
}

void PluginDescriptor::SetEffectFamily(const wxString& family)
{
    mEffectFamily = family;
}

void PluginDescriptor::SetEffectType(EffectType type)
{
    mEffectType = type;
}

void PluginDescriptor::SetEffectInteractive(bool interactive)
{
    mEffectInteractive = interactive;
}

void PluginDescriptor::SetEffectDefault(bool dflt)
{
    mEffectDefault = dflt;
}

void PluginDescriptor::SetEffectLegacy(bool legacy)
{
    mEffectLegacy = legacy;
}

void PluginDescriptor::SetRealtimeSupport(
    EffectDefinitionInterface::RealtimeSince realtime)
{
    mEffectRealtime = realtime;
}

static constexpr auto After_3_1_string = "00";

wxString PluginDescriptor::SerializeRealtimeSupport() const
{
    // Write a string value that converts to 0 or 1, therefore to a boolean,
    // when read as a boolean from a config file by Audacity 3.1 or earlier
    switch (mEffectRealtime) {
    case EffectDefinitionInterface::RealtimeSince::Never:
    default:
        // A value that earlier Audacity interprets as false
        return "0";
    case EffectDefinitionInterface::RealtimeSince::After_3_1:
        // A different value that earlier Audacity interprets as false
        return After_3_1_string;
    case EffectDefinitionInterface::RealtimeSince::Always:
        // A value that earlier Audacity interprets as true
        return "1";
    }
}

void PluginDescriptor::DeserializeRealtimeSupport(const wxString& value)
{
    // Interpret the values stored by SerializeRealtimeSupport, or by previous
    // versions of Audacity
    if (value == After_3_1_string) {
        mEffectRealtime = EffectDefinitionInterface::RealtimeSince::After_3_1;
    } else {
        // This leaves some open-endedness for future versions of Audacity to
        // define other string values they interpret one way, but we interpret
        // otherwise
        long number;
        value.ToLong(&number);
        mEffectRealtime = number
                          ? EffectDefinitionInterface::RealtimeSince::Always
                          : EffectDefinitionInterface::RealtimeSince::Never;
    }
}

void PluginDescriptor::SetEffectAutomatable(bool automatable)
{
    mEffectAutomatable = automatable;
}

// Importer

const wxString& PluginDescriptor::GetImporterIdentifier() const
{
    return mImporterIdentifier;
}

void PluginDescriptor::SetImporterIdentifier(const wxString& identifier)
{
    mImporterIdentifier = identifier;
}

const FileExtensions& PluginDescriptor::GetImporterExtensions()
const
{
    return mImporterExtensions;
}

void PluginDescriptor::WriteXML(XMLWriter& writer) const
{
    writer.StartTag(XMLNodeName);
    writer.WriteAttr(AttrID, GetID());
    writer.WriteAttr(AttrType, static_cast<int>(GetPluginType()));
    writer.WriteAttr(AttrEnabled, IsEnabled());
    writer.WriteAttr(AttrValid, IsValid());
    writer.WriteAttr(AttrProviderID, GetProviderID());
    writer.WriteAttr(AttrPath, GetPath());
    writer.WriteAttr(AttrName, GetSymbol().Internal());
    writer.WriteAttr(AttrVendor, GetVendor());
    writer.WriteAttr(AttrVersion, GetUntranslatedVersion());
    if (GetPluginType() == PluginTypeEffect) {
        writer.WriteAttr(AttrEffectFamily, GetEffectFamily());
        writer.WriteAttr(AttrEffectType, GetEffectType());
        writer.WriteAttr(AttrEffectDefault, IsEffectDefault());
        writer.WriteAttr(AttrEffectRealtime, SerializeRealtimeSupport());
        writer.WriteAttr(AttrEffectAutomatable, IsEffectAutomatable());
        writer.WriteAttr(AttrEffectInteractive, IsEffectInteractive());
    }
    writer.EndTag(XMLNodeName);
}

bool PluginDescriptor::HandleXMLTag(const std::string_view& tag, const AttributesList& attrs)
{
    if (tag == XMLNodeName) {
        for (auto& p : attrs) {
            auto key = wxString(p.first.data(), p.first.length());
            auto& attr = p.second;
            if (key == AttrType) {
                SetPluginType(static_cast<PluginType>(attr.Get<int>()));
            } else if (key == AttrEffectType) {
                SetEffectType(static_cast<EffectType>(attr.Get<int>()));
            } else if (key == AttrEffectDefault) {
                SetEffectDefault(attr.Get<bool>());
            } else if (key == AttrEffectRealtime) {
                DeserializeRealtimeSupport(attr.ToWString());
            } else if (key == AttrEffectAutomatable) {
                SetEffectAutomatable(attr.Get<bool>());
            } else if (key == AttrEffectInteractive) {
                SetEffectInteractive(attr.Get<bool>());
            } else if (key == AttrEnabled) {
                SetEnabled(attr.Get<bool>());
            } else if (key == AttrValid) {
                SetValid(attr.Get<bool>());
            } else if (key == AttrID) {
                SetID(attr.ToWString());
            } else if (key == AttrPath) {
                SetPath(attr.ToWString());
            } else if (key == AttrName) {
                SetSymbol(attr.ToWString());
            } else if (key == AttrVendor) {
                SetVendor(attr.ToWString());
            } else if (key == AttrVersion) {
                SetVersion(attr.ToWString());
            } else if (key == AttrEffectFamily) {
                SetEffectFamily(attr.ToWString());
            } else if (key == AttrProviderID) {
                SetProviderID(attr.ToWString());
            }
        }
        return true;
    }
    return false;
}

XMLTagHandler* PluginDescriptor::HandleXMLChild(const std::string_view& tag)
{
    return nullptr;
}

void PluginDescriptor::HandleXMLEndTag(const std::string_view& basic_string_view)
{
}

void PluginDescriptor::SetImporterExtensions(FileExtensions extensions)
{
    mImporterExtensions = std::move(extensions);
}
