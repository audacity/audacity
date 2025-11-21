/**********************************************************************

  Audacity: A Digital Audio Editor

  PluginDescriptor.h

  Split from PluginManager.h

**********************************************************************/

#pragma once
#include <wx/string.h>

#include "EffectInterface.h"
#include "PluginInterface.h"
#include "XMLTagHandler.h"
#include "wxArrayStringEx.h"

class XMLWriter;

///////////////////////////////////////////////////////////////////////////////
//
// PluginDescriptor
//
///////////////////////////////////////////////////////////////////////////////

typedef enum : unsigned {
    PluginTypeNone = 0,         // 2.1.0 placeholder entries...not used by 2.1.1 or greater
    PluginTypeStub =1,              // Used for plugins that have not yet been registered
    PluginTypeEffect =1 << 1,
    PluginTypeAudacityCommand=1 << 2,
    PluginTypeExporter=1 << 3,
    PluginTypeImporter=1 << 4,
    PluginTypeModule=1 << 5,
} PluginType;

class PluginDescriptorXMLTagHandler;

// TODO:  Convert this to multiple derived classes
//! Represents either a PluginProvider or a loaded plug-in and caches some
//! information about it
class MODULE_MANAGER_API PluginDescriptor final : public XMLTagHandler
{
public:

    static constexpr auto XMLNodeName { "PluginDescriptor" };

    PluginType GetPluginType() const;

    // All plugins

    // These return untranslated strings
    const wxString& GetID() const;
    const wxString& GetProviderID() const;
    const PluginPath& GetPath() const;
    const ComponentInterfaceSymbol& GetSymbol() const;

    const wxString& GetUntranslatedVersion() const;
    // There is no translated version

    const wxString& GetVendor() const;

    bool IsEnabled() const;
    bool IsValid() const;

    void SetEnabled(bool enable);
    void SetValid(bool valid);

    // Effect plugins only

    // Internal string only, no translated counterpart!
    // (Use Effect::GetFamilyName instead)
    // This string persists in configuration files
    // So config compatibility will break if it is changed across Audacity versions
    wxString GetEffectFamily() const;

    EffectType GetEffectType() const;
    bool IsEffectDefault() const;
    bool IsEffectInteractive() const;
    bool IsEffectLegacy() const;
    bool IsEffectRealtime() const;
    bool IsEffectAutomatable() const;

    // Importer plugins only

    const wxString& GetImporterIdentifier() const;
    const TranslatableString& GetImporterFilterDescription() const;
    const FileExtensions& GetImporterExtensions() const;

    void WriteXML(XMLWriter& writer) const;

    bool HandleXMLTag(const std::string_view& tag, const AttributesList& attrs) override;
    XMLTagHandler* HandleXMLChild(const std::string_view& tag) override;
    void HandleXMLEndTag(const std::string_view&) override;

    void SetPluginType(PluginType type);

    // These should be passed an untranslated value
    void SetID(const PluginID& ID);
    void SetProviderID(const PluginID& providerID);
    void SetPath(const PluginPath& path);
    void SetSymbol(const ComponentInterfaceSymbol& symbol);

    // These should be passed an untranslated value wrapped in XO() so
    // the value will still be extracted for translation
    void SetVersion(const wxString& version);
    void SetVendor(const wxString& vendor);

    // "family" should be an untranslated string wrapped in wxT()
    void SetEffectFamily(const wxString& family);
    void SetEffectType(EffectType type);
    void SetEffectDefault(bool dflt);
    void SetEffectInteractive(bool interactive);
    void SetEffectLegacy(bool legacy);
    void SetRealtimeSupport(EffectDefinitionInterface::RealtimeSince realtime);

    //! for serialization
    wxString SerializeRealtimeSupport() const;
    //! for deserialization
    void DeserializeRealtimeSupport(const wxString& value);

    void SetEffectAutomatable(bool automatable);

    void SetImporterIdentifier(const wxString& identifier);
    void SetImporterFilterDescription(const TranslatableString& filterDesc);
    void SetImporterExtensions(FileExtensions extensions);

private:

    // Common

    PluginType mPluginType { PluginTypeNone };

    wxString mID;
    PluginPath mPath;
    ComponentInterfaceSymbol mSymbol;
    wxString mVersion;
    wxString mVendor;
    wxString mProviderID;
    bool mEnabled { false };
    bool mValid { false };

    // Effects

    wxString mEffectFamily;
    EffectType mEffectType { EffectTypeNone };
    bool mEffectInteractive { false };
    bool mEffectDefault { false };
    bool mEffectLegacy { false };
    EffectDefinitionInterface::RealtimeSince mEffectRealtime {
        EffectDefinitionInterface::RealtimeSince::Never };
    bool mEffectAutomatable { false };

    // Importers

    wxString mImporterIdentifier;
    FileExtensions mImporterExtensions;
};
