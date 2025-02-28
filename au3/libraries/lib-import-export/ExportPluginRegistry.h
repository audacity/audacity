#pragma once

#include <memory>
#include <functional>

#include <wx/string.h>

#include "Registry.h"

class ExportPlugin;
struct FormatInfo;

class IMPORT_EXPORT_API ExportPluginRegistry final
{
    struct ExportPluginRegistryItem;

    ExportPluginRegistry();

public:

    std::vector<bool>::const_iterator a;

    using ExportPlugins = std::vector<std::unique_ptr<ExportPlugin> >;

    class IMPORT_EXPORT_API ConstIterator final
    {
        ExportPlugins::const_iterator mPluginIt;
        int mFormatIndex;
    public:
        using value_type = const std::tuple<ExportPlugin*, int>;//proxy type

        struct ProxyPtr final
        {
            explicit ProxyPtr(std::remove_const_t<value_type> value)
                : mValue(std::move(value)) { }
            value_type* operator->() const { return &mValue; }
        private:
            value_type mValue;
        };

        using iterator_category = std::input_iterator_tag;
        using difference_type = int;
        using pointer = ProxyPtr;
        using reference = value_type;

        ConstIterator(ExportPlugins::const_iterator pluginIt, int formatIndex)
            : mPluginIt(std::move(pluginIt)), mFormatIndex(formatIndex) { }

        reference operator*() const { return std::make_tuple(mPluginIt->get(), mFormatIndex); }

        pointer operator->() const { return ProxyPtr { std::make_tuple(mPluginIt->get(), mFormatIndex) }; }

        ConstIterator& operator++();

        ConstIterator operator++(int)
        {
            auto prev = *this;
            ++*this;
            return prev;
        }

        friend bool operator==(const ConstIterator& a, const ConstIterator& b)
        {
            return a.mPluginIt == b.mPluginIt && a.mFormatIndex == b.mFormatIndex;
        }

        friend bool operator!=(const ConstIterator& a, const ConstIterator& b)
        {
            return a.mPluginIt != b.mPluginIt || a.mFormatIndex != b.mFormatIndex;
        }
    };

    using Factory
        =std::function< std::unique_ptr< ExportPlugin >() >;

    ExportPluginRegistry(ExportPluginRegistry&) = delete;
    ExportPluginRegistry(ExportPluginRegistry&&) = delete;

    ~ExportPluginRegistry();

    // Objects of this type are statically constructed in files implementing
    // subclasses of ExportPlugin
    // Register factories, not plugin objects themselves, which allows them
    // to have some fresh state variables each time export begins again
    // and to compute translated strings for the current locale
    struct IMPORT_EXPORT_API RegisteredPlugin : Registry::RegisteredItem<ExportPluginRegistryItem>
    {
        RegisteredPlugin(
            const Identifier& id, // an internal string naming the plug-in
            const Factory&, const Registry::Placement& placement = { wxEmptyString, {} });
    };

    static ExportPluginRegistry& Get();

    void Initialize();

    ConstIterator cbegin() const noexcept { return { mPlugins.begin(), 0 }; }
    ConstIterator cend() const noexcept { return { mPlugins.end(), 0 }; }
    ConstIterator begin() const noexcept { return cbegin(); }
    ConstIterator end() const noexcept { return cend(); }

    /**
     * \brief Returns first pair of [exportPlugin, formatIndex], such that:
     * `exportPlugin->GetFormatInfo(formatIndex).format == format`
     */
    std::tuple<ExportPlugin*, int> FindFormat(const wxString& format, bool compareWithCase = false) const;

private:
    struct Traits : Registry::DefaultTraits {
        using LeafTypes = List<ExportPluginRegistryItem>;
    };
    struct IMPORT_EXPORT_API ExportPluginRegistryItem final : Registry::SingleItem {
        static Registry::GroupItem<Traits>& Registry();
        ExportPluginRegistryItem(const Identifier& id, Factory factory);
        Factory mFactory;
    };

    ExportPlugins mPlugins;
};
