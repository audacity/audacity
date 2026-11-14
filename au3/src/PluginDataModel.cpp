/*!*********************************************************************

  Audacity: A Digital Audio Editor

  @file PluginDataModel.cpp
  @author Vitaly Sverchinsky

**********************************************************************/

#include "PluginDataModel.h"

#include "ModuleManager.h"
#include "PluginManager.h"

const void* PluginDataModel::RowIdRoot {};

union RowConv
{
    void* data;
    uint32_t value;
};

uint32_t GetRowIndex(const wxDataViewItem& item)
{
    const RowConv conv { item.GetID() };
    return conv.value & 0x7FFFFFFF;
}

void* MakeRow(uint32_t index)
{
    RowConv conv { nullptr };
    conv.value = index | 0x80000000;
    return conv.data;
}

PluginDataModel::PluginDataModel(int filterEffectCategory,
                                 int filterState,
                                 const wxString& filterType,
                                 const wxString& filterExpr)
    : mFilterType(filterType)
    , mFilterExpr(filterExpr)
    , mFilterState(filterState)
    , mFilterCategory(filterEffectCategory)
{
    auto& pm = PluginManager::Get();
    for (auto& desc : pm.AllPlugins()) {
        if ((desc.GetPluginType() != PluginTypeEffect || desc.GetEffectType() == EffectTypeHidden)
            && desc.GetPluginType() != PluginTypeStub) {
            continue;
        }
        mPluginStateModel.emplace_back(&desc, desc.IsEnabled());
    }
    UpdateFilter();
}

wxDataViewItem PluginDataModel::GetItemForRow(uint32_t row) const
{
    if (row < mIndexFilterMap.size()) {
        return wxDataViewItem { mIndexFilterMap[row] }
    }
    return {};
}

bool PluginDataModel::GetRowForItem(const wxDataViewItem& item, uint32_t& row) const
{
    if (item.IsOk()) {
        row = GetRowIndex(item);
        return row < mIndexFilterMap.size();
    }
    return false;
}

uint32_t PluginDataModel::GetRowCount() const
{
    return mIndexFilterMap.size();
}

bool PluginDataModel::IsEnabled(const wxDataViewItem& item, unsigned col) const
{
    return true;
}

bool PluginDataModel::HasContainerColumns(const wxDataViewItem& item) const
{
    return item.GetID() == RowIdRoot;
}

void PluginDataModel::SetFilterType(const wxString& type)
{
    if (mFilterType != type) {
        mFilterType = type;
        UpdateFilter();
    }
}

wxString PluginDataModel::GetFilterType() const
{
    return mFilterType;
}

void PluginDataModel::SetFilterState(int state)
{
    if (mFilterState != state) {
        mFilterState = state;
        UpdateFilter();
    }
}

int PluginDataModel::GetFilterState() const
{
    return mFilterState;
}

void PluginDataModel::SetFilterExpr(const wxString& expr)
{
    if (mFilterExpr != expr) {
        mFilterExpr = expr;
        UpdateFilter();
    }
}

wxString PluginDataModel::GetFilterExpr() const
{
    return mFilterExpr;
}

void PluginDataModel::SetFilterCategory(int category)
{
    if (mFilterCategory != category) {
        mFilterCategory = category;
        UpdateFilter();
    }
}

int PluginDataModel::GetFilterCategory() const
{
    return mFilterCategory;
}

int PluginDataModel::Compare(const wxDataViewItem& item1, const wxDataViewItem& item2, unsigned column,
                             bool ascending) const
{
    switch (column) {
    case ColumnName:
    case ColumnPath:
    case ColumnType:
    {
        wxVariant value1;
        GetValue(value1, item1, column);
        wxVariant value2;
        GetValue(value2, item2, column);
        const auto result = value1.GetString().CmpNoCase(value2.GetString());
        //Prevent row "jumping" when strings are equal
        if (result == 0) {
            return ascending == wxPtrToUInt(item1.GetID()) > wxPtrToUInt(item2.GetID()) ? 1 : -1;
        }
        return ascending ? result : -result;
    } break;
    default:
        return wxDataViewModel::Compare(item1, item2, column, ascending);
    }
}

void PluginDataModel::ApplyChanges(const std::function<bool(int, int, const wxString&)>& progressUpdateFn,
                                   const std::function<void(const TranslatableString&)>& errorFn)
{
    auto& mm = ModuleManager::Get();
    auto& pm = PluginManager::Get();

    const auto enableCount = std::count_if(
        mPluginStateModel.begin(),
        mPluginStateModel.end(),
        [](const auto& p) { return p.first->GetPluginType() == PluginTypeStub && p.second; }
        );

    auto counter = 0;
    for (const auto& [desc, enabled] : mPluginStateModel) {
        if (desc->GetPluginType() == PluginTypeStub && enabled) {
            if (progressUpdateFn) {
                if (!progressUpdateFn(++counter, enableCount, desc->GetPath())) {
                    break;
                }
            }

            TranslatableString errMsg;
            if (mm.RegisterEffectPlugin(desc->GetProviderID(), desc->GetPath(), errMsg)) {
                pm.UnregisterPlugin(desc->GetProviderID() + wxT("_") + desc->GetPath());
            } else if (errorFn) {
                errorFn(XO("Effect or Command at %s failed to register:\n%s")
                        .Format(desc->GetPath(), !errMsg.empty() ? errMsg : XO("Unknown error")));
            }
        } else {
            desc->SetEnabled(enabled);
        }
    }
    pm.Save();
    pm.NotifyPluginsChanged();
}

unsigned PluginDataModel::GetColumnCount() const
{
    return 4;
}

wxString PluginDataModel::GetColumnType(unsigned col) const
{
    switch (col) {
    case ColumnName:
    case ColumnType:
    case ColumnPath:
        return "string";
    case ColumnState:
        return "bool";
    default: break;
    }
    return "null";
}

void PluginDataModel::GetValue(wxVariant& variant, const wxDataViewItem& item, unsigned col) const
{
    assert(item.IsOk());

    const auto index = GetRowIndex(item);
    if (index >= mPluginStateModel.size()) {
        return;
    }

    const auto [descriptor, state] = mPluginStateModel[index];
    switch (col) {
    case ColumnName:
    {
        if (descriptor->GetPluginType() == PluginTypeEffect) {
            variant = descriptor->GetSymbol().Translation();
        }
        // This is not right and will not work when other plugin types are added.
        // But it's presumed that the plugin manager dialog will be fully developed
        // by then.
        else if (descriptor->GetPluginType() == PluginTypeStub) {
            wxFileName fname{ descriptor->GetPath() };
            variant = fname.GetName().Trim(false).Trim(true);
        }
    } break;
    case ColumnPath:
        variant = descriptor->GetPath();
        break;
    case ColumnType:
        variant = descriptor->GetEffectFamily();
        break;
    case ColumnState:
        variant = state;
        break;
    default:
        break;
    }
}

bool PluginDataModel::SetValue(const wxVariant& variant, const wxDataViewItem& item, unsigned col)
{
    if (col == ColumnState) {
        const auto index = GetRowIndex(item);
        if (index < mPluginStateModel.size()) {
            mPluginStateModel[index].second = variant.GetBool();
            return true;
        }
    }
    return false;
}

wxDataViewItem PluginDataModel::GetParent(const wxDataViewItem& item) const
{
    return {};
}

bool PluginDataModel::IsContainer(const wxDataViewItem& item) const
{
    return item.GetID() == RowIdRoot;
}

unsigned PluginDataModel::GetChildren(const wxDataViewItem& item, wxDataViewItemArray& children) const
{
    children.clear();
    if (item.GetID() != RowIdRoot) {
        return 0;
    }

    children.reserve(mIndexFilterMap.size());
    for (const auto index : mIndexFilterMap) {
        children.push_back(wxDataViewItem(index));
    }

    return children.size();
}

PluginDescriptor* PluginDataModel::GetPlugin(wxDataViewItem item)
{
    if (!item.IsOk()) {
        return nullptr;
    }
    const auto index = GetRowIndex(item);
    if (index >= mPluginStateModel.size()) {
        return nullptr;
    }

    return mPluginStateModel[index].first;
}

void PluginDataModel::UpdateFilter()
{
    auto textFilters = wxSplit(mFilterExpr, ' ');
    std::for_each(
        textFilters.begin(),
        textFilters.end(),
        [](wxString& str) { str.MakeLower(); }
        );
    mIndexFilterMap.clear();

    auto filterFn = [=](PluginDescriptor* desc, bool state)
    {
        if ((!mFilterType.empty() && desc->GetProviderID() != mFilterType)
            || (mFilterState != -1 && state != static_cast<bool>(mFilterState))
            || (mFilterCategory != -1 && mFilterCategory != desc->GetEffectType())) {
            return false;
        }

        for (auto& filter : textFilters) {
            if ((desc->GetPluginType() != PluginTypeEffect
                 || desc->GetSymbol().Translation().Lower().Find(filter) == wxNOT_FOUND)
                && (desc->GetPluginType() != PluginTypeStub
                    || wxFileName(desc->GetPath()).GetName().Lower().Find(filter) == wxNOT_FOUND)) {
                return false;
            }
        }
        return true;
    };

    for (uint32_t i = 0; i < mPluginStateModel.size(); ++i) {
        const auto [desc, state] = mPluginStateModel[i];
        if (filterFn(desc, state)) {
            mIndexFilterMap.push_back(MakeRow(i));
        }
    }
    Cleared();
}

bool PluginDataModel::IsFilterEmpty() const
{
    return mFilterState == -1 && mFilterType.empty() && mFilterExpr.empty();
}
