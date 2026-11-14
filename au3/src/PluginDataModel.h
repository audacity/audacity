/*!*********************************************************************

  Audacity: A Digital Audio Editor

  @file PluginDataModel.h

  @author Vitaly Sverchinsky

**********************************************************************/
#pragma once

#include <wx/dataview.h>

#include "PluginDescriptor.h"

class PluginDescriptor;

///!brief A plugins list model that can be attached to wxDataViewCtrl
class PluginDataModel final : public wxDataViewModel
{
    static const void* RowIdRoot;

    std::vector<std::pair<PluginDescriptor*, bool> > mPluginStateModel;
    std::vector<void*> mIndexFilterMap;

    wxString mFilterType;
    wxString mFilterExpr;
    int mFilterState{ -1 };
    int mFilterCategory{ -1 };

public:

    enum Column
    {
        ColumnName,
        ColumnType,
        ColumnPath,
        ColumnState,
        ColumnNum
    };

    PluginDataModel(int filterEffectCategory = -1, int filterState = -1, const wxString& filterType = {}, const wxString& filterExpr = {});

    wxDataViewItem GetItemForRow(uint32_t row) const;
    bool GetRowForItem(const wxDataViewItem& item, uint32_t& row) const;
    uint32_t GetRowCount() const;

    bool IsEnabled(const wxDataViewItem&, unsigned) const override;
    bool HasContainerColumns(const wxDataViewItem& item) const override;

    void SetFilterType(const wxString& type);
    wxString GetFilterType() const;
    void SetFilterState(int state);
    int GetFilterState() const;
    void SetFilterExpr(const wxString& expr);
    wxString GetFilterExpr() const;
    void SetFilterCategory(int category);
    int GetFilterCategory() const;

    int Compare(const wxDataViewItem& item1, const wxDataViewItem& item2, unsigned column, bool ascending) const override;

    void ApplyChanges(const std::function<bool(int, int, const wxString&)>& progressUpdateFn,
                      const std::function<void(const TranslatableString&)>& errorFn);

    unsigned GetColumnCount() const override;
    wxString GetColumnType(unsigned col) const override;
    void GetValue(wxVariant& variant, const wxDataViewItem& item, unsigned col) const override;
    bool SetValue(const wxVariant& variant, const wxDataViewItem& item, unsigned col) override;
    wxDataViewItem GetParent(const wxDataViewItem& item) const override;
    bool IsContainer(const wxDataViewItem& item) const override;
    unsigned GetChildren(const wxDataViewItem& item, wxDataViewItemArray& children) const override;

    PluginDescriptor* GetPlugin(wxDataViewItem item);

    bool IsListModel() const override { return false; }
private:
    void UpdateFilter();
    bool IsFilterEmpty() const;
};
