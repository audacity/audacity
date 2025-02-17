#include "PluginDataViewCtrl.h"

#include <wx/renderer.h>

#include "PluginDataModel.h"

BEGIN_EVENT_TABLE(PluginDataViewCtrl, wxDataViewCtrl)
EVT_CHAR(PluginDataViewCtrl::OnCharEvent)
END_EVENT_TABLE()

#if defined(wxHAS_GENERIC_DATAVIEWCTRL) || defined(__WXGTK__)
class PluginDataViewRenderer : public wxDataViewCustomRenderer
{
public:
    using wxDataViewCustomRenderer::wxDataViewCustomRenderer;

    bool ActivateCell(const wxRect& cell,
                      wxDataViewModel* model,
                      const wxDataViewItem& item,
                      unsigned col,
                      const wxMouseEvent* mouseEvent) final
    {
        if (mouseEvent == nullptr) {
            wxCHECK(GetView(), false);

            wxVariant value;
            model->GetValue(value, item, PluginDataModel::ColumnState);
            value = !value.GetBool();

            wxDataViewItemArray sel;
            GetView()->GetSelections(sel);
            if (!sel.empty()) {
                for (const auto& other : sel) {
                    model->ChangeValue(value, other, PluginDataModel::ColumnState);
                }
            } else {
                model->ChangeValue(value, item, PluginDataModel::ColumnState);
            }

   #if wxUSE_ACCESSIBILITY
            if (const auto ctrl = dynamic_cast<PluginDataViewCtrl*>(GetView())) {
                wxAccessible::NotifyEvent(
                    wxACC_EVENT_OBJECT_NAMECHANGE,
                    ctrl,
                    wxOBJID_CLIENT,
                    ctrl->GetRowByItem(item) + 1);
            }
   #endif
            return true;
        }
        return OnCellClicked(cell, model, item, col, mouseEvent);
    }

protected:
    virtual bool OnCellClicked(const wxRect& cell,
                               wxDataViewModel* model,
                               const wxDataViewItem& item,
                               unsigned col,
                               const wxMouseEvent* mouseEvent)
    {
        return false;
    }
};

class PluginDataViewTextRenderer final : public PluginDataViewRenderer
{
    wxString mText;
public:

    PluginDataViewTextRenderer()
        : PluginDataViewRenderer("string", wxDATAVIEW_CELL_ACTIVATABLE)
    {
    }

    bool SetValue(const wxVariant& value) override
    {
        mText = value.GetString();
        return true;
    }

    bool GetValue(wxVariant&) const override
    {
        return false;
    }

    bool Render(wxRect cell, wxDC* dc, int state) override
    {
        if (!mText.empty()) {
            RenderText(mText, 0, cell, dc, state);
        }
        return true;
    }

    wxSize GetSize() const override
    {
        if (!mText.empty()) {
            return GetTextExtent(mText);
        }
        return GetView()->FromDIP(wxSize(wxDVC_DEFAULT_RENDERER_SIZE,
                                         wxDVC_DEFAULT_RENDERER_SIZE));
    }

#if wxUSE_ACCESSIBILITY
    wxString GetAccessibleDescription() const override
    {
        return mText;
    }

#endif
};

// Activated cell also toggles all other cells that are currently selected
class PluginDataViewStateRenderer final : public PluginDataViewRenderer
{
    wxVariant mValue;
public:

    PluginDataViewStateRenderer()
        : PluginDataViewRenderer("bool", wxDATAVIEW_CELL_ACTIVATABLE)
    { }

    bool SetValue(const wxVariant& value) override
    {
        mValue = value;
        return true;
    }

    bool GetValue(wxVariant&) const override
    {
        return false;
    }

    bool Render(wxRect cell, wxDC* dc, int state) override
    {
        if (mValue.IsNull()) {
            return false;
        }
        //Duplicated from wxDataViewToggleRenderer
        int flags = 0;
        if (mValue.GetBool()) {
            flags |= wxCONTROL_CHECKED;
        }
        if (GetMode() != wxDATAVIEW_CELL_ACTIVATABLE
            || !(GetOwner()->GetOwner()->IsEnabled() && GetEnabled())) {
            flags |= wxCONTROL_DISABLED;
        }
        wxSize size = cell.GetSize();
        size.IncTo(GetSize());
        cell.SetSize(size);

        wxRendererNative& renderer = wxRendererNative::Get();
        wxWindow* const win = GetOwner()->GetOwner();
        renderer.DrawCheckBox(win, *dc, cell, flags);
        return true;
    }

    wxSize GetSize() const override
    {
        return wxRendererNative::Get().GetCheckBoxSize(GetView());
    }

#if wxUSE_ACCESSIBILITY
    wxString GetAccessibleDescription() const override
    {
        if (!mValue.IsNull()) {
            return mValue.GetBool() ? _("Enabled") : _("Disabled");
        }
        return {};
    }

#endif

private:
    bool OnCellClicked(const wxRect&,
                       wxDataViewModel* model,
                       const wxDataViewItem& item,
                       unsigned col,
                       const wxMouseEvent*) override
    {
        if (!mValue.IsNull()) {
            model->ChangeValue(!mValue.GetBool(), item, col);
            return true;
        }
        return false;
    }
};
#else
class PluginDataViewStateRenderer final : public wxDataViewToggleRenderer
{
public:
    PluginDataViewStateRenderer()
        : wxDataViewToggleRenderer(GetDefaultType(), wxDATAVIEW_CELL_ACTIVATABLE, wxDVR_DEFAULT_ALIGNMENT)
    { }
};

class PluginDataViewTextRenderer final : public wxDataViewTextRenderer
{
public:
    PluginDataViewTextRenderer()
        : wxDataViewTextRenderer(GetDefaultType(), wxDATAVIEW_CELL_INERT, wxDVR_DEFAULT_ALIGNMENT)
    { }
};

#endif

#if defined(wxHAS_GENERIC_DATAVIEWCTRL) && wxUSE_ACCESSIBILITY
class PluginsDataViewCtrlAx final : public wxAccessible
{
public:
    PluginsDataViewCtrlAx(PluginDataViewCtrl* parent)
        : wxAccessible(parent)
    {
    }

    wxAccStatus GetChild(int childId, wxAccessible** accessible) override
    {
        *accessible = childId == wxACC_SELF ? this : nullptr;
        return wxACC_OK;
    }

    wxAccStatus GetChildCount(int* count) override
    {
        if (const auto model = GetModel()) {
            *count = model->GetRowCount();
        } else {
            *count = 0;
        }

        return wxACC_OK;
    }

    wxAccStatus GetName(int childId, wxString* name) override
    {
        const auto ctrl = GetCtrl();
        wxCHECK(ctrl, wxACC_FAIL);

        name->clear();

        if (childId == wxACC_SELF) {
            *name = ctrl->GetName();
            return wxACC_OK;
        }

        const auto model = GetModel();
        if (model == nullptr) {
            return wxACC_INVALID_ARG;
        }

        const auto item = ctrl->GetItemByRow(childId - 1);
        if (!item.IsOk()) {
            return wxACC_INVALID_ARG;
        }

        const auto plugin = model->GetPlugin(item);
        if (plugin == nullptr) {
            return wxACC_INVALID_ARG;
        }

        wxVariant state;
        model->GetValue(state, item, PluginDataModel::ColumnState);

        //separate type and path parts with comma so that
        //screen reader would make a short pause between them
        *name = wxString::Format("%s %s %s, %s",
                                 plugin->GetPluginType() == PluginTypeEffect
                                 ? plugin->GetSymbol().Translation()
                                 : wxFileName(plugin->GetPath()).GetName(),
                                 state.GetBool()
                                 ? _("Enabled")
                                 : _("Disabled"),
                                 plugin->GetEffectFamily(),
                                 plugin->GetPath());

        return wxACC_OK;
    }

    wxAccStatus GetFocus(int* childId, wxAccessible** accessible) override
    {
        const auto ctrl = GetCtrl();
        wxCHECK(ctrl, wxACC_FAIL);

        *childId = wxACC_SELF;
        *accessible = nullptr;

        const auto currentItem = ctrl->GetCurrentItem();
        if (!currentItem.IsOk()) {
            return wxACC_OK;
        }

        *childId = ctrl->GetRowByItem(currentItem) + 1;

        return wxACC_OK;
    }

    wxAccStatus GetLocation(wxRect& rect, int childId) override
    {
        const auto ctrl = GetCtrl();
        wxCHECK(ctrl, wxACC_FAIL);

        if (childId == wxACC_SELF) {
            rect = ctrl->GetScreenRect();
            return wxACC_OK;
        }

        const auto item = ctrl->GetItemByRow(childId - 1);
        if (!item.IsOk()) {
            return wxACC_INVALID_ARG;
        }

        rect = ctrl->GetItemRect(item, nullptr);
        // Indentation and expander column should be included here and therefore
        // reported row width should by the same as the width of the client area.
        rect.width += rect.x;
        rect.x = 0;
        rect.SetPosition(ctrl->ClientToScreen(rect.GetPosition()));

        return wxACC_OK;
    }

    wxAccStatus GetDefaultAction(int childId, wxString* action) override
    {
        const auto ctrl = GetCtrl();
        wxCHECK(ctrl, wxACC_FAIL);

        action->clear();

        if (childId == wxACC_SELF) {
            return wxACC_OK;
        }

        const auto model = GetModel();
        if (model == nullptr) {
            return wxACC_INVALID_ARG;
        }

        const auto item = ctrl->GetItemByRow(childId - 1);

        if (!item.IsOk()) {
            return wxACC_INVALID_ARG;
        }

        wxVariant enabled;
        model->GetValue(enabled, item, PluginDataModel::ColumnState);
        *action = enabled.GetBool()
                  ? _("Disable")
                  : _("Enable");

        return wxACC_OK;
    }

    wxAccStatus DoDefaultAction(int childId) override
    {
        const auto ctrl = GetCtrl();
        wxCHECK(ctrl, wxACC_FAIL);

        if (childId == wxACC_SELF) {
            return wxACC_NOT_SUPPORTED;
        }

        const auto model = GetModel();
        if (model == nullptr) {
            return wxACC_NOT_SUPPORTED;
        }

        const auto item = ctrl->GetItemByRow(childId - 1);
        if (!item.IsOk()) {
            return wxACC_INVALID_ARG;
        }

        wxVariant enabled;
        model->GetValue(enabled, item, PluginDataModel::ColumnState);
        model->SetValue(wxVariant(!enabled.GetBool()), item, PluginDataModel::ColumnState);

        return wxACC_OK;
    }

    wxAccStatus GetSelections(wxVariant* selections) override
    {
        const auto ctrl = GetCtrl();
        wxCHECK(ctrl, wxACC_FAIL);

        wxDataViewItemArray sel;
        ctrl->GetSelections(sel);

        if (sel.IsEmpty()) {
            selections->MakeNull();
            return wxACC_OK;
        }

        if (sel.size() == 1) {
            const auto row = ctrl->GetRowByItem(sel[0]);
            *selections = static_cast<wxLongLong>(row + 1);
        } else {
            wxVariant list(wxVariantList{});
            for (size_t i = 0; i < sel.GetCount(); ++i) {
                const auto row = ctrl->GetRowByItem(sel[i]);
                list.Append(wxVariant(static_cast<wxLongLong>(row + 1)));
            }
            *selections = list;
        }
        return wxACC_OK;
    }

    wxAccStatus GetState(int childId, long* state) override
    {
        const auto ctrl = GetCtrl();
        wxCHECK(ctrl, wxACC_FAIL);

        if (childId == wxACC_SELF) {
            *state = wxACC_STATE_SYSTEM_FOCUSABLE;
            return wxACC_OK;
        }

        const auto model = GetModel();
        if (model == nullptr) {
            return wxACC_INVALID_ARG;
        }

        *state = wxACC_STATE_SYSTEM_FOCUSABLE | wxACC_STATE_SYSTEM_SELECTABLE
                 | wxACC_STATE_SYSTEM_MULTISELECTABLE | wxACC_STATE_SYSTEM_EXTSELECTABLE;

        const auto row = childId - 1;
        if (row < ctrl->GetFirstVisibleRow() || row > ctrl->GetLastVisibleRow()) {
            *state |= wxACC_STATE_SYSTEM_OFFSCREEN;
        }

        if (ctrl->IsSelected(ctrl->GetCurrentItem())) {
            *state |= wxACC_STATE_SYSTEM_FOCUSED;
        }
        if (ctrl->IsSelected(ctrl->GetItemByRow(row))) {
            *state |= wxACC_STATE_SYSTEM_SELECTED;
        }

        return wxACC_OK;
    }

    wxAccStatus GetRole(int childId, wxAccRole* role) override
    {
        *role = childId == wxACC_SELF
                ? wxROLE_SYSTEM_LIST
                : wxROLE_SYSTEM_LISTITEM;
        return wxACC_OK;
    }

private:

    PluginDataViewCtrl* GetCtrl()
    {
        return wxDynamicCast(GetWindow(), PluginDataViewCtrl);
    }

    PluginDataModel* GetModel()
    {
        if (const auto ctrl = GetCtrl()) {
            return dynamic_cast<PluginDataModel*>(ctrl->GetModel());
        }
        return nullptr;
    }
};

#endif

void PluginDataViewCtrl::Init()
{
#if defined(wxHAS_GENERIC_DATAVIEWCTRL)
    //Do not allow wxDataViewMainWindow handle TAB key...
    GetChildren()[0]->Bind(wxEVT_CHAR_HOOK, &PluginDataViewCtrl::OnTableCharHook, this);
#elif defined(__WXOSX__)
    //Implements group toggle behaviour when multiple
    //lines are selected and Enter key pressed.
    Bind(wxEVT_DATAVIEW_ITEM_ACTIVATED, [=](wxDataViewEvent& evt) {
        evt.Skip();
        const auto item = evt.GetItem();
        if (!item.IsOk()) {
            return;
        }

        const auto model = GetModel();
        if (model == nullptr) {
            return;
        }

        wxVariant value;
        model->GetValue(value, item, PluginDataModel::ColumnState);
        if (value.IsNull()) {
            return;
        }

        value = !value.GetBool();

        wxDataViewItemArray sel;
        GetSelections(sel);
        if (!sel.empty()) {
            for (const auto& item : sel) {
                model->ChangeValue(value, item, PluginDataModel::ColumnState);
            }
        } else {
            model->ChangeValue(value, item, PluginDataModel::ColumnState);
        }
        evt.Skip(false);
    });
#endif
    AppendColumn(safenew wxDataViewColumn(
                     _("Name"), safenew PluginDataViewTextRenderer,
                     PluginDataModel::ColumnName,
                     wxDVC_DEFAULT_WIDTH,
                     wxALIGN_NOT,
                     wxDATAVIEW_COL_RESIZABLE | wxDATAVIEW_COL_SORTABLE)
                 );
    AppendColumn(safenew wxDataViewColumn(
                     _("Type"), safenew PluginDataViewTextRenderer,
                     PluginDataModel::ColumnType,
                     wxDVC_DEFAULT_WIDTH,
                     wxALIGN_NOT,
                     wxDATAVIEW_COL_RESIZABLE | wxDATAVIEW_COL_SORTABLE)
                 );
    AppendColumn(safenew wxDataViewColumn(
                     _("Path"), safenew PluginDataViewTextRenderer,
                     PluginDataModel::ColumnPath,
                     wxDVC_DEFAULT_WIDTH,
                     wxALIGN_NOT,
                     wxDATAVIEW_COL_RESIZABLE | wxDATAVIEW_COL_SORTABLE)
                 );
    AppendColumn(safenew wxDataViewColumn(
                     _("Enabled"),
                     safenew PluginDataViewStateRenderer,
                     PluginDataModel::ColumnState,
                     wxDVC_DEFAULT_WIDTH,
                     wxALIGN_CENTER,
                     wxDATAVIEW_COL_SORTABLE
                     ));
}

void PluginDataViewCtrl::OnCharEvent(wxKeyEvent& evt)
{
    evt.Skip();
#if defined(wxHAS_GENERIC_DATAVIEWCTRL)
    //
    if (evt.GetKeyCode() == WXK_CONTROL_A
        && evt.GetModifiers() | WXK_COMMAND) {
        SelectAll();
        evt.Skip(false);
        return;
    }
    if (evt.GetKeyCode() == WXK_SPACE
        || evt.GetKeyCode() == WXK_RETURN
        || evt.GetKeyCode() == WXK_NUMPAD_ENTER) {
        return;
    }

    const auto ch = evt.GetUnicodeKey();
    if (ch == WXK_NONE) {
        return;
    }

    auto item = GetCurrentItem();
    if (!item.IsOk()) {
        return;
    }

    //if item.IsOk(), then there is at least 1 item in the table
    const auto searchStartRow = GetRowByItem(item);
    auto row = searchStartRow + 1;
    while (true)
    {
        item = GetItemByRow(row);

        if (!item.IsOk()) {
            if (row > searchStartRow) {
                //start from the beginning, until we reach
                //searchStartRow again
                row = 0;
                continue;
            }
            break;
        }

        wxVariant data;
        GetModel()->GetValue(data, item, PluginDataModel::ColumnName);
        const auto name = data.GetString();
        if (!name.empty() && name.Left(1).IsSameAs(ch, false)) {
            wxDataViewItemArray sel;
            sel.push_back(item);
            SetSelections(sel);
            SetCurrentItem(sel[0]);
            EnsureVisible(sel[0]);
            break;
        }
        if (row == searchStartRow) {
            break;
        }
        ++row;
    }
    evt.Skip(false);
#else
    evt.Skip();
#endif
}

#if defined(wxHAS_GENERIC_DATAVIEWCTRL)

int PluginDataViewCtrl::GetFirstVisibleRow() const
{
    return GetRowAt({ 0, 0 });
}

int PluginDataViewCtrl::GetLastVisibleRow() const
{
    const auto size = GetClientSize();
    return GetRowAt({ 0, size.y - 1 });
}

int PluginDataViewCtrl::GetRowAt(const wxPoint& point) const
{
    wxDataViewItem item;
    wxDataViewColumn* column;
    HitTest(point, item, column);
    return item.IsOk()
           ? GetRowByItem(item)
           : 0;
}

#endif

#if wxUSE_ACCESSIBILITY
wxAccessible* PluginDataViewCtrl::CreateAccessible()
{
#if defined(wxHAS_GENERIC_DATAVIEWCTRL)
    return safenew PluginsDataViewCtrlAx(this);
#else
    return wxDataViewCtrl::CreateAccessible();
#endif
}

#endif

#if defined(wxHAS_GENERIC_DATAVIEWCTRL)
void PluginDataViewCtrl::OnTableCharHook(wxKeyEvent& evt)
{
    if (evt.GetKeyCode() == WXK_TAB) {
        GetParent()->NavigateIn((evt.GetModifiers() & WXK_COMMAND) == 0);
    } else {
        evt.Skip(evt.GetKeyCode() != WXK_RIGHT && evt.GetKeyCode() != WXK_LEFT);
    }
}

void PluginDataViewCtrl::SetFocus()
{
    if (!GetCurrentItem().IsOk()) {
        wxDataViewItemArray sel;
        sel.push_back(GetItemByRow(0));
        if (sel[0].IsOk()) {
            SetCurrentItem(sel[0]);
            SetSelections(sel);
        }
    }
    wxDataViewCtrl::SetFocus();
#if wxUSE_ACCESSIBILITY
    if (GetCurrentItem().IsOk()) {
        wxAccessible::NotifyEvent(
            wxACC_EVENT_OBJECT_FOCUS,
            this,
            wxOBJID_CLIENT,
            GetRowByItem(GetCurrentItem()) + 1
            );
        wxAccessible::NotifyEvent(
            wxACC_EVENT_OBJECT_SELECTIONWITHIN,
            this,
            wxOBJID_CLIENT,
            wxACC_SELF
            );
    }
#endif
}

#endif
