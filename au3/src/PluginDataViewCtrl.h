#pragma once

#include <wx/dataview.h>

class PluginDataViewCtrl final : public wxDataViewCtrl
{
    friend class PluginsDataViewCtrlAx;
    friend class PluginDataViewRenderer;

    void Init();
public:
    template<typename ... Args>
    PluginDataViewCtrl(Args&&... args)
        : wxDataViewCtrl(std::forward<Args>(args)...)
    {
        Init();
    }

#if defined(wxHAS_GENERIC_DATAVIEWCTRL)
    int GetFirstVisibleRow() const;
    int GetLastVisibleRow() const;
    int GetRowAt(const wxPoint& point) const;
#endif

#if wxUSE_ACCESSIBILITY
    wxAccessible* CreateAccessible() override;
#endif

private:
#if defined(wxHAS_GENERIC_DATAVIEWCTRL)
    void OnTableCharHook(wxKeyEvent& evt);

    void SetFocus() override;
#endif

    void OnCharEvent(wxKeyEvent& evt);

    DECLARE_EVENT_TABLE()
};
