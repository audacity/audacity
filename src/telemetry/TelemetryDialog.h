#pragma once

#include "widgets/wxPanelWrapper.h"
#include "wx/string.h"

class HtmlWindow;
class wxWindow;

class TelemetryDialog final : public wxDialogWrapper
{
    DECLARE_DYNAMIC_CLASS (AboutDialog)
public:
    explicit TelemetryDialog (wxWindow* parent);
    virtual ~TelemetryDialog ();

    void OnAccept (wxCommandEvent& event);
    void OnDecline (wxCommandEvent& event);

    DECLARE_EVENT_TABLE ()

private:
    HtmlWindow* CreateHTMLWindow (wxWindow* parent);
};