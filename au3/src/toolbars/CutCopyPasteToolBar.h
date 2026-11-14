/**********************************************************************

  Audacity: A Digital Audio Editor


  CutCopyPaste.h

  ksoze95

**********************************************************************/

#ifndef __AUDACITY_CUT_COPY_PASTE_TOOLBAR__
#define __AUDACITY_CUT_COPY_PASTE_TOOLBAR__

#include <wx/defs.h>

#include "ToolBar.h"
#include "ToolBarButtons.h"

class wxCommandEvent;
class wxDC;
class wxGridSizer;
class wxImage;
class wxWindow;

class CutCopyPasteToolBar final : public ToolBar
{
public:

    CutCopyPasteToolBar(AudacityProject& project);
    virtual ~CutCopyPasteToolBar();

    bool ShownByDefault() const override;
    bool HideAfterReset() const override;

    static Identifier ID();

    void Create(wxWindow* parent) override;

    void OnButton(wxCommandEvent& event);

    void Populate() override;
    void Repaint(wxDC* WXUNUSED(dc)) override {}
    void EnableDisableButtons() override;
    void UpdatePrefs() override;

private:

    void AddButton(
        teBmps eEnabledUp, teBmps eEnabledDown, teBmps eDisabled, int id, const TranslatableString& label, bool toggle = false);

    void RegenerateTooltips() override;

    ToolBarButtons mButtons;
    wxGridSizer* mToolSizer;

public:

    DECLARE_CLASS(CutCopyPaste)
    DECLARE_EVENT_TABLE()
};

#endif
