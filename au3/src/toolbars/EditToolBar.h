/**********************************************************************

  Audacity: A Digital Audio Editor


  EditToolbar.h

  Dominic Mazzoni
  Shane T. Mueller
  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_EDIT_TOOLBAR__
#define __AUDACITY_EDIT_TOOLBAR__

#include <wx/defs.h>

#include "ToolBar.h"
#include "ToolBarButtons.h"

class wxCommandEvent;
class wxDC;
class wxGridSizer;
class wxImage;
class wxWindow;

class AButton;

class EditToolBar final : public ToolBar
{
public:

    static Identifier ID();

    EditToolBar(AudacityProject& project);
    virtual ~EditToolBar();

    void Create(wxWindow* parent) override;

    void OnButton(wxCommandEvent& event);

    void Populate() override;
    void Repaint(wxDC* WXUNUSED(dc)) override {}
    void EnableDisableButtons() override;
    void UpdatePrefs() override;

private:

    void AddButton(
        teBmps eEnabledUp, teBmps eEnabledDown, teBmps eDisabled, int id, const TranslatableString& label, bool toggle = false);

    void AddSeparator();

    void RegenerateTooltips() override;

    ToolBarButtons mButtons;
    wxGridSizer* mToolSizer;

public:

    DECLARE_CLASS(EditToolBar)
    DECLARE_EVENT_TABLE()
};

#endif
