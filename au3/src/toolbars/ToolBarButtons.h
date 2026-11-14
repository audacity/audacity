/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  ToolBarButtons.h

  ksoze95

**********************************************************************/

#ifndef __AUDACITY_TOOLBAR_BUTTONS__
#define __AUDACITY_TOOLBAR_BUTTONS__

#include <functional>
#include <vector>
#include "ToolBar.h"

class wxCommandEvent;

class AudacityProject;
class AButton;

class ToolBarButtons final
{
public:
    struct Entry final {
        int tool;
        CommandID commandName;
        TranslatableString untranslatedLabel;
    };

    using ButtonList = std::vector<Entry>;

    ToolBarButtons(ToolBar* const parent, AudacityProject& project, ButtonList buttonList, int size, int firstButtonId);

    void OnButton(wxCommandEvent& event);

    AButton* CreateButton(
        teBmps eEnabledUp, teBmps eEnabledDown, teBmps eDisabled, int id, const TranslatableString& label, bool toggle = false);

    void SetEnabled(int id, bool state);
    void SetCustomEnableDisableButtonsAction(std::function<void()> action);

    void PopUp(int id);
    void PushDown(int id);

    void EnableDisableButtons();
    void RegenerateTooltips();

private:
    void ForAllButtons(int Action);

private:
    ToolBar* mParent;
    AudacityProject& mProject;
    int mFirstButtonId;

    std::vector<AButton*> mButtons;
    ButtonList mButtonList;

    std::function<void()> mCustomEnableDisableButtonsAction;
};

#endif
