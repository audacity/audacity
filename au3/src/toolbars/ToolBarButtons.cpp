/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  ToolBarButtons.cpp

  ksoze95

**********************************************************************/

#include <utility>
#include <wx/app.h>

#include "ToolBarButtons.h"

#include "AllThemeResources.h"
#include "../widgets/AButton.h"
#include "Project.h"

#include "CommandContext.h"
#include "CommandManager.h"
#include "../commands/CommandDispatch.h"

// flags so 1,2,4,8 etc.
enum {
    TBActTooltips = 1,
    TBActEnableDisable = 2,
};

ToolBarButtons::ToolBarButtons(ToolBar* const parent, AudacityProject& project, ButtonList buttonList, int size, int firstButtonId)
    : mParent(parent)
    , mProject(project)
    , mFirstButtonId(firstButtonId)
    , mButtons(size)
    , mButtonList(std::move(buttonList))
{
}

void ToolBarButtons::OnButton(wxCommandEvent& event)
{
    int id = event.GetId() - mFirstButtonId;

    // Be sure the pop-up happens even if there are exceptions, except for buttons which toggle.
    auto cleanup = finally([&] { mButtons[id]->InteractionOver(); });

    auto flags = CommandManager::Get(mProject).GetUpdateFlags();
    const CommandContext context(mProject);

    CommandDispatch::HandleTextualCommand(
        mButtonList[id].commandName, context, flags, false);

#if defined(__WXMAC__)
    // Bug 2402
    // LLL: It seems that on the Mac the IDLE events are processed
    //      differently than on Windows/GTK and the AdornedRulerPanel's
    //      OnPaint() method gets called sooner that expected. This is
    //      evident when zooming from this toolbar only. When zooming from
    //      the Menu or from keyboard ommand, the zooming works correctly.
    wxTheApp->ProcessIdle();
#endif
}

AButton* ToolBarButtons::CreateButton(teBmps eEnabledUp, teBmps eEnabledDown, teBmps eDisabled, int thisButtonId,
                                      const TranslatableString& label, bool toggle)
{
    auto& button = mButtons[thisButtonId];// ;
    button = safenew AButton(mParent, thisButtonId + mFirstButtonId);
    button->SetButtonType(AButton::FrameButton);
    button->SetButtonToggles(toggle);
    button->SetImages(
        theTheme.Image(bmpRecoloredUpSmall),
        theTheme.Image(bmpRecoloredUpHiliteSmall),
        theTheme.Image(bmpRecoloredDownSmall),
        theTheme.Image(bmpRecoloredHiliteSmall),
        theTheme.Image(bmpRecoloredUpSmall));
    button->SetIcons(theTheme.Image(eEnabledUp), theTheme.Image(eEnabledDown), theTheme.Image(eDisabled));
    button->SetFrameMid(3);
    button->SetLabel(label);
    button->SetMinSize(wxSize { 25, 25 });
    button->SetMaxSize(wxSize { 25, 25 });
    return button;
}

void ToolBarButtons::SetEnabled(int id, bool state)
{
    mButtons[id]->SetEnabled(state);
}

void ToolBarButtons::SetCustomEnableDisableButtonsAction(std::function<void()> action)
{
    mCustomEnableDisableButtonsAction = std::move(action);
}

void ToolBarButtons::PopUp(int id)
{
    mButtons[id]->PopUp();
}

void ToolBarButtons::PushDown(int id)
{
    mButtons[id]->PushDown();
}

void ToolBarButtons::EnableDisableButtons()
{
    ForAllButtons(TBActEnableDisable);
}

void ToolBarButtons::RegenerateTooltips()
{
    ForAllButtons(TBActTooltips);
}

void ToolBarButtons::ForAllButtons(int Action)
{
    CommandManager* cm = nullptr;

    if (Action & TBActEnableDisable) {
        cm = &CommandManager::Get(mProject);

        if (mCustomEnableDisableButtonsAction) {
            mCustomEnableDisableButtonsAction();
        }
    }

    for (const auto& entry : mButtonList) {
#if wxUSE_TOOLTIPS
        if (Action & TBActTooltips) {
            ComponentInterfaceSymbol command{
                entry.commandName, entry.untranslatedLabel };
            ToolBar::SetButtonToolTip(mProject,
                                      *mButtons[entry.tool], &command, 1u);
        }
#endif
        if (cm) {
            SetEnabled(entry.tool, cm->GetEnabled(entry.commandName));
        }
    }
}
