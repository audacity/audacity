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
#include "../Menus.h"
#include "Project.h"

#include "../commands/CommandContext.h"
#include "../commands/CommandManager.h"
#include "../commands/CommandDispatch.h"

// flags so 1,2,4,8 etc.
enum {
   TBActTooltips = 1,
   TBActEnableDisable = 2,
};

ToolBarButtons::ToolBarButtons(ToolBar *const parent, AudacityProject & project, ButtonList buttonList, int size, int firstButtonId)
   : mParent(parent)
   , mProject(project)
   , mFirstButtonId(firstButtonId)
   , mButtons(size)
   , mButtonList(std::move(buttonList))
{
}

void ToolBarButtons::OnButton(wxCommandEvent & event)
{
   int id = event.GetId() - mFirstButtonId;

   // Be sure the pop-up happens even if there are exceptions, except for buttons which toggle.
   auto cleanup = finally( [&] { mButtons[id]->InteractionOver(); });

   auto &cm = CommandManager::Get( mProject );

   auto flags = MenuManager::Get(mProject).GetUpdateFlags();
   const CommandContext context( mProject );

   CommandDispatch::HandleTextualCommand(cm,
      mButtonList[id].commandName, context, flags, false );

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

AButton* ToolBarButtons::CreateButton(teBmps eEnabledUp, teBmps eEnabledDown, teBmps eDisabled, int thisButtonId, const TranslatableString& label, bool toggle)
{
   AButton *&r = mButtons[thisButtonId];

   r = ToolBar::MakeButton(mParent,
      bmpRecoloredUpSmall, bmpRecoloredDownSmall, bmpRecoloredUpHiliteSmall, bmpRecoloredHiliteSmall,
      eEnabledUp, eEnabledDown, eDisabled,
      wxWindowID(thisButtonId + mFirstButtonId),
      wxDefaultPosition,
      toggle,
      theTheme.ImageSize( bmpRecoloredUpSmall ));

   r->SetLabel(label);

   return r;
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
   ForAllButtons( TBActEnableDisable );
}

void ToolBarButtons::RegenerateTooltips()
{
   ForAllButtons( TBActTooltips );
}

void ToolBarButtons::ForAllButtons(int Action)
{
   CommandManager* cm = nullptr;

   if (Action & TBActEnableDisable) {
      cm = &CommandManager::Get( mProject );

      if (mCustomEnableDisableButtonsAction)
         mCustomEnableDisableButtonsAction();
   }

   for (const auto &entry : mButtonList) {
#if wxUSE_TOOLTIPS
      if ( Action & TBActTooltips ) {
         ComponentInterfaceSymbol command{
            entry.commandName, entry.untranslatedLabel };
         ToolBar::SetButtonToolTip( mProject,
            *mButtons[entry.tool], &command, 1u );
      }
#endif
      if (cm) {
         SetEnabled(entry.tool, cm->GetEnabled(entry.commandName));
      }
   }
}
