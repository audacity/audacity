/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TimelineMenus.cpp

  Dmitry Vedenko

*/
#include "../AdornedRulerPanel.h"

#include "Project.h"

#include "../commands/CommandManager.h"
#include "../commands/CommandContext.h"

namespace
{
void SetRulerType(const CommandContext& context, AdornedRulerPanel::RulerTypeValues type)
{
   auto& project = context.project;

   auto& ruler = AdornedRulerPanel::Get(project);
   ruler.SetRulerType(type);

   CommandManager::Get(project).UpdateCheckmarks(project);
}

void OnSetMinutesSeconds(const CommandContext& context)
{
   SetRulerType(context, AdornedRulerPanel::stMinutesAndSeconds);
}

void OnSetBeatsAndMeasures(const CommandContext& context)
{
   SetRulerType(context, AdornedRulerPanel::stBeatsAndMeasures);
}

AdornedRulerPanel::RulerTypeValues GetRulerType(const AudacityProject& project)
{
   auto& panel = AdornedRulerPanel::Get(project);
   return panel.GetRulerType();
}

using namespace MenuTable;

BaseItemSharedPtr ExtraSelectionMenu()
{
   using Options = CommandManager::Options;
   static BaseItemSharedPtr menu { Menu(
      wxT("Timeline"), XXO("&Timeline"),
      Command(
         wxT("MinutesAndSeconds"), XXO("Minutes and Seconds"),
         OnSetMinutesSeconds, AlwaysEnabledFlag,
         CommandManager::Options {}.CheckTest(
            [](const AudacityProject& project) {
               return GetRulerType(project) ==
                      AdornedRulerPanel::stMinutesAndSeconds;
            })),
      Command(
         wxT("BeatsAndMeasures"), XXO("Beats and Measures"),
         OnSetBeatsAndMeasures, AlwaysEnabledFlag,
         CommandManager::Options {}.CheckTest(
            [](const AudacityProject& project) {
               return GetRulerType(project) ==
                      AdornedRulerPanel::stBeatsAndMeasures;
            }))) };
   return menu;
}

AttachedItem sAttachment2 { wxT("Optional/Extra/Part1"),
   Indirect(ExtraSelectionMenu()) };

} // namespace
