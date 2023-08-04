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
void SetTimeDisplayMode(const CommandContext& context, TimeDisplayMode type)
{
   auto& project = context.project;

   auto& ruler = AdornedRulerPanel::Get(project);
   ruler.SetTimeDisplayMode(type);

   CommandManager::Get(project).UpdateCheckmarks(project);
}

void OnSetMinutesSeconds(const CommandContext& context)
{
   SetTimeDisplayMode(context, TimeDisplayMode::MinutesAndSeconds);
}

void OnSetBeatsAndMeasures(const CommandContext& context)
{
   SetTimeDisplayMode(context, TimeDisplayMode::BeatsAndMeasures);
}

TimeDisplayMode GetTimeDisplayMode(const AudacityProject& project)
{
   auto& panel = AdornedRulerPanel::Get(project);
   return panel.GetTimeDisplayMode();
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
               return GetTimeDisplayMode(project) ==
                      TimeDisplayMode::MinutesAndSeconds;
            })),
      Command(
         wxT("BeatsAndMeasures"), XXO("Beats and Measures"),
         OnSetBeatsAndMeasures, AlwaysEnabledFlag,
         CommandManager::Options {}.CheckTest(
            [](const AudacityProject& project) {
               return GetTimeDisplayMode(project) ==
                      TimeDisplayMode::BeatsAndMeasures;
            }))) };
   return menu;
}

AttachedItem sAttachment2 { wxT("Optional/Extra/Part1"),
   Indirect(ExtraSelectionMenu()) };

} // namespace
