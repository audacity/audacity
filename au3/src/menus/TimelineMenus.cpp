/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TimelineMenus.cpp

  Dmitry Vedenko

*/
#include "../AdornedRulerPanel.h"

#include "Project.h"

#include "CommandManager.h"
#include "CommandContext.h"

namespace {
void SetTimeDisplayMode(const CommandContext& context, TimeDisplayMode type)
{
    auto& project = context.project;

    auto& ruler = AdornedRulerPanel::Get(project);
    ruler.SetTimeDisplayMode(type);

    CommandManager::Get(project).UpdateCheckmarks();
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

using namespace MenuRegistry;

auto ExtraSelectionMenu()
{
    static auto menu = std::shared_ptr{ Menu(
                                            wxT("Timeline"), XXO("Tim&eline"),
                                            Command(
                                                wxT("MinutesAndSeconds"), XXO("Minutes and Seconds"),
                                                OnSetMinutesSeconds, AlwaysEnabledFlag,
                                                Options {}.CheckTest(
                                                    [](const AudacityProject& project) {
            return GetTimeDisplayMode(project)
                   == TimeDisplayMode::MinutesAndSeconds;
        })),
                                            Command(
                                                wxT("BeatsAndMeasures"), XXO("Beats and Measures"),
                                                OnSetBeatsAndMeasures, AlwaysEnabledFlag,
                                                Options {}.CheckTest(
                                                    [](const AudacityProject& project) {
            return GetTimeDisplayMode(project)
                   == TimeDisplayMode::BeatsAndMeasures;
        }))) };
    return menu;
}

AttachedItem sAttachment2 { Indirect(ExtraSelectionMenu()),
                            wxT("View/Other/Toolbars") };
} // namespace
