/*
* Audacity: A Digital Audio Editor
*/
#include "macroscommandsregister.h"

#include <ranges>

#include "../macromanagercommands.h"

using namespace au::macromanager;
using namespace muse;
using namespace muse::rcommand;

namespace {
// A command title marks its mnemonic letter with "&" and shows "&&" as a literal "&".
// A macro name is user text, so each "&" in it must stay visible and not become a mnemonic.
String escapeMnemonics(String text)
{
    return text.replace(u"&", u"&&");
}

CommandInfo macroCommandInfo(const Macro& macro)
{
    const String name = String::fromStdString(macro.name);

    return CommandInfo {
        makeMacroRunCommand(macro.name),
        TranslatableString::untranslatable(escapeMnemonics(name)),
        //: Command description: shown as a tooltip; can be a full sentence. %1 is the macro name
        TranslatableString("command_description", "Run the macro “%1”").arg(name),
        InputSchema(),
        Decoration(),
        Availability::UI
    };
}
}

void MacrosCommandsRegister::init()
{
    repository()->macrosChanged().onNotify(this, [this]() {
        reload();
    });

    reload();
}

void MacrosCommandsRegister::reload()
{
    const MacroList& macros = repository()->macros();

    auto infos = macros | std::views::transform(macroCommandInfo);
    m_commandInfos = { infos.begin(), infos.end() };

    auto commands = m_commandInfos | std::views::transform(&CommandInfo::command);
    m_commands = { commands.begin(), commands.end() };

    m_commandListChanged.notify();
}

std::string MacrosCommandsRegister::moduleName() const
{
    return "macromanager";
}

const std::vector<Command>& MacrosCommandsRegister::commandList() const
{
    return m_commands;
}

const std::vector<CommandInfo>& MacrosCommandsRegister::commandInfoList() const
{
    return m_commandInfos;
}

async::Notification MacrosCommandsRegister::commandListChanged() const
{
    return m_commandListChanged;
}
