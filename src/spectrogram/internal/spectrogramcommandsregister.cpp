/*
 * Audacity: A Digital Audio Editor
 */
#include "spectrogramcommandsregister.h"

#include "framework/global/types/translatablestring.h"

#include "../spectrogramcommands.h"

namespace au::spectrogram {
namespace {
using muse::rcommand::Arg;
using muse::rcommand::CommandInfo;
using muse::rcommand::DataType;
using muse::rcommand::Decoration;
using muse::rcommand::InputSchema;

const std::vector<CommandInfo> s_commandInfos = {
    CommandInfo{
        TRACK_SPECTROGRAM_SETTINGS_COMMAND,
        //: Action title: shown as a menu item or a button label; keep it short
        muse::TranslatableString("action", "Spectrogram settings…"),
        //: Action description: shown as a tooltip; can be a full sentence
        muse::TranslatableString("action_description", "Spectrogram settings…"),
        InputSchema({
                    { "trackId", Arg(DataType::Integer, u"Id of the track") },
                }),
        Decoration()
    },
};
}

std::string SpectrogramCommandsRegister::moduleName() const
{
    return "spectrogram";
}

const std::vector<muse::rcommand::Command>& SpectrogramCommandsRegister::commandList() const
{
    static std::vector<muse::rcommand::Command> commands;
    if (commands.empty()) {
        commands.reserve(s_commandInfos.size());
        for (const auto& info : s_commandInfos) {
            commands.push_back(info.command);
        }
    }
    return commands;
}

const std::vector<CommandInfo>& SpectrogramCommandsRegister::commandInfoList() const
{
    return s_commandInfos;
}
}
