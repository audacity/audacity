/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: wxwidgets

   Dan Horgan
   James Crook

******************************************************************//**

\file HelpCommand.cpp
\brief Definitions for HelpCommand and HelpCommandType classes

*//*******************************************************************/

#include "HelpCommand.h"

#include "../CommonCommandFlags.h"
#include "CommandContext.h"
#include "CommandDispatch.h"
#include "CommandTargets.h"
#include "EffectAndCommandPluginManager.h"
#include "LoadCommands.h"
#include "MenuRegistry.h"
#include "PluginManager.h"
#include "SettingsVisitor.h"
#include "ShuttleGui.h"

const ComponentInterfaceSymbol HelpCommand::Symbol
{ XO("Help") };

const ComponentInterfaceSymbol CommentCommand::Symbol
{ XO("Comment") };

namespace {
BuiltinCommandsModule::Registration< HelpCommand > reg;
}
namespace {
BuiltinCommandsModule::Registration< CommentCommand > reg2;
}

enum {
    kJson,
    kLisp,
    kBrief,
    nFormats
};

static const EnumValueSymbol kFormats[nFormats] =
{
    // These are acceptable dual purpose internal/visible names

    /* i18n-hint JavaScript Object Notation */
    { XO("JSON") },
    /* i18n-hint name of a computer programming language */
    { XO("LISP") },
    { XO("Brief") }
};

template<bool Const>
bool HelpCommand::VisitSettings(SettingsVisitorBase<Const>& S)
{
    S.Define(mCommandName, wxT("Command"), wxString { "Help" });
    S.DefineEnum(mFormat, wxT("Format"), 0, kFormats, nFormats);
    return true;
}

bool HelpCommand::VisitSettings(SettingsVisitor& S)
{ return VisitSettings<false>(S); }

bool HelpCommand::VisitSettings(ConstSettingsVisitor& S)
{ return VisitSettings<true>(S); }

void HelpCommand::PopulateOrExchange(ShuttleGui& S)
{
    S.AddSpace(0, 5);

    S.StartMultiColumn(2, wxALIGN_CENTER);
    {
        S.TieTextBox(XXO("Command:"), mCommandName);
        S.TieChoice(XXO("Format:"),
                    mFormat, Msgids(kFormats, nFormats));
    }
    S.EndMultiColumn();
}

bool HelpCommand::Apply(const CommandContext& context)
{
    if (mFormat == kJson) {
        return ApplyInner(context);
    }

    if (mFormat == kLisp) {
        CommandContext LispyContext(
            context.project,
            std::make_unique<LispifiedCommandOutputTargets>(*context.pOutput.get())
            );
        return ApplyInner(LispyContext);
    }

    if (mFormat == kBrief) {
        CommandContext BriefContext(
            context.project,
            std::make_unique<BriefCommandOutputTargets>(*context.pOutput.get())
            );
        return ApplyInner(BriefContext);
    }

    return false;
}

bool HelpCommand::ApplyInner(const CommandContext& context)
{
    PluginID ID = PluginManager::Get().GetByCommandIdentifier(mCommandName);
    if (ID.empty()) {
        context.Status("Command not found");
    } else {
        EffectAndCommandPluginManager::Get().GetCommandDefinition(ID, context, 1);
    }
    return true;
}

template<bool Const>
bool CommentCommand::VisitSettings(SettingsVisitorBase<Const>& S)
{
    S.Define(mComment, wxT("_"),  wxString {});
    return true;
}

bool CommentCommand::VisitSettings(SettingsVisitor& S)
{ return VisitSettings<false>(S); }

bool CommentCommand::VisitSettings(ConstSettingsVisitor& S)
{ return VisitSettings<true>(S); }

void CommentCommand::PopulateOrExchange(ShuttleGui& S)
{
    S.AddSpace(0, 5);

    S.StartMultiColumn(2, wxALIGN_CENTER);
    {
        S.TieTextBox(XXO("_"), mComment, 80);
    }
    S.EndMultiColumn();
}

namespace {
using namespace MenuRegistry;

// Register menu items

AttachedItem sAttachment{
    // Note that the PLUGIN_SYMBOL must have a space between words,
    // whereas the short-form used here must not.
    // (So if you did write "Compare Audio" for the PLUGIN_SYMBOL name, then
    // you would have to use "CompareAudio" here.)
    Command(wxT("Help"), XXO("Help..."),
            CommandDispatch::OnAudacityCommand, AudioIONotBusyFlag()),
    wxT("Optional/Extra/Part2/Scriptables2")
};
}
