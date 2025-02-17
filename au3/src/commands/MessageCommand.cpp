/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: wxwidgets

   Dan Horgan

******************************************************************//**

\file MessageCommand.cpp
\brief Definitions for MessageCommand class

*//*******************************************************************/

#include "MessageCommand.h"

#include "CommandDispatch.h"
#include "MenuRegistry.h"
#include "../CommonCommandFlags.h"
#include "LoadCommands.h"
#include "CommandContext.h"
#include "SettingsVisitor.h"
#include "ShuttleGui.h"

const ComponentInterfaceSymbol MessageCommand::Symbol
{ XO("Message") };

namespace {
BuiltinCommandsModule::Registration< MessageCommand > reg;
}

template<bool Const>
bool MessageCommand::VisitSettings(SettingsVisitorBase<Const>& S)
{
    S.Define(mMessage, wxT("Text"), wxString { "Some message" });
    return true;
}

bool MessageCommand::VisitSettings(SettingsVisitor& S)
{ return VisitSettings<false>(S); }

bool MessageCommand::VisitSettings(ConstSettingsVisitor& S)
{ return VisitSettings<true>(S); }

void MessageCommand::PopulateOrExchange(ShuttleGui& S)
{
    S.AddSpace(0, 5);

    S.StartMultiColumn(2, wxALIGN_CENTER);
    {
        S.TieTextBox(XXO("Text:"), mMessage, 60);
    }
    S.EndMultiColumn();
}

bool MessageCommand::Apply(const CommandContext& context)
{
    context.Status(mMessage);
    return true;
}

namespace {
using namespace MenuRegistry;

// Register menu items

AttachedItem sAttachment{
    // Note that the PLUGIN_SYMBOL must have a space between words,
    // whereas the short-form used here must not.
    // (So if you did write "Compare Audio" for the PLUGIN_SYMBOL name, then
    // you would have to use "CompareAudio" here.)
    Command(wxT("Message"), XXO("Message..."),
            CommandDispatch::OnAudacityCommand, AudioIONotBusyFlag()),
    wxT("Optional/Extra/Part2/Scriptables2")
};
}
