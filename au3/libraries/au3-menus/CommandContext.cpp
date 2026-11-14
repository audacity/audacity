/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2017 Audacity Team
   License: wxwidgets

   Dan Horgan
   James Crook

******************************************************************//**

\file CommandContext.cpp
\brief Contains definitions for CommandContext

\class CommandContext
\brief CommandContext provides additional information to an
'Apply()' command.  It provides the project, and provides output
channels for Error, Progress and Status.  Status is used for general
messaging from a command back to its invoker.

*//*******************************************************************/
#include "CommandContext.h"

#include <map>
#include <wx/log.h>
#include <wx/variant.h>
#include <wx/arrstr.h>

#include "CommandTargets.h"

CommandContext::CommandContext(
    AudacityProject& p
    , const wxEvent* e
    , int ii
    , const CommandParameter& param)
    : project{p}
    // No target specified?  Use the special interactive one that pops up a dialog.
    , pOutput{TargetFactory::Call()}
    , pEvt{e}
    , index{ii}
    , parameter{param}
{
}

CommandContext::CommandContext(
    AudacityProject& p,
    std::unique_ptr<CommandOutputTargets> target)
    : project{p}
    // Revisit and use std_unique pointer for pOutput??
    , pOutput(std::move(target))
    , pEvt{nullptr}
    , index{0}
    , parameter{CommandParameter {}}
{
}

CommandContext::~CommandContext() = default;

void CommandContext::Status(const wxString& message, bool bFlush) const
{
    if (pOutput) {
        pOutput->Status(message, bFlush);
    } else {
        wxLogDebug("Status:%s", message);
    }
}

void CommandContext::Error(const wxString& message) const
{
    if (pOutput) {
        pOutput->Error(message);
    } else {
        wxLogDebug("Error:%s", message);
    }
}

void CommandContext::Progress(double d) const
{
    if (pOutput) {
        pOutput->Progress(d);
    }
}

void CommandContext::StartArray() const
{
    if (pOutput) {
        pOutput->StartArray();
    }
}

void CommandContext::EndArray() const
{
    if (pOutput) {
        pOutput->EndArray();
    }
}

void CommandContext::StartStruct() const
{
    if (pOutput) {
        pOutput->StartStruct();
    }
}

void CommandContext::EndStruct() const
{
    if (pOutput) {
        pOutput->EndStruct();
    }
}

void CommandContext::StartField(const wxString& name) const
{
    if (pOutput) {
        pOutput->StartField(name);
    }
}

void CommandContext::EndField() const
{
    if (pOutput) {
        pOutput->EndField();
    }
}

void CommandContext::AddItem(const wxString& value, const wxString& name) const
{
    if (pOutput) {
        pOutput->AddItem(value, name);
    }
}

void CommandContext::AddBool(const bool value, const wxString& name) const
{
    if (pOutput) {
        pOutput->AddItem(value, name);
    }
}

void CommandContext::AddItem(const double value, const wxString& name) const
{
    if (pOutput) {
        pOutput->AddItem(value, name);
    }
}
