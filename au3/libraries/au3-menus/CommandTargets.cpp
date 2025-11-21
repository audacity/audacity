/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan

******************************************************************//**

\file CommandTargets.cpp
\brief Contains definitions for CommandType class

*//*******************************************************************/
#include "CommandTargets.h"
#include "BasicUI.h"
#include <locale>
#include <sstream>

CommandProgressTarget::~CommandProgressTarget() = default;

CommandMessageTarget::~CommandMessageTarget()
{
    Flush();
}

void CommandMessageTarget::StartArray()
{
    wxString Padding;
    Padding.Pad(mCounts.size() * 2 - 2);
    Update(wxString::Format("%s%s[ ", (mCounts.back() > 0) ? ",\n" : "\n", Padding));
    mCounts.back() += 1;
    mCounts.push_back(0);
}

void CommandMessageTarget::EndArray()
{
    if (mCounts.size() > 1) {
        mCounts.pop_back();
    }
    Update(" ]");
}

void CommandMessageTarget::StartStruct()
{
    wxString Padding;
    Padding.Pad(mCounts.size() * 2 - 2);
    Update(wxString::Format("%s%s{ ", (mCounts.back() > 0) ? ",\n" : "\n", Padding));
    mCounts.back() += 1;
    mCounts.push_back(0);
}

void CommandMessageTarget::EndStruct()
{
    if (mCounts.size() > 1) {
        mCounts.pop_back();
    }
    Update(" }");
}

void CommandMessageTarget::AddItem(const wxString& value, const wxString& name)
{
    wxString Padding;
    Padding.Pad(mCounts.size() * 2 - 2);
    Padding = ((value.length() < 15) || (mCounts.back() <= 0)) ? wxString{} : wxString("\n") + Padding;
    if (name.empty()) {
        Update(wxString::Format("%s%s\"%s\"", (mCounts.back() > 0) ? ", " : "", Padding, Escaped(value)));
    } else {
        Update(wxString::Format("%s%s\"%s\":\"%s\"", (mCounts.back() > 0) ? ", " : "", Padding, name, Escaped(value)));
    }
    mCounts.back() += 1;
}

void CommandMessageTarget::AddBool(const bool value,      const wxString& name)
{
    if (name.empty()) {
        Update(wxString::Format("%s\"%s\"", (mCounts.back() > 0) ? ", " : "", value ? "true" : "false"));
    } else {
        Update(wxString::Format("%s\"%s\":\"%s\"", (mCounts.back() > 0) ? ", " : "", name, value ? "true" : "false"));
    }
    mCounts.back() += 1;
}

void CommandMessageTarget::AddItem(const double value,    const wxString& name)
{
    std::stringstream str;
    std::locale nolocale("C");
    str.imbue(nolocale);

    if (name.empty()) {
        str << ((mCounts.back() > 0) ? ", " : "") << value;
    } else {
        str << ((mCounts.back() > 0) ? ", " : "") << "\"" << name << "\"" << ":" << value;
    }

    Update(str.str());
    mCounts.back() += 1;
}

void CommandMessageTarget::StartField(const wxString& name)
{
    if (name.empty()) {
        Update(wxString::Format("%s", (mCounts.back() > 0) ? ", " : ""));
    } else {
        Update(wxString::Format("%s\"%s\":", (mCounts.back() > 0) ? ", " : "", name));
    }
    mCounts.back() += 1;
    mCounts.push_back(0);
}

void CommandMessageTarget::EndField()
{
    if (mCounts.size() > 1) {
        mCounts.pop_back();
    }
}

void CommandMessageTarget::Flush()
{
}

wxString CommandMessageTarget::Escaped(const wxString& str)
{
    wxString Temp = str;
    Temp.Replace("\"", "\\\"");
    return Temp;
}

CommandMessageTargetDecorator::~CommandMessageTargetDecorator() = default;

LispyCommandMessageTarget::~LispyCommandMessageTarget() = default;

void LispyCommandMessageTarget::StartArray()
{
    wxString Padding;
    Padding.Pad(mCounts.size() * 2 - 2);
    Update(wxString::Format((mCounts.back() > 0) ? "\n%s(" : "(", Padding));
    mCounts.back() += 1;
    mCounts.push_back(0);
}

void LispyCommandMessageTarget::EndArray()
{
    if (mCounts.size() > 1) {
        mCounts.pop_back();
    }
    Update(")");
}

void LispyCommandMessageTarget::StartStruct()
{
    wxString Padding;
    Padding.Pad(mCounts.size() * 2 - 2);
    Update(wxString::Format((mCounts.back() > 0) ? "\n%s(" : "(", Padding));
    mCounts.back() += 1;
    mCounts.push_back(0);
}

void LispyCommandMessageTarget::EndStruct()
{
    if (mCounts.size() > 1) {
        mCounts.pop_back();
    }
    Update(")");
}

void LispyCommandMessageTarget::AddItem(const wxString& value, const wxString& name)
{
    wxString Padding;
    if (name.empty()) {
        Update(wxString::Format("%s%s\"%s\"", (mCounts.back() > 0) ? " " : "", Padding, Escaped(value)));
    } else {
        Update(wxString::Format("%s%s(%s \"%s\")", (mCounts.back() > 0) ? " " : "", Padding, name, Escaped(value)));
    }
    mCounts.back() += 1;
}

void LispyCommandMessageTarget::AddBool(const bool value,      const wxString& name)
{
    if (name.empty()) {
        Update(wxString::Format("%s%s", (mCounts.back() > 0) ? " " : "", value ? "True" : "False"));
    } else {
        Update(wxString::Format("%s(%s %s)", (mCounts.back() > 0) ? " " : "", name, value ? "True" : "False"));
    }
    mCounts.back() += 1;
}

void LispyCommandMessageTarget::AddItem(const double value,    const wxString& name)
{
    if (name.empty()) {
        Update(wxString::Format("%s%g", (mCounts.back() > 0) ? " " : "", value));
    } else {
        Update(wxString::Format("%s(%s %g)", (mCounts.back() > 0) ? " " : "", name, value));
    }
    mCounts.back() += 1;
}

void LispyCommandMessageTarget::StartField(const wxString& name)
{
    Update(wxString::Format("%s(%s", (mCounts.back() > 0) ? " " : "", name));
    mCounts.back() += 1;
    mCounts.push_back(0);
}

void LispyCommandMessageTarget::EndField()
{
    if (mCounts.size() > 1) {
        mCounts.pop_back();
    }
    Update(")");
}

BriefCommandMessageTarget::~BriefCommandMessageTarget() = default;

void BriefCommandMessageTarget::StartArray()
{
    wxString Padding;
    Padding.Pad(mCounts.size() * 2 - 2);
    if (mCounts.size() <= 3) {
        Update(wxString::Format("%s%s ", (mCounts.back() > 0) ? " \n" : "", Padding));
    }
    mCounts.back() += 1;
    mCounts.push_back(0);
}

void BriefCommandMessageTarget::EndArray()
{
    if (mCounts.size() > 1) {
        mCounts.pop_back();
    }
    if (mCounts.size() <= 3) {
        Update(" ");
    }
}

void BriefCommandMessageTarget::StartStruct()
{
    wxString Padding;
    Padding.Pad(mCounts.size() * 2 - 2);
    if (mCounts.size() <= 3) {
        Update(wxString::Format("%s%s ", (mCounts.back() > 0) ? " \n" : "", Padding));
    }
    mCounts.back() += 1;
    mCounts.push_back(0);
}

void BriefCommandMessageTarget::EndStruct()
{
    if (mCounts.size() > 1) {
        mCounts.pop_back();
    }
    if (mCounts.size() <= 3) {
        Update(" ");
    }
}

void BriefCommandMessageTarget::AddItem(const wxString& value, const wxString& WXUNUSED(name))
{
    if (mCounts.size() <= 3) {
        Update(wxString::Format("%s\"%s\"", (mCounts.back() > 0) ? " " : "", Escaped(value)));
    }
    mCounts.back() += 1;
}

void BriefCommandMessageTarget::AddBool(const bool value,      const wxString& WXUNUSED(name))
{
    if (mCounts.size() <= 3) {
        Update(wxString::Format("%s%s", (mCounts.back() > 0) ? " " : "", value ? "True" : "False"));
    }
    mCounts.back() += 1;
}

void BriefCommandMessageTarget::AddItem(const double value,    const wxString& WXUNUSED(name))
{
    if (mCounts.size() <= 3) {
        Update(wxString::Format("%s%g", (mCounts.back() > 0) ? " " : "", value));
    }
    mCounts.back() += 1;
}

void BriefCommandMessageTarget::StartField(const wxString& WXUNUSED(name))
{
    mCounts.back() += 1;
    mCounts.push_back(0);
}

void BriefCommandMessageTarget::EndField()
{
    if (mCounts.size() > 1) {
        mCounts.pop_back();
    }
}

NullProgressTarget::~NullProgressTarget() = default;

ProgressToMessageTarget::~ProgressToMessageTarget() = default;

NullMessageTarget::~NullMessageTarget() = default;

MessageBoxTarget::~MessageBoxTarget() = default;

void MessageBoxTarget::Update(const wxString& message)
{
    // Should these messages be localized?
    BasicUI::ShowMessageBox(Verbatim(message));
}

ResponseTarget::~ResponseTarget() = default;

CombinedMessageTarget::~CombinedMessageTarget() = default;

LispifiedCommandOutputTargets::LispifiedCommandOutputTargets(CommandOutputTargets& target)
    : CommandOutputTargets(),
    pToRestore(&target)
{
    mProgressTarget = std::move(target.mProgressTarget),
    mStatusTarget = std::make_shared<LispyCommandMessageTarget>(*target.mStatusTarget.get()),
    mErrorTarget = std::move(target.mErrorTarget);
}

LispifiedCommandOutputTargets::~LispifiedCommandOutputTargets()
{
    pToRestore->mProgressTarget = std::move(mProgressTarget);
    //The status was never captured so does not need restoring.
    //pToRestore->mStatusTarget = std::move( mStatusTarget );
    pToRestore->mErrorTarget = std::move(mErrorTarget);
}

BriefCommandOutputTargets::BriefCommandOutputTargets(CommandOutputTargets& target)
    : CommandOutputTargets(),
    pToRestore(&target)
{
    mProgressTarget = std::move(target.mProgressTarget),
    mStatusTarget = std::make_shared<BriefCommandMessageTarget>(*target.mStatusTarget.get()),
    mErrorTarget = std::move(target.mErrorTarget);
}

BriefCommandOutputTargets::~BriefCommandOutputTargets()
{
    pToRestore->mProgressTarget = std::move(mProgressTarget);
    //The status was never captured so does not need restoring.
    //pToRestore->mStatusTarget = std::move( mStatusTarget );
    pToRestore->mErrorTarget = std::move(mErrorTarget);
}
