/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: GPL v2 or later - see LICENSE.txt

   Dan Horgan

******************************************************************//**

\file CommandTargets
\brief Contains classes for 'command output targets' - objects which can
receive output from a command. For instance, a progress target might pass the
information to a GUI ProgressDialog. Using abstract targets means the command
objects needn't be concerned with what happens to the information.

Note: currently, reusing target objects is not generally safe - perhaps they
should be reference-counted.

\class CommandMessageTargetDecorator
\brief CommandMessageTargetDecorator is a CommandOutputTarget that forwards
its work on to another one.  Typically we derive from it to modify some
functionality and forward the rest.

\class BriefCommandMessageTarget
\brief BriefCommandMessageTarget is a CommandOutputTarget that provides
status in a briefer listing

\class LispyCommandMessageTarget
\brief LispyCommandMessageTarget is a CommandOutputTarget that provides status
in a lispy style.

\class CommandOutputTargets
\brief CommandOutputTargets a mix of three output classes to output
progress indication, status messages and errors.

\class BriefCommandOutputTargets
\brief BriefCommandOutputTargets is a CommandOutputTargets that  replaces the
status message target with the BriefCommandMessageTarget version.

\class LispifiedCommandOutputTargets
\brief LispifiedCommandOutputTargets is a CommandOutputTargets that  replaces the
status message target with the LispyCommandMessageTarget version.

\class ProgressToMessageTarget
\brief ProgressToMessageTarget formats the percentage complete text as a message
and sends it to that message target.

*//*******************************************************************/

#ifndef __COMMANDTARGETS__
#define __COMMANDTARGETS__

#include <memory>
#include <vector>
#include <wx/string.h>
#include <wx/thread.h>

class wxStatusBar;

/// Interface for objects that can receive command progress information
class CommandProgressTarget /* not final */
{
public:
    virtual ~CommandProgressTarget();
    virtual void Update(double completed) = 0;
};

/// Interface for objects that can receive (string) messages from a command
class MENUS_API CommandMessageTarget /* not final */
{
public:
    CommandMessageTarget() { mCounts.push_back(0); }
    virtual ~CommandMessageTarget();
    virtual void Update(const wxString& message) = 0;
    virtual void StartArray();
    virtual void EndArray();
    virtual void StartStruct();
    virtual void EndStruct();
    virtual void AddItem(const wxString& value, const wxString& name = {});
    virtual void AddBool(const bool value, const wxString& name = {});
    virtual void AddItem(const double value, const wxString& name = {});
    virtual void StartField(const wxString& name = {});
    virtual void EndField();
    virtual void Flush();
    wxString Escaped(const wxString& str);
    std::vector<int> mCounts;
};

class MENUS_API CommandMessageTargetDecorator : public CommandMessageTarget
{
public:
    CommandMessageTargetDecorator(CommandMessageTarget& target)
        : mTarget(target) {}
    ~CommandMessageTargetDecorator() override;
    void Update(const wxString& message) override { mTarget.Update(message); }
    void StartArray() override { mTarget.StartArray(); }
    void EndArray() override { mTarget.EndArray(); }
    void StartStruct() override { mTarget.StartStruct(); }
    void EndStruct() override { mTarget.EndStruct(); }
    void AddItem(const wxString& value, const wxString& name = {}) override
    { mTarget.AddItem(value, name); }
    void AddBool(const bool value, const wxString& name = {}) override
    { mTarget.AddBool(value, name); }
    void AddItem(const double value, const wxString& name = {}) override
    { mTarget.AddItem(value, name); }
    void StartField(const wxString& name = {}) override
    { mTarget.StartField(name); }
    void EndField() override
    { mTarget.EndField(); }
    void Flush() override
    { mTarget.Flush(); }
    CommandMessageTarget& mTarget;
};

class MENUS_API LispyCommandMessageTarget : public CommandMessageTargetDecorator /* not final */
{
public:
    LispyCommandMessageTarget(CommandMessageTarget& target)
        : CommandMessageTargetDecorator(target) {}
    ~LispyCommandMessageTarget() override;
    virtual void StartArray() override;
    virtual void EndArray() override;
    virtual void StartStruct() override;
    virtual void EndStruct() override;
    virtual void AddItem(const wxString& value, const wxString& name = {})
    override;
    virtual void AddBool(const bool value, const wxString& name = {})
    override;
    virtual void AddItem(const double value, const wxString& name = {})
    override;
    virtual void StartField(const wxString& name = {})override;
    virtual void EndField() override;
};

class MENUS_API BriefCommandMessageTarget : public CommandMessageTargetDecorator /* not final */
{
public:
    BriefCommandMessageTarget(CommandMessageTarget& target)
        : CommandMessageTargetDecorator(target) {}
    ~BriefCommandMessageTarget() override;
    virtual void StartArray() override;
    virtual void EndArray() override;
    virtual void StartStruct() override;
    virtual void EndStruct() override;
    virtual void AddItem(const wxString& value, const wxString& name = {})
    override;
    virtual void AddBool(const bool value, const wxString& name = {})
    override;
    virtual void AddItem(const double value, const wxString& name = {})
    override;
    virtual void StartField(const wxString& name = {})override;
    virtual void EndField() override;
};

/// Used to ignore a command's progress updates
class MENUS_API NullProgressTarget final : public CommandProgressTarget
{
public:
    ~NullProgressTarget() override;
    void Update(double WXUNUSED(completed)) override {}
};

///
class MENUS_API ProgressToMessageTarget final : public CommandProgressTarget
{
private:
    std::unique_ptr<CommandMessageTarget> mTarget;
public:
    ProgressToMessageTarget(std::unique_ptr<CommandMessageTarget>&& target)
        : mTarget(std::move(target))
    { }
    ~ProgressToMessageTarget() override;
    void Update(double completed) override
    {
        mTarget->Update(wxString::Format(wxT("%.2f%%"), completed * 100));
    }
};

/// Used to ignore a command's message updates
class MENUS_API NullMessageTarget final : public CommandMessageTarget
{
public:
    ~NullMessageTarget() override;
    void Update(const wxString&) override {}
};

/// Displays messages from a command in a BasicUI::MessageBox
class MENUS_API MessageBoxTarget final : public CommandMessageTarget
{
public:
    ~MessageBoxTarget() override;
    void Update(const wxString& message) override;
};

/// Constructs a response (to be sent back to a script)
class MENUS_API ResponseTarget final : public CommandMessageTarget
{
private:
    wxSemaphore mSemaphore;
    wxString mBuffer;
public:
    ResponseTarget()
        : mSemaphore(0, 1),
        mBuffer(wxEmptyString)
    {
        // Cater for handling long responses quickly.
        mBuffer.Alloc(40000);
    }

    ~ResponseTarget() override;
    void Update(const wxString& message) override
    {
        mBuffer += message;
    }

    virtual void Flush() override
    {
        mSemaphore.Post();
    }

    wxString GetResponse()
    {
        mSemaphore.Wait();
        return mBuffer;
    }
};

/// Sends messages to two message targets at once
class MENUS_API CombinedMessageTarget final : public CommandMessageTarget
{
private:
    std::unique_ptr<CommandMessageTarget> m1, m2;
public:
    CombinedMessageTarget(std::unique_ptr<CommandMessageTarget>&& t1,
                          std::unique_ptr<CommandMessageTarget>&& t2)
        : m1(std::move(t1)), m2(std::move(t2))
    {
        wxASSERT(m1);
        wxASSERT(m2);
    }

    ~CombinedMessageTarget() override;
    void Update(const wxString& message) override
    {
        m1->Update(message);
        m2->Update(message);
    }
};

/**
\class TargetFactory
\brief TargetFactory makes Command output targets.
 By default, we ignore progress updates but display all other messages directly
*/
class TargetFactory
{
public:
    static std::unique_ptr<CommandProgressTarget> ProgressDefault()
    {
        return std::make_unique<NullProgressTarget>();
    }

    static std::shared_ptr<CommandMessageTarget> MessageDefault()
    {
        return std::make_shared<MessageBoxTarget>();
    }
};

/// Used to aggregate the various output targets a command may have.
/// Assumes responsibility for pointers passed into it.
/// mProgressTarget is a unique pointer, but mStatusTraget and
/// mErrorTarget are shared ones, because they may both point to the same
/// output
class CommandOutputTargets /* not final */
{
public:
    std::unique_ptr<CommandProgressTarget> mProgressTarget;
    std::shared_ptr<CommandMessageTarget> mStatusTarget;
    std::shared_ptr<CommandMessageTarget> mErrorTarget;
public:
    CommandOutputTargets(
        std::unique_ptr<CommandProgressTarget> pt =
        TargetFactory::ProgressDefault(),
        std::shared_ptr<CommandMessageTarget> st =
        TargetFactory::MessageDefault(),
        std::shared_ptr<CommandMessageTarget> et =
        TargetFactory::MessageDefault())
        : mProgressTarget(std::move(pt)), mStatusTarget(st), mErrorTarget(et)
    { }
    ~CommandOutputTargets()
    {
    }

    // Lots of forwarding...
    void Progress(double completed)
    {
        if (mProgressTarget) {
            mProgressTarget->Update(completed);
        }
    }

    void Status(const wxString& status, bool bFlush=false)
    {
        if (mStatusTarget) {
            mStatusTarget->Update(status);
            if (bFlush) {
                mStatusTarget->Flush();
            }
        }
    }

    void Error(const wxString& message)
    {
        if (mErrorTarget) {
            mErrorTarget->Update(message);
        }
    }

    void StartArray()
    {
        if (mStatusTarget) {
            mStatusTarget->StartArray();
        }
    }

    void EndArray()
    {
        if (mStatusTarget) {
            mStatusTarget->EndArray();
        }
    }

    void StartStruct()
    {
        if (mStatusTarget) {
            mStatusTarget->StartStruct();
        }
    }

    void EndStruct()
    {
        if (mStatusTarget) {
            mStatusTarget->EndStruct();
        }
    }

    void StartField(const wxString& name)
    {
        if (mStatusTarget) {
            mStatusTarget->StartField(name);
        }
    }

    void EndField()
    {
        if (mStatusTarget) {
            mStatusTarget->EndField();
        }
    }

    void AddItem(const wxString& value, const wxString& name = {})
    {
        if (mStatusTarget) {
            mStatusTarget->AddItem(value, name);
        }
    }

    void AddBool(const bool value, const wxString& name = {})
    {
        if (mStatusTarget) {
            mStatusTarget->AddItem(value, name);
        }
    }

    void AddItem(const double value, const wxString& name = {})
    {
        if (mStatusTarget) {
            mStatusTarget->AddItem(value, name);
        }
    }
};

class MENUS_API LispifiedCommandOutputTargets : public CommandOutputTargets
{
public:
    LispifiedCommandOutputTargets(CommandOutputTargets& target);
    ~LispifiedCommandOutputTargets();
private:
    CommandOutputTargets* pToRestore;
};

class MENUS_API BriefCommandOutputTargets : public CommandOutputTargets
{
public:
    BriefCommandOutputTargets(CommandOutputTargets& target);
    ~BriefCommandOutputTargets();
private:
    CommandOutputTargets* pToRestore;
};

#endif /* End of include guard: __COMMANDTARGETS__ */
