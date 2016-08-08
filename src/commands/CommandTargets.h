/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: GPL v2 - see LICENSE.txt

   Dan Horgan

******************************************************************//**

\file CommandTargets
\brief Contains classes for 'command output targets' - objects which can
receive output from a command. For instance, a progress target might pass the
information to a GUI ProgressDialog. Using abstract targets means the command
objects needn't be concerned with what happens to the information.

Note: currently, reusing target objects is not generally safe - perhaps they
should be reference-counted.

*//*******************************************************************/

#ifndef __COMMANDTARGETS__
#define __COMMANDTARGETS__

#include "../MemoryX.h"
#include <wx/string.h>
#include <wx/msgdlg.h>
#include <wx/statusbr.h>
#include "../widgets/ProgressDialog.h"
#include "../commands/ResponseQueue.h"
#include "../src/Project.h"

/// Interface for objects that can receive command progress information
class CommandProgressTarget /* not final */
{
public:
   virtual ~CommandProgressTarget() {}
   virtual void Update(double completed) = 0;
};

/// Used to ignore a command's progress updates
class NullProgressTarget final : public CommandProgressTarget
{
public:
   virtual ~NullProgressTarget() {}
   void Update(double WXUNUSED(completed)) override {}
};

/// Sends command progress information to a ProgressDialog
class GUIProgressTarget final : public CommandProgressTarget
{
private:
   ProgressDialog &mProgress;
public:
   GUIProgressTarget(ProgressDialog &pd)
      : mProgress(pd)
   {}
   virtual ~GUIProgressTarget() {}
   void Update(double completed) override
   {
      mProgress.Update(completed);
   }
};

/// Interface for objects that can receive (string) messages from a command
class CommandMessageTarget /* not final */
{
public:
   virtual ~CommandMessageTarget() {}
   virtual void Update(const wxString &message) = 0;
};

///
class ProgressToMessageTarget final : public CommandProgressTarget
{
private:
   std::unique_ptr<CommandMessageTarget> mTarget;
public:
   ProgressToMessageTarget(std::unique_ptr<CommandMessageTarget> &&target)
      : mTarget(std::move(target))
   { }
   virtual ~ProgressToMessageTarget()
   {
   }
   void Update(double completed) override
   {
      mTarget->Update(wxString::Format(wxT("%.2f%%"), completed*100));
   }
};

/// Used to ignore a command's message updates
class NullMessageTarget final : public CommandMessageTarget
{
public:
   virtual ~NullMessageTarget() {}
   void Update(const wxString &) override {}
};

/// Displays messages from a command in a wxMessageBox
class MessageBoxTarget final : public CommandMessageTarget
{
public:
   virtual ~MessageBoxTarget() {}
   void Update(const wxString &message) override
   {
      wxMessageBox(message);
   }
};

/// Displays messages from a command in a wxStatusBar
class StatusBarTarget final : public CommandMessageTarget
{
private:
   wxStatusBar &mStatus;
public:
   StatusBarTarget(wxStatusBar &sb)
      : mStatus(sb)
   {}
   void Update(const wxString &message) override
   {
      mStatus.SetStatusText(message, 0);
   }
};

/// Adds messages to a response queue (to be sent back to a script)
class ResponseQueueTarget final : public CommandMessageTarget
{
private:
   ResponseQueue &mResponseQueue;
public:
   ResponseQueueTarget(ResponseQueue &responseQueue)
      : mResponseQueue(responseQueue)
   { }
   virtual ~ResponseQueueTarget()
   {
      mResponseQueue.AddResponse(wxString(wxT("\n")));
   }
   void Update(const wxString &message) override
   {
      mResponseQueue.AddResponse(message);
   }
};

/// Sends messages to two message targets at once
class CombinedMessageTarget final : public CommandMessageTarget
{
private:
   std::unique_ptr<CommandMessageTarget> m1, m2;
public:
   CombinedMessageTarget(std::unique_ptr<CommandMessageTarget> &&t1,
                         std::unique_ptr<CommandMessageTarget> &&t2)
      : m1(std::move(t1)), m2(std::move(t2))
   {
      wxASSERT(m1);
      wxASSERT(m2);
   }
   ~CombinedMessageTarget()
   {
   }
   void Update(const wxString &message) override
   {
      m1->Update(message);
      m2->Update(message);
   }
};


// By default, we ignore progress updates but display all other messages
// directly
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
class CommandOutputTarget
{
private:
   std::unique_ptr<CommandProgressTarget> mProgressTarget;
   std::shared_ptr<CommandMessageTarget> mStatusTarget;
   std::shared_ptr<CommandMessageTarget> mErrorTarget;
public:
   CommandOutputTarget(std::unique_ptr<CommandProgressTarget> &&pt = TargetFactory::ProgressDefault(),
                       std::shared_ptr<CommandMessageTarget>  &&st = TargetFactory::MessageDefault(),
                       std::shared_ptr<CommandMessageTarget> &&et = TargetFactory::MessageDefault())
      : mProgressTarget(std::move(pt)), mStatusTarget(st), mErrorTarget(et)
   { }
   ~CommandOutputTarget()
   {
   }
   void Progress(double completed)
   {
      if (mProgressTarget)
         mProgressTarget->Update(completed);
   }
   void Status(const wxString &status)
   {
      if (mStatusTarget)
         mStatusTarget->Update(status);
   }
   void Error(const wxString &message)
   {
      if (mErrorTarget)
         mErrorTarget->Update(message);
   }
};

#endif /* End of include guard: __COMMANDTARGETS__ */
