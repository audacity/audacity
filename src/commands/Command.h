/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   File License: wxWidgets

   Dan Horgan

******************************************************************//**

\file Command.h
\brief Contains declaration of Command base class.

\class CommandExecutionContext
\brief Represents a context to which a command may be applied.

\class Command
\brief Base class which encapsulates a process.

That process may depend on certain parameters (determined by the command's
signature) and may produce output on various channels. Any process which is to
be controlled by a script should be separated out into its own Command class.
(And that class should be registered with the CommandDirectory).

*//*******************************************************************/

#ifndef __COMMAND__
#define __COMMAND__

#include <wx/app.h>

#include "../Project.h"

#include "CommandMisc.h"
#include "CommandSignature.h"

class AudacityApp;
class CommandOutputTarget;

class CommandExecutionContext
{
public:
   CommandExecutionContext(AudacityApp *WXUNUSED(app), AudacityProject *WXUNUSED(proj))
   {
   };
   AudacityApp *GetApp() const
   {
      return (AudacityApp *) wxTheApp;
   };
   AudacityProject *GetProject() const
   {
      // TODO:  Presumably, this would be different if running in a command context.
      // So, if this command system is ever actually enabled, then this will need to
      // be reviewed.
      return GetActiveProject();
   };
};

// Interface
class Command /* not final */
{
public:
   virtual void Progress(double completed) = 0;
   virtual void Status(const wxString &message) = 0;
   virtual void Error(const wxString &message) = 0;
   virtual ~Command() { }
   virtual wxString GetName() = 0;
   virtual CommandSignature &GetSignature() = 0;
   virtual bool SetParameter(const wxString &paramName, const wxVariant &paramValue);
   virtual bool Apply(CommandExecutionContext context) = 0;
};

using CommandHolder = std::shared_ptr<Command>;

// Command which wraps another command
class DecoratedCommand /* not final */ : public Command
{
protected:
   CommandHolder mCommand;
public:
   void Progress(double completed) override;
   void Status(const wxString &message) override;
   void Error(const wxString &message) override;

   DecoratedCommand(const CommandHolder &cmd)
      : mCommand(cmd)
   {
      wxASSERT(cmd != NULL);
   }
   virtual ~DecoratedCommand();
   wxString GetName() override;
   CommandSignature &GetSignature() override;
   bool SetParameter(const wxString &paramName, const wxVariant &paramValue) override;
};

// Decorator command that performs the given command and then outputs a status
// message according to the result
class ApplyAndSendResponse final : public DecoratedCommand
{
public:
   ApplyAndSendResponse(const CommandHolder &cmd)
      : DecoratedCommand(cmd)
   { }

   bool Apply(CommandExecutionContext context) override;
};

class CommandImplementation /* not final */ : public Command
{
private:
   CommandType &mType;
   ParamValueMap mParams;

   /// Using the command signature, looks up a possible parameter value and
   /// checks whether it passes the validator.
   bool Valid(const wxString &paramName, const wxVariant &paramValue);

protected:
   std::unique_ptr<CommandOutputTarget> mOutput;

   // Convenience methods for allowing subclasses to access parameters
   void TypeCheck(const wxString &typeName,
                  const wxString &paramName,
                  const wxVariant &param);
   void CheckParam(const wxString &paramName);
   bool GetBool(const wxString &paramName);
   long GetLong(const wxString &paramName);
   double GetDouble(const wxString &paramName);
   wxString GetString(const wxString &paramName);

public:
   // Convenience methods for passing messages to the output target
   void Progress(double completed);
   void Status(const wxString &status) override;
   void Error(const wxString &message) override;

   /// Constructor should not be called directly; only by a factory which
   /// ensures name and params are set appropriately for the command.
   CommandImplementation(CommandType &type,
                         std::unique_ptr<CommandOutputTarget> &&output);

   virtual ~CommandImplementation();

   /// An instance method for getting the command name (for consistency)
   wxString GetName();

   /// Get the signature of the command
   CommandSignature &GetSignature();

   /// Attempt to one of the command's parameters to a particular value.
   /// (Note: wxVariant is reference counted)
   bool SetParameter(const wxString &paramName, const wxVariant &paramValue);

   // Subclasses should override the following:
   // =========================================

   /// Actually carry out the command. Return true if successful and false
   /// otherwise.
   bool Apply(CommandExecutionContext context) override;
};

#endif /* End of include guard: __COMMAND__ */
