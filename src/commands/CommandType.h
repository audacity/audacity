/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan

******************************************************************//**

\file CommandType.h
\brief Contains declarations for CommandType class

*//*******************************************************************/

#ifndef __COMMANDTYPE__
#define __COMMANDTYPE__

#include "CommandMisc.h"
#include "CommandSignature.h"
#include "../MemoryX.h"

class Command;
using CommandHolder = std::shared_ptr<Command>;
class CommandOutputTarget;
class CommandSignature;
class wxString;

class CommandType /* not final */
{
private:
   wxString mName;
   Maybe<CommandSignature> mSignature;

public:
   CommandType();
   virtual ~CommandType();
   const wxString &GetName();
   CommandSignature &GetSignature();
   wxString Describe();

   // Subclasses should override the following:
   // =========================================

   // Return the name of the command type
   virtual wxString BuildName() = 0;

   /// Postcondition: signature is a 'signature' map containing parameter
   // names, validators and default values.
   virtual void BuildSignature(CommandSignature &signature) = 0;

   // Create a command instance with the specified output target
   virtual CommandHolder Create(std::unique_ptr<CommandOutputTarget> &&target) = 0;
};

#endif /* End of include guard: __COMMANDTYPE__ */
