/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2009 Audacity Team.
   File License: wxwidgets

   BatchEvalCommand.h
   Dan Horgan

******************************************************************//**

\class BatchEvalCommand
\brief Given a string representing a command, pass it to the BatchCommands
system.

The eventual aim is to move the code from BatchCommands out into separate
command classes, but until this happens, BatchEvalCommand can act as a 'bridge'
to that system.

*//*******************************************************************/

#ifndef __BATCHEVALCOMMAND__
#define __BATCHEVALCOMMAND__

#include "Command.h"
#include "CommandType.h"
#include "../BatchCommands.h"

class BatchEvalCommandType : public CommandType
{
public:
   virtual wxString BuildName();
   virtual void BuildSignature(CommandSignature &signature);
   virtual Command *Create(CommandOutputTarget *target);
};

class BatchEvalCommand : public CommandImplementation
{
public:
   BatchEvalCommand(CommandType &type,
                    CommandOutputTarget *target)
      : CommandImplementation(type, target)
   { }

   virtual ~BatchEvalCommand();
   virtual bool Apply(CommandExecutionContext context);
};

#endif /* End of include guard: __BATCHEVALCOMMAND__ */

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: TBD
