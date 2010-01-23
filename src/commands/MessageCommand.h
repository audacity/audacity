/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   File License: wxWidgets

   Dan Horgan

******************************************************************//**

\file MessageCommand.h
\brief Contains definition of MessageCommand class.

*//***************************************************************//***

\class MessageCommand
\brief Command to send a message (currently on the status channel)

*//*******************************************************************/

#ifndef __MESSAGECOMMAND__
#define __MESSAGECOMMAND__

#include "Command.h"
#include "CommandType.h"

class MessageCommandType : public CommandType
{
public:
   virtual wxString BuildName();
   virtual void BuildSignature(CommandSignature &signature);
   virtual Command *Create(CommandOutputTarget *target);
};

class MessageCommand : public CommandImplementation
{
public:
   MessageCommand(CommandType &type,
                  CommandOutputTarget *target)
      : CommandImplementation(type, target) {}
   virtual bool Apply(CommandExecutionContext context);
};

#endif /* End of include guard: __MESSAGECOMMAND__ */

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
