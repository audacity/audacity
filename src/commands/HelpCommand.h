/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan

******************************************************************//**

\file HelpCommand
\brief Declarations of HelpCommand and HelpCommandType classes

\class HelpCommand
\brief Command which returns information about the given command

*//*******************************************************************/

#ifndef __HELPCOMMAND__
#define __HELPCOMMAND__

#include "CommandType.h"
#include "Command.h"

class HelpCommandType : public CommandType
{
public:
   virtual wxString BuildName();
   virtual void BuildSignature(CommandSignature &signature);
   virtual Command *Create(CommandOutputTarget *target);
};

class HelpCommand : public CommandImplementation
{
public:
   HelpCommand(HelpCommandType &type, CommandOutputTarget *target)
      : CommandImplementation(type, target) { }
   virtual bool Apply(CommandExecutionContext context);
};

#endif /* End of include guard: __HELPCOMMAND__ */

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
