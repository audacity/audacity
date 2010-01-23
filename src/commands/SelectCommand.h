/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: GPL v2 - see LICENSE.txt

   Dan Horgan

******************************************************************//**

\file SelectCommand.h
\brief Declarations for SelectCommand and SelectCommandType classes

*//*******************************************************************/

#ifndef __SELECTCOMMAND__
#define __SELECTCOMMAND__

#include "CommandType.h"
#include "Command.h"

class SelectCommandType : public CommandType
{
public:
   virtual wxString BuildName();
   virtual void BuildSignature(CommandSignature &signature);
   virtual Command *Create(CommandOutputTarget *target);
};

class SelectCommand : public CommandImplementation
{
public:
   SelectCommand(SelectCommandType &type, CommandOutputTarget *target)
      : CommandImplementation(type, target) { }
   virtual bool Apply(CommandExecutionContext context);
};

#endif /* End of include guard: __SELECTCOMMAND__ */
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
