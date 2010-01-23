/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan

******************************************************************//**

\file SetTrackInfoCommand.h
\brief Declarations of SetTrackInfoCommand and SetTrackInfoCommandType classes

*//*******************************************************************/

#ifndef __SETTRACKINFOCOMMAND__
#define __SETTRACKINFOCOMMAND__

#include "Command.h"
#include "CommandType.h"

class SetTrackInfoCommandType : public CommandType
{
public:
   virtual wxString BuildName();
   virtual void BuildSignature(CommandSignature &signature);
   virtual Command *Create(CommandOutputTarget *target);
};

class SetTrackInfoCommand : public CommandImplementation
{
public:
   SetTrackInfoCommand(CommandType &type, CommandOutputTarget *target)
      : CommandImplementation(type, target)
   { }
   virtual ~SetTrackInfoCommand()
   { }

   virtual bool Apply(CommandExecutionContext context);
};

#endif /* End of include guard: __SETTRACKINFOCOMMAND__ */

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
