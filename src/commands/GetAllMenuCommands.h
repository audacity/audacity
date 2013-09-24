/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxWidgets

   Dan Horgan

******************************************************************//**

\file GetAllMenuCommands.h
\brief Contains declaration of GetAllMenuCommands class.

\class GetAllMenuCommands
\brief Command which outputs a list of available menu commands on the status
channel.

*//*******************************************************************/

#ifndef __GETALLMENUCOMMANDS__
#define __GETALLMENUCOMMANDS__

#include "Command.h"
#include "CommandType.h"

class GetAllMenuCommandsType : public CommandType
{
public:
   virtual wxString BuildName();
   virtual void BuildSignature(CommandSignature &signature);
   virtual Command *Create(CommandOutputTarget *target);
};

class GetAllMenuCommands : public CommandImplementation
{
public:
   GetAllMenuCommands(CommandType &type,
                      CommandOutputTarget *target)
      : CommandImplementation(type, target)
   { }

   virtual ~GetAllMenuCommands()
   { }

   virtual bool Apply(CommandExecutionContext context);
};

#endif /* End of include guard: __GETALLMENUCOMMANDS__ */
