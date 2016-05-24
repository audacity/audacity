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

class GetAllMenuCommandsType final : public CommandType
{
public:
   wxString BuildName() override;
   void BuildSignature(CommandSignature &signature) override;
   CommandHolder Create(std::unique_ptr<CommandOutputTarget> &&target) override;
};

class GetAllMenuCommands final : public CommandImplementation
{
public:
   GetAllMenuCommands(CommandType &type,
                      std::unique_ptr<CommandOutputTarget> &&target)
      : CommandImplementation(type, std::move(target))
   { }

   virtual ~GetAllMenuCommands()
   { }

   bool Apply(CommandExecutionContext context) override;
};

#endif /* End of include guard: __GETALLMENUCOMMANDS__ */
