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

class SetTrackInfoCommandType final : public CommandType
{
public:
   wxString BuildName() override;
   void BuildSignature(CommandSignature &signature) override;
   CommandHolder Create(std::unique_ptr<CommandOutputTarget> &&target) override;
};

class SetTrackInfoCommand final : public CommandImplementation
{
public:
   SetTrackInfoCommand(CommandType &type, std::unique_ptr<CommandOutputTarget> &&target)
      : CommandImplementation(type, std::move(target))
   { }
   virtual ~SetTrackInfoCommand()
   { }

   bool Apply(CommandExecutionContext context) override;
};

#endif /* End of include guard: __SETTRACKINFOCOMMAND__ */
