/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan
   Marty Goddard
******************************************************************//**

\file GetTrackInfoCommand.h
\brief Declarations of GetTrackInfoCommand and GetTrackInfoCommandType classes

*//*******************************************************************/

#ifndef __GETTRACKINFOCOMMAND__
#define __GETTRACKINFOCOMMAND__

#include "Command.h"
#include "CommandType.h"

class GetTrackInfoCommandType : public CommandType
{
public:
   virtual wxString BuildName();
   virtual void BuildSignature(CommandSignature &signature);
   virtual Command *Create(CommandOutputTarget *target);
};

class GetTrackInfoCommand : public CommandImplementation
{
public:
   GetTrackInfoCommand(CommandType &type, CommandOutputTarget *target)
      : CommandImplementation(type, target)
   { }
   virtual ~GetTrackInfoCommand()
   { }

   virtual bool Apply(CommandExecutionContext context);

private:
   void SendBooleanStatus(bool BooleanValue);
};

#endif /* End of include guard: __GETTRACKINFOCOMMAND__ */
