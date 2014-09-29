/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2009 Audacity Team.
   File License: wxwidgets

   OpenSaveCommands.h
   Stephen Parry

******************************************************************//**

\class OpenProjectCommand
\brief Command for opening an Audacity project

\class SaveProjectCommand
\brief Command for saving an Audacity project

*//*******************************************************************/

#include "Command.h"
#include "CommandType.h"

// Open

class OpenProjectCommandType : public CommandType
{
public:
   virtual wxString BuildName();
   virtual void BuildSignature(CommandSignature &signature);
   virtual Command *Create(CommandOutputTarget *target);
};

class OpenProjectCommand : public CommandImplementation
{
public:
   OpenProjectCommand(CommandType &type,
                    CommandOutputTarget *target)
      : CommandImplementation(type, target)
   { }

   virtual ~OpenProjectCommand();
   virtual bool Apply(CommandExecutionContext context);
};

// Save

class SaveProjectCommandType : public CommandType
{
public:
   virtual wxString BuildName();
   virtual void BuildSignature(CommandSignature &signature);
   virtual Command *Create(CommandOutputTarget *target);
};

class SaveProjectCommand : public CommandImplementation
{
public:
   SaveProjectCommand(CommandType &type,
                    CommandOutputTarget *target)
      : CommandImplementation(type, target)
   { }

   virtual ~SaveProjectCommand();
   virtual bool Apply(CommandExecutionContext context);
};
