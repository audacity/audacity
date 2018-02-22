/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Marty Goddard

******************************************************************//**

\file GetProjectInfoCommand.h
\brief Declarations of GetProjectInfoCommand and GetProjectInfoCommandType classes

*//*******************************************************************/

#ifndef __GETPROJECTINFOCOMMAND__
#define __GETPROJECTINFOCOMMAND__

#include "Command.h"
#include "CommandType.h"

class GetProjectInfoCommandType final : public CommandType
{
public:
   wxString BuildName() override;
   void BuildSignature(CommandSignature &signature) override;
   CommandHolder Create(std::unique_ptr<CommandOutputTarget> &&target) override;
};


class GetProjectInfoCommand final : public CommandImplementation
{
public:
   GetProjectInfoCommand(CommandType &type, std::unique_ptr<CommandOutputTarget> &&target)
      : CommandImplementation(type, std::move(target))
   { }
   virtual ~GetProjectInfoCommand()
   { }

   bool Apply(CommandExecutionContext context) override;

private:
   int SendNumberOfTracks(CommandExecutionContext context);
   int SendFocusedTrackIndex(CommandExecutionContext context);

// Function pointer to get a particular (Boolean only) Track parameter
   typedef bool (GetProjectInfoCommand::*Getter)(const Track *track) const;

// Uses the Function pointer to set a particular parameter within a loop of otherwise duplicate code
   void SendTracksInfo(TrackList *projTracks, Getter);

// Functions pointed to for getting track parameters
   bool testSelected(const Track * track) const;
   bool testLinked(const Track * track) const;
   bool testSolo(const Track * track) const;
   bool testMute(const Track * track) const;
};








#endif /* End of include guard: __GETPROJECTINFOCOMMAND__ */
