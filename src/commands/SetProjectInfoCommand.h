/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Marty Goddard

******************************************************************//**

\file SetProjectInfoCommand.h
\brief Declarations of SetProjectInfoCommand and SetProjectInfoCommandType classes

*//*******************************************************************/

#ifndef __SETPROJECTINFOCOMMAND__
#define __SETPROJECTINFOCOMMAND__

#include "Command.h"
#include "CommandType.h"

// Forward decls
class Track;
class TrackList;

class SetProjectInfoCommandType final : public CommandType
{
public:
   wxString BuildName() override;
   void BuildSignature(CommandSignature &signature) override;
   CommandHolder Create(std::unique_ptr<CommandOutputTarget> &&target) override;
};


class SetProjectInfoCommand final : public CommandImplementation
{
public:
   SetProjectInfoCommand(CommandType &type, std::unique_ptr<CommandOutputTarget> &&target)
      : CommandImplementation(type, std::move(target))
   { }
   virtual ~SetProjectInfoCommand()
   { }

   bool Apply(CommandExecutionContext context) override;

private:
// Function pointer to set a particular Track parameter
   typedef void (SetProjectInfoCommand::*Setter)(Track *trk, bool setting) const;

// Uses the Function pointer to set a particular parameter within a loop of otherwise duplicate code
   void SetAllTracksParam(TrackList *projTracks, const wxString &boolValueStr, Setter functPtrToSetter);

// Function pointer to accessing a particular parameter within a loop of otherwise duplicate code
   void setSelected(Track *trk, bool setting) const;
   void setSolo(Track *trk, bool setting) const;
   void setMute(Track *trk, bool setting) const;
};








#endif /* End of include guard: __SETPROJECTINFOCOMMAND__ */
