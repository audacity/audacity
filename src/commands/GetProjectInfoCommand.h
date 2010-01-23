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
#include "../Track.h"

class GetProjectInfoCommandType : public CommandType
{
public:
   virtual wxString BuildName();
   virtual void BuildSignature(CommandSignature &signature);
   virtual Command *Create(CommandOutputTarget *target);
};


class GetProjectInfoCommand : public CommandImplementation
{
public:
   GetProjectInfoCommand(CommandType &type, CommandOutputTarget *target)
      : CommandImplementation(type, target)
   { }
   virtual ~GetProjectInfoCommand()
   { }

   virtual bool Apply(CommandExecutionContext context);

private:
   int SendNumberOfTracks(CommandExecutionContext context);
   int SendFocusedTrackIndex(CommandExecutionContext context);

// Function pointer to get a particular (Boolean only) Track parameter
   typedef bool (GetProjectInfoCommand::*Getter)(Track *track) const;

// Uses the Function pointer to set a particular parameter within a loop of otherwise duplicate code
   void SendTracksInfo(TrackList *projTracks, Getter);

// Functions pointed to for getting track parameters
   bool testSelected(Track * track) const {return track->GetSelected();}
   bool testLinked( Track * track) const {return track->GetLinked();}
   bool testSolo( Track * track) const {return track->GetSolo();}
   bool testMute( Track * track) const {return track->GetMute();}
};








#endif /* End of include guard: __GETPROJECTINFOCOMMAND__ */

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
