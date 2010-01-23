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

class SetProjectInfoCommandType : public CommandType
{
public:
   virtual wxString BuildName();
   virtual void BuildSignature(CommandSignature &signature);
   virtual Command *Create(CommandOutputTarget *target);
};


class SetProjectInfoCommand : public CommandImplementation
{
public:
   SetProjectInfoCommand(CommandType &type, CommandOutputTarget *target)
      : CommandImplementation(type, target)
   { }
   virtual ~SetProjectInfoCommand()
   { }

   virtual bool Apply(CommandExecutionContext context);

private:
// Function pointer to set a particular Track parameter
   typedef void (SetProjectInfoCommand::*Setter)(Track *trk, bool setting) const;

// Uses the Function pointer to set a particular parameter within a loop of otherwise duplicate code
   void SetAllTracksParam(TrackList *projTracks, wxString boolValueStr, Setter functPtrToSetter);

// Function pointer to accessing a particular parameter within a loop of otherwise duplicate code
   void setSelected(Track *trk, bool setting) const;
   void setSolo(Track *trk, bool setting) const;
   void setMute(Track *trk, bool setting) const;
};








#endif /* End of include guard: __SETPROJECTINFOCOMMAND__ */

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
