/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Marty Goddard

******************************************************************//**

\file SetProjectInfoCommand.cpp
\brief Definitions for SetProjectInfoCommand and SetProjectInfoCommandType classes

\class SetProjectInfoCommand
\brief Command that returns requested project information

*//*******************************************************************/

#include "SetProjectInfoCommand.h"
#include "../Project.h"
#include "../Track.h"
#include "../WaveTrack.h"

// The following parameters have a boolean string, indicated by the kSetOfTracksStr
#define kSetOfTracksStr "TrackSet"

wxString SetProjectInfoCommandType::BuildName()
{
   return wxT("SetProjectInfo");
}

void SetProjectInfoCommandType::BuildSignature(CommandSignature &signature)
{
   OptionValidator *infoTypeValidator = new OptionValidator();
   infoTypeValidator->AddOption(wxT("SelectedTracks"));
   infoTypeValidator->AddOption(wxT("MuteTracks"));
   infoTypeValidator->AddOption(wxT("SoloTracks"));

   signature.AddParameter(wxT("Type"), wxT("Name"), infoTypeValidator);

   BoolArrayValidator *TracksSetValidator = new BoolArrayValidator();
   signature.AddParameter(wxT(kSetOfTracksStr), wxT("x"), TracksSetValidator);
}

Command *SetProjectInfoCommandType::Create(CommandOutputTarget *target)
{
   return new SetProjectInfoCommand(*this, target);
}




// ***********************  Public Methods *******************
bool SetProjectInfoCommand::Apply(CommandExecutionContext context)
{
   wxString mode = GetString(wxT("Type"));
   wxString settingsString = GetString(wxT(kSetOfTracksStr));

   if (mode.IsSameAs(wxT("SelectedTracks")))
      SetAllTracksParam( context.proj->GetTracks(), settingsString,
&SetProjectInfoCommand::setSelected);

   else if (mode.IsSameAs(wxT("SoloTracks")))
      SetAllTracksParam( context.proj->GetTracks(), settingsString, &SetProjectInfoCommand::setSolo);

   else if (mode.IsSameAs(wxT("MuteTracks")))
      SetAllTracksParam( context.proj->GetTracks(), settingsString, &SetProjectInfoCommand::setMute);
   else
   {
      Error(wxT("Invalid info type!"));
      return false;
   }
   return true;
}



// ***********************  Private Methods *******************
void SetProjectInfoCommand::SetAllTracksParam(TrackList *projTracks, wxString boolValueStr, Setter functPtrToSetter)
{
   unsigned int i=0;
   TrackListIterator iter(projTracks);
   Track *t = iter.First();
   while (t && i<boolValueStr.Len())
   {
      if(boolValueStr[i] == '1')
         (this->*functPtrToSetter)(t, true);
      if(boolValueStr[i] == '0')
         (this->*functPtrToSetter)(t, false);
      i++;
      t = iter.Next();
   }
}

void SetProjectInfoCommand::setSelected(Track * trk, bool param) const
{
   trk->SetSelected(param);
}

void SetProjectInfoCommand::setSolo(Track * trk, bool param) const
{
   if(trk->GetKind() == Track::Wave)
      trk->SetSolo(param);
}

void SetProjectInfoCommand::setMute(Track * trk, bool param) const
{
   if(trk->GetKind() == Track::Wave)
      trk->SetMute(param);
}
