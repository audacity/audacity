/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Marty Goddard

******************************************************************//**

\file GetProjectInfoCommand.cpp
\brief Definitions for GetProjectInfoCommand and GetProjectInfoCommandType classes

\class GetProjectInfoCommand
\brief Command that returns requested project information

*//*******************************************************************/

#include "GetProjectInfoCommand.h"
#include "../TrackPanel.h"
#include "../Project.h"
#include "../Track.h"

wxString GetProjectInfoCommandType::BuildName()
{
   return wxT("GetProjectInfo");
}

void GetProjectInfoCommandType::BuildSignature(CommandSignature &signature)
{
   auto infoTypeValidator = make_movable<OptionValidator>();
   infoTypeValidator->AddOption(wxT("Name"));
   infoTypeValidator->AddOption(wxT("NumberOfTracks"));
   infoTypeValidator->AddOption(wxT("SelectedTracks"));
   infoTypeValidator->AddOption(wxT("MuteTracks"));
   infoTypeValidator->AddOption(wxT("SoloTracks"));
   infoTypeValidator->AddOption(wxT("FocusedTrackID")); // returns the Track ID number of the track in focus

   signature.AddParameter(wxT("Type"), wxT("Name"), std::move(infoTypeValidator));
}

CommandHolder GetProjectInfoCommandType::Create(std::unique_ptr<CommandOutputTarget> &&target)
{
   return std::make_shared<GetProjectInfoCommand>(*this, std::move(target));
}


// ***********************  Public Methods *******************
bool GetProjectInfoCommand::Apply(CommandExecutionContext context)
{
   wxString mode = GetString(wxT("Type"));
   TrackList *projTracks = context.GetProject()->GetTracks();

   if (mode.IsSameAs(wxT("Name")))
   {
      Status(context.GetProject()->GetFileName());
   }
   else if (mode.IsSameAs(wxT("FocusedTrackID")))
   {
      SendFocusedTrackIndex(context);
   }
   else if (mode.IsSameAs(wxT("NumberOfTracks")))
   {
      SendNumberOfTracks(context);
   }
   else if (mode.IsSameAs(wxT("SelectedTracks")))
   {
      SendTracksInfo(projTracks, &GetProjectInfoCommand::testSelected);
   }
   else if (mode.IsSameAs(wxT("LinkedTracks")))
   {
      SendTracksInfo(projTracks, &GetProjectInfoCommand::testLinked);
   }
   else if (mode.IsSameAs(wxT("SoloTracks")))
   {
      SendTracksInfo(projTracks, &GetProjectInfoCommand::testSolo);
   }
   else if (mode.IsSameAs(wxT("MuteTracks")))
   {
      SendTracksInfo(projTracks, &GetProjectInfoCommand::testMute);
   }
   else
   {
      Error(wxT("Invalid info type!"));
      return false;
   }
   return true;
}

//********************* Private Methods *******************************

int GetProjectInfoCommand::SendNumberOfTracks(CommandExecutionContext context)
{
   int returnVal=0;

   TrackListIterator iter(context.GetProject()->GetTracks());
   Track *t = iter.First();
   while (t)
   {
      returnVal++;
      t = iter.Next();
   }
   wxString trackNumStr;
   trackNumStr << returnVal;  // convert to a string to send over named pipe
   Status(trackNumStr);
   return returnVal;
}

int GetProjectInfoCommand::SendFocusedTrackIndex(CommandExecutionContext context)
{
   int returnVal=0;
   int focusTrackIndex=0;
   TrackPanel *panel = context.GetProject()->GetTrackPanel();
   Track* focusedTrack = panel->GetFocusedTrack();

   TrackListIterator iter(context.GetProject()->GetTracks());
   Track *t = iter.First();
   while (t)
   {
      if(t == focusedTrack)   // when we've found the focused track, we know the trackIndex
      {
        returnVal = focusTrackIndex;
        break;
      }
      focusTrackIndex++;
      t = iter.Next();
   }
   wxString trackIndexStr;
   trackIndexStr << returnVal;  // convert to a string to send over named pipe
   Status(trackIndexStr);
   return returnVal;
}


void GetProjectInfoCommand::SendTracksInfo(TrackList *projTracks,
                                           Getter functPtrToGetter)
{
   wxString boolValueStr;
   TrackListIterator iter(projTracks);
   Track *trk = iter.First();
   while (trk)
   {
      if( (this->*functPtrToGetter)(trk))    // Function Pointer to the desired parameter to Test
         boolValueStr.Append(wxT("1"),1);
      else
         boolValueStr.Append(wxT("0"),1);
      trk = iter.Next();
   }
   Status(boolValueStr);
}

bool GetProjectInfoCommand::testSelected(const Track * track) const
{
   return track->GetSelected();
}

bool GetProjectInfoCommand::testLinked(const Track * track) const
{
   return track->GetLinked();
}

bool GetProjectInfoCommand::testSolo(const Track * track) const
{
   return track->GetSolo();
}

bool GetProjectInfoCommand::testMute(const Track * track) const
{
   return track->GetMute();
}
