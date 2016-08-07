/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan
   Marty Goddard
******************************************************************//**

\file GetTrackInfoCommand.cpp
\brief Definitions for GetTrackInfoCommand and GetTrackInfoCommandType classes

\class GetTrackInfoCommand
\brief Command that returns requested track information

*//*******************************************************************/

#include "GetTrackInfoCommand.h"
#include "../TrackPanel.h"
#include "../Project.h"
#include "../WaveTrack.h"

wxString GetTrackInfoCommandType::BuildName()
{
   return wxT("GetTrackInfo");
}

void GetTrackInfoCommandType::BuildSignature(CommandSignature &signature)
{
   auto trackIndexValidator = make_movable<IntValidator>();
   signature.AddParameter(wxT("TrackIndex"), 0, std::move(trackIndexValidator));

   auto infoTypeValidator = make_movable<OptionValidator>();
   infoTypeValidator->AddOption(wxT("Name"));
   infoTypeValidator->AddOption(wxT("StartTime"));
   infoTypeValidator->AddOption(wxT("EndTime"));
   infoTypeValidator->AddOption(wxT("Pan"));
   infoTypeValidator->AddOption(wxT("Gain"));
   infoTypeValidator->AddOption(wxT("Selected"));
   infoTypeValidator->AddOption(wxT("Linked"));
   infoTypeValidator->AddOption(wxT("Solo"));
   infoTypeValidator->AddOption(wxT("Mute"));
   infoTypeValidator->AddOption(wxT("Focused"));

   signature.AddParameter(wxT("Type"), wxT("Name"), std::move(infoTypeValidator));
}

CommandHolder GetTrackInfoCommandType::Create(std::unique_ptr<CommandOutputTarget> &&target)
{
   return std::make_shared<GetTrackInfoCommand>(*this, std::move(target));
}



//******************* Private Member Functions ********************************
void GetTrackInfoCommand::SendBooleanStatus(bool boolValue)
{
   if(boolValue)
      Status(wxT("1"));
   else
      Status(wxT("0"));
}




// ===================== Public Member Functions =================================

bool GetTrackInfoCommand::Apply(CommandExecutionContext context)
{
   wxString mode = GetString(wxT("Type"));

   long trackIndex = GetLong(wxT("TrackIndex"));

   // Get the track indicated by the TrackIndex parameter
   // (Note: this ought to be somewhere else)
   long i = 0;
   TrackListIterator iter(context.GetProject()->GetTracks());
   Track *t = iter.First();
   while (t && i != trackIndex)
   {
      t = iter.Next();
      ++i;
   }
   if (i != trackIndex || !t)
   {
      Error(wxT("TrackIndex was invalid."));
      return false;
   }

   // Now get the particular desired item about the track of interest
   if (mode.IsSameAs(wxT("Name")))
   {
      Status(t->GetName());
   }
   else if (mode.IsSameAs(wxT("StartTime")))
   {
      Status(wxString::Format(wxT("%f"), t->GetStartTime()));
   }
   else if (mode.IsSameAs(wxT("EndTime")))
   {
      Status(wxString::Format(wxT("%f"), t->GetEndTime()));
   }
   else if (mode.IsSameAs(wxT("Pan")))
   {
     if(t->GetKind() == Track::Wave)
       Status(wxString::Format(wxT("%f"), static_cast<WaveTrack*>(t)->GetPan()));
   }
   else if (mode.IsSameAs(wxT("Gain")))
   {
      if(t->GetKind() == Track::Wave)
         Status(wxString::Format(wxT("%f"), static_cast<WaveTrack*>(t)->GetGain()));
   }
   else if (mode.IsSameAs(wxT("Focused")))
   {
      TrackPanel *panel = context.GetProject()->GetTrackPanel();
      SendBooleanStatus(panel->GetFocusedTrack() == t);
   }
   else if (mode.IsSameAs(wxT("Selected")))
   {
      SendBooleanStatus(t->GetSelected());
   }
   else if (mode.IsSameAs(wxT("Linked")))
   {
      SendBooleanStatus(t->GetLinked());
   }
   else if (mode.IsSameAs(wxT("Solo")))
   {
      if (t->GetKind() == Track::Wave)
         SendBooleanStatus(t->GetSolo());
      else
         SendBooleanStatus(false);
   }
   else if (mode.IsSameAs(wxT("Mute")))
   {
      if (t->GetKind() == Track::Wave)
         SendBooleanStatus(t->GetMute());
      else
         SendBooleanStatus(false);
   }
   else
   {
      Error(wxT("Invalid info type!"));
      return false;
   }
   return true;
}
