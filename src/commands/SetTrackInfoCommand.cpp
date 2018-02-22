/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan

******************************************************************//**

\file SetTrackInfoCommand.cpp
\brief Definitions for SetTrackInfoCommand and SetTrackInfoCommandType classes

\class SetTrackInfoCommand
\brief Command that sets track information (currently name only)

*//*******************************************************************/

#include "../Audacity.h"
#include "SetTrackInfoCommand.h"
#include "../Project.h"
#include "../Track.h"
#include "../TrackPanel.h"
#include "../WaveTrack.h"

wxString SetTrackInfoCommandType::BuildName()
{
   return wxT("SetTrackInfo");
}

void SetTrackInfoCommandType::BuildSignature(CommandSignature &signature)
{
   auto trackIndexValidator = make_movable<IntValidator>();
   signature.AddParameter(wxT("TrackIndex"), 0, std::move(trackIndexValidator));
   auto nameValidator = make_movable<DefaultValidator>();
   signature.AddParameter(wxT("Name"), wxT("Unnamed"), std::move(nameValidator));
   auto panValidator = make_movable<DoubleValidator>();
   signature.AddParameter(wxT("Pan"), wxT("1.0"), std::move(panValidator));
   auto gainValidator = make_movable<DoubleValidator>();
   signature.AddParameter(wxT("Gain"), wxT("1.0"), std::move(gainValidator));
   auto selectedValidator = make_movable<BoolValidator>();
   signature.AddParameter(wxT("Selected"), wxT("True"), std::move(selectedValidator));
   auto focusedValidator = make_movable<BoolValidator>();
   signature.AddParameter(wxT("Focused"), wxT("True"), std::move(focusedValidator));
   auto soloValidator = make_movable<BoolValidator>();
   signature.AddParameter(wxT("Solo"), wxT("True"), std::move(soloValidator));
   auto muteValidator = make_movable<BoolValidator>();
   signature.AddParameter(wxT("Mute"), wxT("True"), std::move(muteValidator));
}

CommandHolder SetTrackInfoCommandType::Create(std::unique_ptr<CommandOutputTarget> &&target)
{
   return std::make_shared<SetTrackInfoCommand>(*this, std::move(target));
}

bool SetTrackInfoCommand::Apply(CommandExecutionContext context)
{
   //wxString mode = GetString(wxT("Type"));

   long trackIndex = 0;
   if( HasParam("TrackIndex") )
      trackIndex = GetLong(wxT("TrackIndex"));

   // (Note: track selection ought to be somewhere else)
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

   auto wt = dynamic_cast<WaveTrack *>(t);
   auto pt = dynamic_cast<PlayableTrack *>(t);

   if( HasParam( "Name" ) )
      t->SetName(GetString(wxT("Name")));
   if( wt && HasParam( "Pan" ) )
      wt->SetPan(GetDouble(wxT("Pan")));
   if( wt && HasParam( "Gain" ) )
      wt->SetGain(GetDouble(wxT("Gain")));
   if( HasParam( "Selected" ) )
      t->SetSelected(GetBool(wxT("Selected")));
   if(HasParam("Focused"))
   {
      TrackPanel *panel = context.GetProject()->GetTrackPanel();
      panel->SetFocusedTrack( t );
   }
   if( pt && HasParam( "Solo" ) )
      pt->SetSolo(GetBool(wxT("Solo")));
   if( pt && HasParam( "Mute" ) )
      pt->SetMute(GetBool(wxT("Mute")));

   return true;
}
