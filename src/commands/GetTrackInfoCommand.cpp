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

#include "../Audacity.h"
#include "GetTrackInfoCommand.h"
#include "../Project.h"
#include "../Track.h"
#include "../TrackPanel.h"
#include "../WaveTrack.h"
#include "../ShuttleGui.h"
#include "CommandContext.h"

const int nTypes =3;
static const wxString kTypes[nTypes] =
{
   XO("Tracks"),
   XO("Clips"),
   XO("Labels")
};


GetTrackInfoCommand::GetTrackInfoCommand()
{
   mInfoType = 0;
}

bool GetTrackInfoCommand::DefineParams( ShuttleParams & S ){ 
   wxArrayString types( nTypes, kTypes );
   S.DefineEnum( mInfoType, wxT("Type"), 0, types );
   
   return true;
}

void GetTrackInfoCommand::PopulateOrExchange(ShuttleGui & S)
{
   wxArrayString types( nTypes, kTypes );
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieChoice( _("Types:"), mInfoType, &types);
   }
   S.EndMultiColumn();
}

bool GetTrackInfoCommand::Apply(const CommandContext & context)
{
   TrackList *projTracks = context.GetProject()->GetTracks();
   TrackListIterator iter(projTracks);
   Track *trk = iter.First();
   wxString str = "[\n";
   while (trk)
   {
      str += "{\n";
      str += wxString::Format("  name:\"%s\",\n", trk->GetName() );
      auto t = dynamic_cast<WaveTrack*>( trk );
      if( t )
      {
         str += wxString::Format("  start:%g,\n", t->GetStartTime() );
         str += wxString::Format("  end:%g,\n", t->GetEndTime() );
         str += wxString::Format("  pan:%g,\n", t->GetPan() );
         str += wxString::Format("  gain:%g,\n", t->GetGain() );
         str += wxString::Format("  selected:%s,\n", t->GetSelected()?"True":"False"  );
         str += wxString::Format("  linked:%s,\n", t->GetLinked()?"True":"False"  );
         str += wxString::Format("  solo:%s,\n", t->GetSolo()?"True":"False"  );
         str += wxString::Format("  mute:%s,\n", t->GetMute()?"True":"False"  );
      }
      TrackPanel *panel = context.GetProject()->GetTrackPanel();
      Track * fTrack = panel->GetFocusedTrack();
      str += wxString::Format("  focused:%s,\n", (trk == fTrack)?"True":"False" );
      str += "},\n";
      trk=iter.Next();
   }
   str += "]";
   // Make it true JSON by removing excess commas.
   str.Replace(",\n}","\n}");
   str.Replace(",\n]","\n]");
   context.Status( str );
   return true;
}
