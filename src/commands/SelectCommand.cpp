/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan
   James Crook

******************************************************************//**

\file SelectCommand.cpp
\brief Definitions for SelectCommand classes

\class SelectTimeCommand
\brief Command for changing the time selection

\class SelectTracksCommand
\brief Command for changing the selection of tracks

\class SelectCommand
\brief Command for changing both time and track selection.

*//*******************************************************************/

#include "../Audacity.h"
#include <wx/string.h>
#include <float.h>

#include "SelectCommand.h"
#include "../Project.h"
#include "../Track.h"
#include "../ShuttleGui.h"
#include "CommandContext.h"

bool SelectTimeCommand::DefineParams( ShuttleParams & S ){
   S.Define( mT0,      wxT("StartTime"), 0.0, 0.0, (double)FLT_MAX);
   S.Define( mT1,      wxT("EndTime"),   0.0, 0.0, (double)FLT_MAX);
   S.Define( mFromEnd, wxT("FromEnd"),   false );
   return true;
}

void SelectTimeCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieTextBox(_("Start Time:"),mT0);
      S.TieTextBox(_("End Time:"),mT1);
      S.TieCheckBox(_("From End:"), mFromEnd );
   }
   S.EndMultiColumn();
}

bool SelectTimeCommand::Apply(const CommandContext & context){
   context.GetProject()->mViewInfo.selectedRegion.setTimes(mT0, mT1);
   return true;
}

const int nModes =3;
static const wxString kModes[nModes] =
{
   XO("Set"),
   XO("Add"),
   XO("Remove")
};


bool SelectTracksCommand::DefineParams( ShuttleParams & S ){
   wxArrayString modes( nModes, kModes );
   S.Define( mFirstTrack, wxT("FirstTrack"), 0, 0, 100);
   S.Define( mLastTrack,  wxT("LastTrack"),  0, 0, 100);
   S.DefineEnum( mMode, wxT("Mode"), 0, modes );
   
   return true;
}

void SelectTracksCommand::PopulateOrExchange(ShuttleGui & S)
{
   wxArrayString modes( nModes, kModes );
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieTextBox(_("First Track:"),mFirstTrack);
      S.TieTextBox(_("Last Track:"),mLastTrack);
      S.TieChoice( _("Mode:"), mMode, &modes);
   }
   S.EndMultiColumn();
}

bool SelectTracksCommand::Apply(const CommandContext &context)
{
   int index = 0;
   TrackList *tracks = context.GetProject()->GetTracks();
   int last = wxMax( mFirstTrack, mLastTrack );

   TrackListIterator iter(tracks);
   Track *t = iter.First();
   while (t) {
      bool sel = mFirstTrack <= index && index <= last;
      if( mMode == 0 ){ // Set
         t->SetSelected(sel);
         if (sel)
            context.Status(wxT("Selected track '") + t->GetName() + wxT("'"));
      }
      else if( mMode == 1 && sel ){ // Add
         t->SetSelected(sel);
         context.Status(wxT("Added track '") + t->GetName() + wxT("'"));
      }
      else if( mMode == 2 && sel ){ // Remove
         t->SetSelected(!sel);
         context.Status(wxT("Removed track '") + t->GetName() + wxT("'"));
      }
      t = iter.Next();
      ++index;
   }
   return true;
}
