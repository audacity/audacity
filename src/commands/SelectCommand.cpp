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

\class SelectFrequenciesCommand
\brief Command for changing the frequency selection

\class SelectTracksCommand
\brief Command for changing the selection of tracks

\class SelectCommand
\brief Command for changing time, frequency and track selection. This
class is a little baroque, as it uses the SelectTimeCommand, 
SelectFrequenciesCommand and SelectTracksCommand, when it could just
explicitly code all three.

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
   S.Optional( bHasT0 ).Define( mT0, wxT("Start"), 0.0, 0.0, (double)FLT_MAX);
   S.Optional( bHasT1 ).Define( mT1, wxT("End"), 0.0, 0.0, (double)FLT_MAX);
   S.Define( mFromEnd, wxT("FromEnd"),   false );
   return true;
}

void SelectTimeCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(3, wxALIGN_CENTER);
   {
      S.Optional( bHasT0 ).TieTextBox(_("Start Time:"), mT0);
      S.Optional( bHasT1 ).TieTextBox(_("End Time:"),   mT1);
   }
   S.EndMultiColumn();
   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieCheckBox(_("From End:"), mFromEnd );
   }
   S.EndMultiColumn();
}

bool SelectTimeCommand::Apply(const CommandContext & context){
   if( !bHasT0 && !bHasT1 )
      return true;

   if( mFromEnd ){
      double TEnd = context.GetProject()->GetTracks()->GetEndTime();
      context.GetProject()->mViewInfo.selectedRegion.setTimes(TEnd - mT0, TEnd - mT1);
      return true;
   }
   context.GetProject()->mViewInfo.selectedRegion.setTimes(mT0, mT1);
   return true;
}

bool SelectFrequenciesCommand::DefineParams( ShuttleParams & S ){
   S.Optional( bHasTop ).Define(    mTop,    wxT("High"), 0.0, 0.0, (double)FLT_MAX);
   S.Optional( bHasBottom ).Define( mBottom, wxT("Low"),  0.0, 0.0, (double)FLT_MAX);
   return true;
}

void SelectFrequenciesCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(3, wxALIGN_CENTER);
   {
      S.Optional( bHasTop    ).TieTextBox(_("High:"), mTop);
      S.Optional( bHasBottom ).TieTextBox(_("Low:"),  mBottom);
   }
   S.EndMultiColumn();
}

bool SelectFrequenciesCommand::Apply(const CommandContext & context){
   if( !bHasBottom && !bHasTop )
      return true;

   context.GetProject()->SSBL_ModifySpectralSelection(
      mBottom, mTop, false);// false for not done.
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
   S.Optional( bHasFirstTrack).Define( mFirstTrack, wxT("First"), 0, 0, 100);
   S.Optional( bHasLastTrack ).Define( mLastTrack,  wxT("Last"),  0, 0, 100);
   S.DefineEnum( mMode, wxT("Mode"), 0, modes );
   
   return true;
}

void SelectTracksCommand::PopulateOrExchange(ShuttleGui & S)
{
   wxArrayString modes( nModes, kModes );
   S.AddSpace(0, 5);

   S.StartMultiColumn(3, wxALIGN_CENTER);
   {
      S.Optional( bHasFirstTrack).TieTextBox(_("First Track:"),mFirstTrack);
      S.Optional( bHasLastTrack).TieTextBox(_("Last Track:"),mLastTrack);
   }
   S.EndMultiColumn();
   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieChoice( _("Mode:"), mMode, &modes);
   }
   S.EndMultiColumn();
}

bool SelectTracksCommand::Apply(const CommandContext &context)
{
   if( !bHasFirstTrack && !bHasLastTrack )
      return true;

   int index = 0;
   TrackList *tracks = context.GetProject()->GetTracks();
   int last = wxMax( mFirstTrack, mLastTrack );

   TrackListIterator iter(tracks);
   Track *t = iter.First();
   while (t) {
      bool sel = mFirstTrack <= index && index <= last;
      if( mMode == 0 ){ // Set
         t->SetSelected(sel);
//       if (sel)
//          context.Status(wxT("Selected track '") + t->GetName() + wxT("'"));
      }
      else if( mMode == 1 && sel ){ // Add
         t->SetSelected(sel);
//       context.Status(wxT("Added track '") + t->GetName() + wxT("'"));
      }
      else if( mMode == 2 && sel ){ // Remove
         t->SetSelected(!sel);
//       context.Status(wxT("Removed track '") + t->GetName() + wxT("'"));
      }
      t = iter.Next();
      ++index;
   }
   return true;
}

