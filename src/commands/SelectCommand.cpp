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
#include "../TrackPanel.h"
#include "../ShuttleGui.h"
#include "CommandContext.h"


// Relative to project and relative to selection cover MOST options, since you can already
// set a selection to a clip.
const int nRelativeTos =6;
static const wxString kRelativeTo[nRelativeTos] =
{
   XO("Project Start"),
   XO("Project"),
   XO("Project End"),
   XO("Selection Start"),
   XO("Selection"),
   XO("Selection End")
};

bool SelectTimeCommand::DefineParams( ShuttleParams & S ){
   wxArrayString relativeSpec( nRelativeTos, kRelativeTo );
   // Allow selection down to -ve 100seconds.
   // Typically used to expand/contract selections by a small amount.
   S.OptionalY( bHasT0           ).Define( mT0, wxT("Start"), 0.0, -100.0, (double)FLT_MAX);
   S.OptionalY( bHasT1           ).Define( mT1, wxT("End"), 0.0, -100.0, (double)FLT_MAX);
   S.OptionalN( bHasRelativeSpec ).DefineEnum( mRelativeTo,   wxT("RelativeTo"), 0, relativeSpec );
   return true;
}

void SelectTimeCommand::PopulateOrExchange(ShuttleGui & S)
{
   auto relativeSpec = LocalizedStrings( kRelativeTo, nRelativeTos );
   S.AddSpace(0, 5);

   S.StartMultiColumn(3, wxEXPAND);
   {
      S.SetStretchyCol( 2 );
      S.Optional( bHasT0 ).TieTextBox(_("Start Time:"), mT0);
      S.Optional( bHasT1 ).TieTextBox(_("End Time:"),   mT1);
      // Chooses what time is relative to.
      S.Optional( bHasRelativeSpec ).TieChoice( 
         _("Relative To:"), mRelativeTo, &relativeSpec);
   }
   S.EndMultiColumn();
}

bool SelectTimeCommand::Apply(const CommandContext & context){
   // Many commands need focus on track panel.
   // No harm in setting it with a scripted select.
   context.GetProject()->GetTrackPanel()->SetFocus();
   if( !bHasT0 && !bHasT1 )
      return true;

   AudacityProject * p = context.GetProject();
   double end = p->GetTracks()->GetEndTime();
   double t0;
   double t1;

   switch( bHasRelativeSpec ? mRelativeTo : 0 ){
   default:
   case 0: //project start
      t0 = mT0;
      t1 = mT1;
      break;
   case 1: //project
      t0 = mT0;
      t1 = end + mT1;
      break;
   case 2: //project end;
      t0 = end - mT0;
      t1 = end - mT1;
      break;
   case 3: //selection start
      t0 = mT0 + p->GetSel0();
      t1 = mT1 + p->GetSel0();
      break;
   case 4: //selection
      t0 = mT0 + p->GetSel0();
      t1 = mT1 + p->GetSel1();
      break;
   case 5: //selection end
      t0 =  p->GetSel1() - mT0;
      t1 =  p->GetSel1() - mT1;
      break;
   }

   p->mViewInfo.selectedRegion.setTimes( t0, t1);
   return true;
}

bool SelectFrequenciesCommand::DefineParams( ShuttleParams & S ){
   S.OptionalN( bHasTop ).Define(    mTop,    wxT("High"), 0.0, 0.0, (double)FLT_MAX);
   S.OptionalN( bHasBottom ).Define( mBottom, wxT("Low"),  0.0, 0.0, (double)FLT_MAX);
   return true;
}

void SelectFrequenciesCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(3, wxEXPAND);
   {
      S.SetStretchyCol( 2 );
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
   /* i18n-hint verb, imperative */
   XO("Set"),
   XO("Add"),
   XO("Remove")
};

bool SelectTracksCommand::DefineParams( ShuttleParams & S ){
   wxArrayString modes( nModes, kModes );
   S.OptionalN( bHasFirstTrack).Define( mFirstTrack, wxT("First"), 0, 0, 100);
   S.OptionalN( bHasLastTrack ).Define( mLastTrack,  wxT("Last"),  0, 0, 100);
   S.OptionalY( bHasMode      ).DefineEnum( mMode,   wxT("Mode"), 0, modes );
   
   return true;
}

void SelectTracksCommand::PopulateOrExchange(ShuttleGui & S)
{
   auto modes = LocalizedStrings( kModes, nModes );
   S.AddSpace(0, 5);

   S.StartMultiColumn(3, wxEXPAND);
   {
      S.SetStretchyCol( 2 );
      S.Optional( bHasFirstTrack).TieTextBox(_("First Track:"),mFirstTrack);
      S.Optional( bHasLastTrack).TieTextBox(_("Last Track:"),mLastTrack);
   }
   S.EndMultiColumn();
   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      // Always used, so no check box.
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
      }
      else if( mMode == 1 && sel ){ // Add
         t->SetSelected(sel);
      }
      else if( mMode == 2 && sel ){ // Remove
         t->SetSelected(!sel);
      }
      // Do second channel in stereo track too.
      if( !t->GetLinked() )
         ++index;
      t = iter.Next();
   }
   return true;
}

