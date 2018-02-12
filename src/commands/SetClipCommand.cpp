/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan
   James Crook

******************************************************************//**

\file SetClipCommand.cpp
\brief Definitions for SetClipCommand

\class SetClipCommand
\brief Command that sets clip information

*//*******************************************************************/

#include "SetClipCommand.h"
#include "../Project.h"
#include "../Track.h"
#include "../TrackPanel.h"
#include "../WaveTrack.h"
#include "../ShuttleGui.h"
#include "CommandContext.h"

SetClipCommand::SetClipCommand()
{
}

enum kColours
{
   kColour0,
   kColour1,
   kColour2,
   kColour3,
   nColours
};

static const wxString kColourStrings[nColours] =
{
   XO("Color0"),
   XO("Color1"),
   XO("Color2"),
   XO("Color3"),
};


bool SetClipCommand::DefineParams( ShuttleParams & S ){ 
   wxArrayString colours( nColours, kColourStrings );
   S.Define(   mClipIndex,                                 wxT("Clip"), 0, 0, 100 );
   S.Optional( bHasColour      ).DefineEnum( mColour,      wxT("Color"),      kColour0, colours );
   // Allowing a negative start time is not a mistake.
   // It will be used in demonstrating time before zero.
   S.Optional( bHasT0          ).Define(     mT0,          wxT("Start"),     0.0, -5.0, 1000000.0);
   return true;
};

void SetClipCommand::PopulateOrExchange(ShuttleGui & S)
{
   wxArrayString colours( nColours, kColourStrings );

   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieNumericTextBox( _("Clip Index"), mClipIndex );
   }
   S.EndMultiColumn();
   S.StartMultiColumn(3, wxALIGN_CENTER);
   {
      S.Optional( bHasColour    ).TieChoice(          _("Colour:"),   mColour, &colours );
      S.Optional( bHasT0        ).TieNumericTextBox(  _("Start:"),    mT0 );
   }
   S.EndMultiColumn();
}

bool SetClipCommand::Apply(const CommandContext & context)
{
   TrackList *tracks = context.GetProject()->GetTracks();
   TrackListIterator iter(tracks);
   Track *t = iter.First();
   WaveClip * pClip = NULL;
   int i=0;

   while (t && i <= mClipIndex) {
      if (t->GetKind() == Track::Wave) {
         WaveTrack *waveTrack = static_cast<WaveTrack*>(t);
         WaveClipPointers ptrs( waveTrack->SortedClipArray());
         for(auto it = ptrs.begin(); (it != ptrs.end()) && (i<=mClipIndex); it++,i++ ){
            pClip = *it;
         }
      }
      t = iter.Next();
   }

   if (i <= mClipIndex || !pClip)
   {
      context.Error(wxT("ClipIndex was invalid."));
      return false;
   }

   if( bHasColour )
      pClip->SetColourIndex(mColour);

   // No validation of overlap yet.  We assume the user is sensible!
   if( bHasT0 )
      pClip->SetOffset(mT0);

   // \todo Use SetClip to move a clip between tracks too.

   return true;
}
