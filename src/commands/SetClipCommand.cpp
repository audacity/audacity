/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: wxwidgets

   James Crook

******************************************************************//**

\file SetClipCommand.cpp
\brief Definitions for SetClipCommand

\class SetClipCommand
\brief Command that sets clip information

*//*******************************************************************/

#include "../Audacity.h"
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
   S.OptionalY( bHasTrackIndex     ).Define(     mTrackIndex,     wxT("Track"),      0, 0, 100 );
   S.OptionalN( bHasChannelIndex   ).Define(     mChannelIndex,   wxT("Channel"),    0, 0, 100 );
   S.OptionalY( bHasContainsTime   ).Define(     mContainsTime,   wxT("At"),         0.0, 0.0, 100000.0 );
   S.OptionalN( bHasColour         ).DefineEnum( mColour,         wxT("Color"),      kColour0, colours );
   // Allowing a negative start time is not a mistake.
   // It will be used in demonstrating time before zero.
   S.OptionalN( bHasT0             ).Define(     mT0,             wxT("Start"),      0.0, -5.0, 1000000.0);
   return true;
};

void SetClipCommand::PopulateOrExchange(ShuttleGui & S)
{
   wxArrayString colours( nColours, kColourStrings );

   S.AddSpace(0, 5);

   S.StartMultiColumn(3, wxALIGN_CENTER);
   {
      S.Optional( bHasTrackIndex  ).TieNumericTextBox(  _("Track Index:"),   mTrackIndex );
      S.Optional( bHasChannelIndex).TieNumericTextBox(  _("Channel Index:"), mChannelIndex );
      S.Optional( bHasContainsTime).TieNumericTextBox(  _("At:"),            mContainsTime );
      S.Optional( bHasColour      ).TieChoice(          _("Colour:"),        mColour, &colours );
      S.Optional( bHasT0          ).TieNumericTextBox(  _("Start:"),         mT0 );
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
   int j=0;

   bool bIsSecondChannel = false;
   while (t )
   {
      bool bThisTrack = 
         (bHasTrackIndex && (i==mTrackIndex)) ||
         (bHasChannelIndex && (j==mChannelIndex ) ) ||
         (!bHasTrackIndex && !bHasChannelIndex) ;

      if( bThisTrack && (t->GetKind() == Track::Wave)) {
         bool bFound = false;
         WaveTrack *waveTrack = static_cast<WaveTrack*>(t);
         WaveClipPointers ptrs( waveTrack->SortedClipArray());
         for(auto it = ptrs.begin(); (it != ptrs.end()); it++ ){
            pClip = *it;
            bFound = 
               !bHasContainsTime || (
                  ( pClip->GetStartTime() <= mContainsTime ) &&
                  ( pClip->GetEndTime() >= mContainsTime )
               );
            if( bFound )
            {
               // Inside this IF is where we actually apply the command

               if( bHasColour )
                  pClip->SetColourIndex(mColour);
               // No validation of overlap yet.  We assume the user is sensible!
               if( bHasT0 )
                  pClip->SetOffset(mT0);
               // \todo Use SetClip to move a clip between tracks too.

            }
         }
      }
      bIsSecondChannel = t->GetLinked();
      if( !bIsSecondChannel )
         ++i;
      j++;
      t = iter.Next();
   }
   return true;
}
