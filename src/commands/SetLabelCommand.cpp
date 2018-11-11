/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: wxwidgets

   James Crook

******************************************************************//**

\file SetLabelCommand.cpp
\brief Definitions for SetLabelCommand

\class SetLabelCommand
\brief Command that sets label information

*//*******************************************************************/

#include "../Audacity.h"
#include "SetLabelCommand.h"

#include "../Project.h"
#include "../Track.h"
#include "../TrackPanel.h"
#include "../WaveTrack.h"
#include "../LabelTrack.h"
#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "CommandContext.h"

SetLabelCommand::SetLabelCommand()
{
}


bool SetLabelCommand::DefineParams( ShuttleParams & S ){ 
   S.Define(    mLabelIndex,                            wxT("Label"), 0, 0, 100 );
   S.OptionalY( bHasText       ).Define(  mText,        wxT("Text"),       wxT("empty") );
   S.OptionalY( bHasT0         ).Define(  mT0,          wxT("Start"),      0.0, 0.0, 100000.0);
   S.OptionalY( bHasT1         ).Define(  mT1,          wxT("End"),        0.0, 0.0, 100000.0);
   S.OptionalN( bHasSelected   ).Define(  mbSelected,   wxT("Selected"),   false );
   return true;
};

void SetLabelCommand::PopulateOrExchange(ShuttleGui & S)
{
   S.AddSpace(0, 5);

   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.TieNumericTextBox( _("Label Index"), mLabelIndex );
   }
   S.EndMultiColumn();
   S.StartMultiColumn(3, wxALIGN_CENTER);
   {
      S.Optional( bHasText      ).TieTextBox(         _("Text:"),     mText );
      S.Optional( bHasT0        ).TieNumericTextBox(  _("Start:"),    mT0 );
      S.Optional( bHasT1        ).TieNumericTextBox(  _("End:"),      mT1 );
      S.Optional( bHasSelected  ).TieCheckBox(        _("Selected:"), mbSelected );
   }
   S.EndMultiColumn();
}

bool SetLabelCommand::Apply(const CommandContext & context)
{
   // \todo we have similar code for finding the nth Label, Clip, Track etc.
   // this code could be put in subroutines/reduced.

   //wxString mode = GetString(wxT("Type"));
   AudacityProject * p = context.GetProject();
   TrackList *tracks = context.GetProject()->GetTracks();
   LabelStruct * pLabel = NULL;
   int i=0;
   int nn=0;

   LabelTrack *labelTrack {};
   for (auto lt : tracks->Any<LabelTrack>()) {
      if( i > mLabelIndex )
         break;
      labelTrack = lt;
      for (nn = 0;
         (nn< (int)labelTrack->mLabels.size()) && i<=mLabelIndex; 
         nn++) {
         i++;
         pLabel = &labelTrack->mLabels[nn];
      }
   }

   if ( (i< mLabelIndex) || (pLabel == NULL))
   {
      context.Error(wxT("LabelIndex was invalid."));
      return false;
   }
   if( bHasText )
      pLabel->title = mText;
   if( bHasT0 )
      pLabel->selectedRegion.setT0(mT0, false);
   if( bHasT1 )
      pLabel->selectedRegion.setT1(mT1, false);
   if( bHasT0 || bHasT1 )
      pLabel->selectedRegion.ensureOrdering();
   pLabel->updated = true;

   // Only one label can be selected.
   if( bHasSelected ){
      if( mbSelected )
      {
         labelTrack->mSelIndex = nn-1;
         double t0 = pLabel->selectedRegion.t0();
         double t1 = pLabel->selectedRegion.t1();
         p->mViewInfo.selectedRegion.setTimes( t0, t1);
      }
      else if( labelTrack->mSelIndex == (nn-1) )
         labelTrack->mSelIndex = -1;
   }

   labelTrack->SortLabels();
   return true;
}
