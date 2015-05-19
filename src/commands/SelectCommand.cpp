/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2009 Audacity Team
   License: wxwidgets

   Dan Horgan

******************************************************************//**

\file SelectCommand.cpp
\brief Definitions for SelectCommand and SelectCommandType classes

\class SelectCommand
\brief Command for changing the selection

*//*******************************************************************/

#include "SelectCommand.h"
#include <wx/string.h>
#include "../Project.h"

wxString SelectCommandType::BuildName()
{
   return wxT("Select");
}

void SelectCommandType::BuildSignature(CommandSignature &signature)
{
   OptionValidator *modeValidator = new OptionValidator();
   modeValidator->AddOption(wxT("None"));
   modeValidator->AddOption(wxT("All"));
   modeValidator->AddOption(wxT("Range"));
   modeValidator->AddOption(wxT("Name"));
   signature.AddParameter(wxT("Mode"), wxT("All"), modeValidator);

   DoubleValidator *startTimeValidator = new DoubleValidator();
   signature.AddParameter(wxT("StartTime"), 0.0, startTimeValidator);
   DoubleValidator *endTimeValidator = new DoubleValidator();
   signature.AddParameter(wxT("EndTime"), 0.0, endTimeValidator);
   IntValidator *firstTrackValidator = new IntValidator();

   signature.AddParameter(wxT("FirstTrack"), 0, firstTrackValidator);
   IntValidator *lastTrackValidator = new IntValidator();
   signature.AddParameter(wxT("LastTrack"), 0, lastTrackValidator);

   Validator *trackNameValidator = new Validator();
   signature.AddParameter(wxT("TrackName"), 0, trackNameValidator);
}

Command *SelectCommandType::Create(CommandOutputTarget *target)
{
   return new SelectCommand(*this, target);
}

bool SelectCommand::Apply(CommandExecutionContext context)
{
   wxString mode = GetString(wxT("Mode"));
   if (mode.IsSameAs(wxT("None")))
   {
      // select none
      context.GetProject()->OnSelectNone();
   }
   else if (mode.IsSameAs(wxT("All")))
   {
      // select all
      context.GetProject()->OnSelectAll();
   }
   else if (mode.IsSameAs(wxT("Range")))
   {
      // select range
      double t0 = GetDouble(wxT("StartTime"));
      double t1 = GetDouble(wxT("EndTime"));

      TrackList *tracks = context.GetProject()->GetTracks();

      if (t0 < context.GetProject()->GetTracks()->GetMinOffset())
      {
         Error(wxT("Start time is before start of track!"));
         return false;
      }
      if (t1 > context.GetProject()->GetTracks()->GetEndTime())
      {
         Error(wxT("End time is after end of track!"));
         return false;
      }

      // PRL: to do: only setting time boundaries of current selection.
      // Should other fields be left alone, or rather
      // defaulted, as in the second branch?
      // Or should this command take more parameters?
#if 1
      context.GetProject()->mViewInfo.selectedRegion.setTimes(t0, t1);
#else
      context.GetProject()->mViewInfo.selectedRegion = SelectedRegion(t0, t1);
#endif

      // select specified tracks
      long firstTrack = GetLong(wxT("FirstTrack"));
      long lastTrack = GetLong(wxT("LastTrack"));

      if (firstTrack < 0)
      {
         Error(wxT("Trying to select a negatively numbered track!"));
         return false;
      }
      if (lastTrack >= tracks->GetCount())
      {
         Error(wxT("Trying to select higher number track than exists!"));
         return false;
      }

      int index = 0;
      TrackListIterator iter(tracks);
      Track *t = iter.First();
      while (t) {
         bool sel = firstTrack <= index && index <= lastTrack;
         t->SetSelected(sel);

         if (sel)
            Status(wxT("Selected track '") + t->GetName() + wxT("'"));

         t = iter.Next();
         ++index;
      }
      wxASSERT(index >= lastTrack);
   }
   else if (mode.IsSameAs(wxT("Name")))
   {
      wxString name = GetString(wxT("TrackName"));
      TrackList *tracks = context.GetProject()->GetTracks();
      TrackListIterator iter(tracks);
      Track *t = iter.First();
      while (t) {
         bool sel = t->GetName().IsSameAs(name);
         t->SetSelected(sel);

         if (sel)
            Status(wxT("Selected track '") + t->GetName() + wxT("'"));

         t = iter.Next();
      }
   }
   else
   {
      Error(wxT("Invalid selection mode!"));
      return false;
   }
   return true;
}
