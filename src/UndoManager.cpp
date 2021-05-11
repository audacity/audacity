/**********************************************************************

  Audacity: A Digital Audio Editor

  UndoManager.cpp

  Dominic Mazzoni

*******************************************************************//**

\class UndoManager
\brief Works with HistoryDialog to provide the Undo functionality.

*//****************************************************************//**

\class UndoStackElem
\brief Holds one item with description and time range for the
UndoManager

*//*******************************************************************/



#include "UndoManager.h"

#include <wx/hashset.h>

#include "Clipboard.h"
#include "DBConnection.h"
#include "Diags.h"
#include "Project.h"
#include "SampleBlock.h"
#include "Sequence.h"
#include "WaveTrack.h"          // temp
//#include "NoteTrack.h"  // for Sonify* function declarations
#include "Diags.h"
#include "Tags.h"
#include "widgets/ProgressDialog.h"


#include <unordered_set>

wxDEFINE_EVENT(EVT_UNDO_PUSHED, wxCommandEvent);
wxDEFINE_EVENT(EVT_UNDO_MODIFIED, wxCommandEvent);
wxDEFINE_EVENT(EVT_UNDO_RENAMED, wxCommandEvent);
wxDEFINE_EVENT(EVT_UNDO_OR_REDO, wxCommandEvent);
wxDEFINE_EVENT(EVT_UNDO_RESET, wxCommandEvent);
wxDEFINE_EVENT(EVT_UNDO_PURGE, wxCommandEvent);

using SampleBlockID = long long;

static const AudacityProject::AttachedObjects::RegisteredFactory key{
   [](AudacityProject &project)
      { return std::make_unique<UndoManager>( project ); }
};

UndoManager &UndoManager::Get( AudacityProject &project )
{
   return project.AttachedObjects::Get< UndoManager >( key );
}

const UndoManager &UndoManager::Get( const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

UndoManager::UndoManager( AudacityProject &project )
   : mProject{ project }
{
   current = -1;
   saved = -1;
}

UndoManager::~UndoManager()
{
   wxASSERT( stack.empty() );
}

namespace {
   SpaceArray::value_type
   CalculateUsage(const TrackList &tracks, SampleBlockIDSet &seen)
   {
      SpaceArray::value_type result = 0;
      //TIMER_START( "CalculateSpaceUsage", space_calc );
      InspectBlocks(
         tracks,
         BlockSpaceUsageAccumulator( result ),
         &seen
      );
      return result;
   }
}

void UndoManager::CalculateSpaceUsage()
{
   space.clear();
   space.resize(stack.size(), 0);

   SampleBlockIDSet seen;

   // After copies and pastes, a block file may be used in more than
   // one place in one undo history state, and it may be used in more than
   // one undo history state.  It might even be used in two states, but not
   // in another state that is between them -- as when you have state A,
   // then make a cut to get state B, but then paste it back into state C.

   // So be sure to count each block file once only, in the last undo item that
   // contains it.

   // Why the last and not the first? Because the user of the History dialog
   // may DELETE undo states, oldest first.  To reclaim disk space you must
   // DELETE all states containing the block file.  So the block file's
   // contribution to space usage should be counted only in that latest state.

   for (size_t nn = stack.size(); nn--;)
   {
      // Scan all tracks at current level
      auto &tracks = *stack[nn]->state.tracks;
      space[nn] = CalculateUsage(tracks, seen);
   }

   // Count the usage of the clipboard separately, using another set.  Do not
   // multiple-count any block occurring multiple times within the clipboard.
   seen.clear();
   mClipboardSpaceUsage = CalculateUsage(
      Clipboard::Get().GetTracks(), seen);

   //TIMER_STOP( space_calc );
}

wxLongLong_t UndoManager::GetLongDescription(
   unsigned int n, TranslatableString *desc, TranslatableString *size)
{
   wxASSERT(n < stack.size());
   wxASSERT(space.size() == stack.size());

   *desc = stack[n]->description;

   *size = Internat::FormatSize(space[n]);

   return space[n];
}

void UndoManager::GetShortDescription(unsigned int n, TranslatableString *desc)
{
   wxASSERT(n < stack.size());

   *desc = stack[n]->shortDescription;
}

void UndoManager::SetLongDescription(
  unsigned int n, const TranslatableString &desc)
{
   n -= 1;

   wxASSERT(n < stack.size());

   stack[n]->description = desc;
}

void UndoManager::RemoveStateAt(int n)
{
   // Remove the state from the array first, and destroy it at function exit.
   // Because in case of callbacks from destruction of Sample blocks, there
   // might be a yield to GUI and other events might inspect the undo stack
   // (such as history window update).  Don't expose an inconsistent stack
   // state.
   auto iter = stack.begin() + n;
   auto state = std::move(*iter);
   stack.erase(iter);
}


//! Just to find a denominator for a progress indicator.
/*! This estimate procedure should in fact be exact */
size_t UndoManager::EstimateRemovedBlocks(size_t begin, size_t end)
{
   if (begin == end)
      return 0;

   // Collect ids that survive
   SampleBlockIDSet wontDelete;
   auto f = [&](const auto &p){
      InspectBlocks(*p->state.tracks, {}, &wontDelete);
   };
   auto first = stack.begin(), last = stack.end();
   std::for_each( first, first + begin, f );
   std::for_each( first + end, last, f );
   if (saved >= 0)
      std::for_each( first + saved, first + saved + 1, f );
   InspectBlocks(TrackList::Get(mProject), {}, &wontDelete);

   // Collect ids that won't survive (and are not negative pseudo ids)
   SampleBlockIDSet seen, mayDelete;
   std::for_each( first + begin, first + end, [&](const auto &p){
      auto &tracks = *p->state.tracks;
      InspectBlocks(tracks, [&]( const SampleBlock &block ){
         auto id = block.GetBlockID();
         if ( id > 0 && !wontDelete.count( id ) )
            mayDelete.insert( id );
      },
      &seen);
   } );
   return mayDelete.size();
}

void UndoManager::RemoveStates(size_t begin, size_t end)
{
   // Install a callback function that updates a progress indicator
   unsigned long long nToDelete = EstimateRemovedBlocks(begin, end),
      nDeleted = 0;
   ProgressDialog dialog{ XO("Progress"), XO("Discarding undo/redo history"),
      pdlgHideStopButton | pdlgHideCancelButton
   };
   auto callback = [&](const SampleBlock &){
      dialog.Update(++nDeleted, nToDelete);
   };
   auto &trackFactory = WaveTrackFactory::Get( mProject );
   auto &pSampleBlockFactory = trackFactory.GetSampleBlockFactory();
   auto prevCallback =
      pSampleBlockFactory->SetBlockDeletionCallback(callback);
   auto cleanup = finally([&]{ pSampleBlockFactory->SetBlockDeletionCallback( prevCallback ); });

   // Wrap the whole in a savepoint for better performance
   Optional<TransactionScope> pTrans;
   auto pConnection = ConnectionPtr::Get(mProject).mpConnection.get();
   if (pConnection)
      pTrans.emplace(*pConnection, "DiscardingUndoStates");

   for (size_t ii = begin; ii < end; ++ii) {
      RemoveStateAt(begin);

      if (current > begin)
        --current;
      if (saved > static_cast<int>(begin))
        --saved;
   }

   // Success, commit the savepoint
   if (pTrans)
      pTrans->Commit();
   
   if (begin != end)
      // wxWidgets will own the event object
      mProject.QueueEvent( safenew wxCommandEvent{ EVT_UNDO_PURGE } );

   // Check sanity
   wxASSERT_MSG(
      nDeleted == 0 || // maybe bypassing all deletions
      nDeleted == nToDelete, "Block count was misestimated");
}

void UndoManager::ClearStates()
{
   RemoveStates(0, stack.size());
   current = -1;
   saved = -1;
}

unsigned int UndoManager::GetNumStates()
{
   return stack.size();
}

unsigned int UndoManager::GetCurrentState()
{
   return current;
}

bool UndoManager::UndoAvailable()
{
   return (current > 0);
}

bool UndoManager::RedoAvailable()
{
   return (current < (int)stack.size() - 1);
}

void UndoManager::ModifyState(const TrackList * l,
                              const SelectedRegion &selectedRegion,
                              const std::shared_ptr<Tags> &tags)
{
   if (current == wxNOT_FOUND) {
      return;
   }

//   SonifyBeginModifyState();
   // Delete current -- not necessary, but let's reclaim space early
   stack[current]->state.tracks.reset();

   // Duplicate
   auto tracksCopy = TrackList::Create( nullptr );
   for (auto t : *l) {
      if ( t->GetId() == TrackId{} )
         // Don't copy a pending added track
         continue;
      tracksCopy->Add(t->Duplicate());
   }

   // Replace
   stack[current]->state.tracks = std::move(tracksCopy);
   stack[current]->state.tags = tags;

   stack[current]->state.selectedRegion = selectedRegion;
//   SonifyEndModifyState();

   // wxWidgets will own the event object
   mProject.QueueEvent( safenew wxCommandEvent{ EVT_UNDO_MODIFIED } );
}

void UndoManager::RenameState( int state,
   const TranslatableString &longDescription,
   const TranslatableString &shortDescription)
{
   if (state >= 0 && state < stack.size() ) {
      auto &theState = *stack[state];
      theState.description = longDescription;
      theState.shortDescription = shortDescription;

      // wxWidgets will own the event object
      mProject.QueueEvent( safenew wxCommandEvent{ EVT_UNDO_RENAMED } );
   }
}

void UndoManager::PushState(const TrackList * l,
                            const SelectedRegion &selectedRegion,
                            const std::shared_ptr<Tags> &tags,
                            const TranslatableString &longDescription,
                            const TranslatableString &shortDescription,
                            UndoPush flags)
{
   if ( (flags & UndoPush::CONSOLIDATE) != UndoPush::NONE &&
       // compare full translations not msgids!
       lastAction.Translation() == longDescription.Translation() &&
       mayConsolidate ) {
      ModifyState(l, selectedRegion, tags);
      // MB: If the "saved" state was modified by ModifyState, reset
      //  it so that UnsavedChanges returns true.
      if (current == saved) {
         saved = -1;
      }
      return;
   }

   auto tracksCopy = TrackList::Create( nullptr );
   for (auto t : *l) {
      if ( t->GetId() == TrackId{} )
         // Don't copy a pending added track
         continue;
      tracksCopy->Add(t->Duplicate());
   }

   mayConsolidate = true;

   AbandonRedo();

   // Assume tags was duplicated before any changes.
   // Just save a NEW shared_ptr to it.
   stack.push_back(
      std::make_unique<UndoStackElem>
         (std::move(tracksCopy),
            longDescription, shortDescription, selectedRegion, tags)
   );

   current++;

   lastAction = longDescription;

   // wxWidgets will own the event object
   mProject.QueueEvent( safenew wxCommandEvent{ EVT_UNDO_PUSHED } );
}

void UndoManager::AbandonRedo()
{
   if (saved > current) {
      saved = -1;
   }
   RemoveStates( current + 1, stack.size() );
}

void UndoManager::SetStateTo(unsigned int n, const Consumer &consumer)
{
   wxASSERT(n < stack.size());

   current = n;

   lastAction = {};
   mayConsolidate = false;

   consumer( *stack[current] );

   // wxWidgets will own the event object
   mProject.QueueEvent( safenew wxCommandEvent{ EVT_UNDO_RESET } );
}

void UndoManager::Undo(const Consumer &consumer)
{
   wxASSERT(UndoAvailable());

   current--;

   lastAction = {};
   mayConsolidate = false;

   consumer( *stack[current] );

   // wxWidgets will own the event object
   mProject.QueueEvent( safenew wxCommandEvent{ EVT_UNDO_OR_REDO } );
}

void UndoManager::Redo(const Consumer &consumer)
{
   wxASSERT(RedoAvailable());

   current++;

   /*
   if (!RedoAvailable()) {
      *sel0 = stack[current]->sel0;
      *sel1 = stack[current]->sel1;
   }
   else {
      current++;
      *sel0 = stack[current]->sel0;
      *sel1 = stack[current]->sel1;
      current--;
   }
   */

   lastAction = {};
   mayConsolidate = false;

   consumer( *stack[current] );

   // wxWidgets will own the event object
   mProject.QueueEvent( safenew wxCommandEvent{ EVT_UNDO_OR_REDO } );
}

void UndoManager::VisitStates( const Consumer &consumer, bool newestFirst )
{
   auto fn = [&]( decltype(stack[0]) &ptr ){ consumer( *ptr ); };
   if (newestFirst)
      std::for_each(stack.rbegin(), stack.rend(), fn);
   else
      std::for_each(stack.begin(), stack.end(), fn);
}

void UndoManager::VisitStates(
   const Consumer &consumer, size_t begin, size_t end )
{
   auto size = stack.size();
   if (begin < end) {
      end = std::min(end, size);
      for (auto ii = begin; ii < end; ++ii)
         consumer(*stack[ii]);
   }
   else {
      if (size == 0)
         return;
      begin = std::min(begin, size - 1);
      for (auto ii = begin; ii > end; --ii)
         consumer(*stack[ii]);
   }
}

bool UndoManager::UnsavedChanges() const
{
   return (saved != current);
}

void UndoManager::StateSaved()
{
   saved = current;
}

int UndoManager::GetSavedState() const
{
   return saved;
}

// currently unused
//void UndoManager::Debug()
//{
//   for (unsigned int i = 0; i < stack.size(); i++) {
//      for (auto t : stack[i]->tracks->Any())
//         wxPrintf(wxT("*%d* %s %f\n"),
//                  i, (i == (unsigned int)current) ? wxT("-->") : wxT("   "),
//                t ? t->GetEndTime()-t->GetStartTime() : 0);
//   }
//}

