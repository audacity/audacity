/**********************************************************************

  Audacity: A Digital Audio Editor

  Menus.cpp

  Dominic Mazzoni
  Brian Gunlogson
  et al.

*******************************************************************//**

\file Menus.cpp
\brief Functions for building toobar menus and enabling and disabling items

*//****************************************************************//**

\class MenuCreator
\brief MenuCreator is responsible for creating the main menu bar.

*//****************************************************************//**

\class MenuManager
\brief MenuManager handles updates to menu state.

*//*******************************************************************/

#include "Audacity.h" // for USE_* macros
#include "Menus.h"

#include "Experimental.h"

#include <wx/frame.h>

#include "ModuleManager.h"
#include "Project.h"
#include "ProjectHistory.h"
#include "ProjectSettings.h"
#include "UndoManager.h"
#include "commands/CommandManager.h"
#include "prefs/TracksPrefs.h"
#include "toolbars/ToolManager.h"
#include "widgets/AudacityMessageBox.h"
#include "widgets/ErrorDialog.h"

#include <unordered_set>

#include <wx/menu.h>
#include <wx/windowptr.h>

MenuCreator::MenuCreator()
{
}

MenuCreator::~MenuCreator()
{
}

static const AudacityProject::AttachedObjects::RegisteredFactory key{
  []( AudacityProject &project ){
     return std::make_shared< MenuManager >( project ); }
};

MenuManager &MenuManager::Get( AudacityProject &project )
{
   return project.AttachedObjects::Get< MenuManager >( key );
}

const MenuManager &MenuManager::Get( const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

MenuManager::MenuManager( AudacityProject &project )
   : mProject{ project }
{
   UpdatePrefs();
   mProject.Bind( EVT_UNDO_OR_REDO, &MenuManager::OnUndoRedo, this );
   mProject.Bind( EVT_UNDO_RESET, &MenuManager::OnUndoRedo, this );
   mProject.Bind( EVT_UNDO_PUSHED, &MenuManager::OnUndoRedo, this );
}

MenuManager::~MenuManager()
{
   mProject.Unbind( EVT_UNDO_OR_REDO, &MenuManager::OnUndoRedo, this );
   mProject.Unbind( EVT_UNDO_RESET, &MenuManager::OnUndoRedo, this );
   mProject.Unbind( EVT_UNDO_PUSHED, &MenuManager::OnUndoRedo, this );
}

void MenuManager::UpdatePrefs()
{
   bool bSelectAllIfNone;
   gPrefs->Read(wxT("/GUI/SelectAllOnNone"), &bSelectAllIfNone, false);
   // 0 is grey out, 1 is Autoselect, 2 is Give warnings.
#ifdef EXPERIMENTAL_DA
   // DA warns or greys out.
   mWhatIfNoSelection = bSelectAllIfNone ? 2 : 0;
#else
   // Audacity autoselects or warns.
   mWhatIfNoSelection = bSelectAllIfNone ? 1 : 2;
#endif
   mStopIfWasPaused = true;  // not configurable for now, but could be later.
}

/// Namespace for structures that go into building a menu
namespace Registry {

BaseItem::~BaseItem() {}

SharedItem::~SharedItem() {}

ComputedItem::~ComputedItem() {}

SingleItem::~SingleItem() {}

GroupItem::~GroupItem() {}

Visitor::~Visitor(){}
void Visitor::BeginGroup(GroupItem &, const Path &) {}
void Visitor::EndGroup(GroupItem &, const Path &) {}
void Visitor::Visit(SingleItem &, const Path &) {}

}

namespace MenuTable {

MenuItem::MenuItem( const wxString &internalName,
   const TranslatableString &title_, BaseItemPtrs &&items_ )
: ConcreteGroupItem< false, MenuVisitor >{
   internalName, std::move( items_ ) }, title{ title_ }
{
   wxASSERT( !title.empty() );
}
MenuItem::~MenuItem() {}

ConditionalGroupItem::ConditionalGroupItem(
   const wxString &internalName, Condition condition_, BaseItemPtrs &&items_ )
: ConcreteGroupItem< false, MenuVisitor >{
   internalName, std::move( items_ ) }, condition{ condition_ }
{
}
ConditionalGroupItem::~ConditionalGroupItem() {}

CommandItem::CommandItem(const CommandID &name_,
         const TranslatableString &label_in_,
         CommandFunctorPointer callback_,
         CommandFlag flags_,
         const CommandManager::Options &options_,
         CommandHandlerFinder finder_)
: SingleItem{ name_ }, label_in{ label_in_ }
, finder{ finder_ }, callback{ callback_ }
, flags{ flags_ }, options{ options_ }
{}
CommandItem::~CommandItem() {}

CommandGroupItem::CommandGroupItem(const wxString &name_,
         std::vector< ComponentInterfaceSymbol > items_,
         CommandFunctorPointer callback_,
         CommandFlag flags_,
         bool isEffect_,
         CommandHandlerFinder finder_)
: SingleItem{ name_ }, items{ std::move(items_) }
, finder{ finder_ }, callback{ callback_ }
, flags{ flags_ }, isEffect{ isEffect_ }
{}
CommandGroupItem::~CommandGroupItem() {}

SpecialItem::~SpecialItem() {}

CommandHandlerFinder FinderScope::sFinder =
   [](AudacityProject &project) -> CommandHandlerObject & {
      // If this default finder function is reached, then FinderScope should
      // have been used somewhere, or an explicit CommandHandlerFinder passed
      // to menu item constructors
      wxASSERT( false );
      return project;
   };

}

namespace {

const auto MenuPathStart = wxT("MenuBar");

struct ItemOrdering;

using namespace Registry;
struct CollectedItems
{
   struct Item{
      // Predefined, or merged from registry already:
      BaseItem *visitNow;
      // Corresponding item from the registry, its sub-items to be merged:
      GroupItem *mergeLater;
      // Ordering hint for the merged item:
      OrderingHint hint;
   };
   std::vector< Item > items;
   std::vector< BaseItemSharedPtr > &computedItems;

   // A linear search.  Smarter search may not be worth the effort.
   using Iterator = decltype( items )::iterator;
   auto Find( const Identifier &name ) -> Iterator
   {
      auto end = items.end();
      return name.empty()
         ? end
         : std::find_if( items.begin(), end,
            [&]( const Item& item ){
               return name == item.visitNow->name; } );
   }

   auto InsertNewItemUsingPreferences(
      ItemOrdering &itemOrdering, BaseItem *pItem ) -> bool;

   auto InsertNewItemUsingHint(
      BaseItem *pItem, const OrderingHint &hint, bool force ) -> bool;

   auto SubordinateSingleItem( Item &found, BaseItem *pItem ) -> void;

   auto MergeItems(
      Visitor &visitor, ItemOrdering &itemOrdering,
      const BaseItemPtrs &toMerge, const OrderingHint &hint ) -> void;

   auto MergeItem(
      Visitor &visitor, ItemOrdering &itemOrdering, BaseItem *pItem ) -> bool;
};

// When a computed or shared item, or nameless grouping, specifies a hint and
// the subordinate does not, propagate the hint.
const OrderingHint &ChooseHint(BaseItem *delegate, const OrderingHint &hint)
{
   return !delegate || delegate->orderingHint.type == OrderingHint::Unspecified
      ? hint
      : delegate->orderingHint;
}

// "Collection" of items is the first pass of visitation, and resolves
// delegation and delayed computation and splices transparent group nodes.
// This first pass is done at each group, starting with a top-level group.
// This pass does not descend to the leaves.  Rather, the visitation passes
// alternate as the entire tree is recursively visited.

// forward declaration for mutually recursive functions
void CollectItem( Registry::Visitor &visitor,
   CollectedItems &collection, BaseItem *Item, const OrderingHint &hint );
void CollectItems( Registry::Visitor &visitor,
   CollectedItems &collection, const BaseItemPtrs &items,
   const OrderingHint &hint )
{
   for ( auto &item : items )
      CollectItem( visitor, collection, item.get(),
         ChooseHint( item.get(), hint ) );
}
void CollectItem( Registry::Visitor &visitor,
   CollectedItems &collection, BaseItem *pItem, const OrderingHint &hint )
{
   if (!pItem)
      return;

   using namespace Registry;
   if (const auto pShared =
       dynamic_cast<SharedItem*>( pItem )) {
      auto delegate = pShared->ptr.get();
      if ( delegate )
         // recursion
         CollectItem( visitor, collection, delegate,
            ChooseHint( delegate, pShared->orderingHint ) );
   }
   else
   if (const auto pComputed =
       dynamic_cast<ComputedItem*>( pItem )) {
      auto result = pComputed->factory( visitor );
      if (result) {
         // Guarantee long enough lifetime of the result
         collection.computedItems.push_back( result );
         // recursion
         CollectItem( visitor, collection, result.get(),
            ChooseHint( result.get(), pComputed->orderingHint ) );
      }
   }
   else
   if (auto pGroup = dynamic_cast<GroupItem*>(pItem)) {
      if (pGroup->Transparent() && pItem->name.empty())
         // nameless grouping item is transparent to path calculations
         // collect group members now
         // recursion
         CollectItems(
            visitor, collection, pGroup->items, pGroup->orderingHint );
      else
         // all other group items
         // defer collection of members until collecting at next lower level
         collection.items.push_back( {pItem, nullptr, hint} );
   }
   else {
      wxASSERT( dynamic_cast<SingleItem*>(pItem) );
      // common to all single items
      collection.items.push_back( {pItem, nullptr, hint} );
   }
}

using Path = std::vector< Identifier >;

namespace {
   std::unordered_set< wxString > sBadPaths;
   void BadPath(
     const TranslatableString &format, const wxString &key, const Identifier &name )
   {
     // Warn, but not more than once in a session for each bad path
     auto badPath = key + '/' + name.GET();
     if ( sBadPaths.insert( badPath ).second ) {
        auto msg = TranslatableString{ format }.Format( badPath );
        // debug message
        wxLogDebug( msg.Translation() );
#ifdef IS_ALPHA
        // user-visible message
        AudacityMessageBox( msg );
#endif
     }
   }

   void ReportGroupGroupCollision( const wxString &key, const Identifier &name )
   {
      BadPath(
XO("Plug-in group at %s was merged with a previously defined group"),
         key, name);
   }

   void ReportItemItemCollision( const wxString &key, const Identifier &name )
   {
      BadPath(
XO("Plug-in item at %s conflicts with a previously defined item and was discarded"),
         key, name);
   }

   void ReportConflictingPlacements( const wxString &key, const Identifier &name )
   {
      BadPath(
XO("Plug-in items at %s specify conflicting placements"),
         key, name);
   }
}

struct ItemOrdering {
   wxString key;

   ItemOrdering( const Path &path )
   {
      // The set of path names determines only an unordered tree.
      // We want an ordering of the tree that is stable across runs.
      // The last used ordering for this node can be found in preferences at this
      // key:
      wxArrayString strings;
      for (const auto &id : path)
         strings.push_back( id.GET() );
      key = '/' + ::wxJoin( strings, '/', '\0' );
   }

   // Retrieve the old ordering on demand, if needed to merge something.
   bool gotOrdering = false;
   wxString strValue;
   wxArrayString ordering;

   auto Get() -> wxArrayString & {
      if ( !gotOrdering ) {
         gPrefs->Read(key, &strValue);
         ordering = ::wxSplit( strValue, ',' );
         gotOrdering = true;
      }
      return ordering;
   };
};

auto CollectedItems::InsertNewItemUsingPreferences(
   ItemOrdering &itemOrdering, BaseItem *pItem )
   -> bool
{
   // Note that if more than one plug-in registers items under the same
   // node, then it is not specified which plug-in is handled first,
   // the first time registration happens.  It might happen that you
   // add a plug-in, run the program, then add another, then run again;
   // registration order determined by those actions might not
   // correspond to the order of re-loading of modules in later
   // sessions.  But whatever ordering is chosen the first time some
   // plug-in is seen -- that ordering gets remembered in preferences.

   if ( !pItem->name.empty() ) {
      // Check saved ordering first, and rebuild that as well as is possible
      auto &ordering = itemOrdering.Get();
      auto begin2 = ordering.begin(), end2 = ordering.end(),
         found2 = std::find( begin2, end2, pItem->name );
      if ( found2 != end2 ) {
         auto insertPoint = items.end();
         // Find the next name in the saved ordering that is known already
         // in the collection.
         while ( ++found2 != end2 ) {
            auto known = Find( *found2 );
            if ( known != insertPoint ) {
               insertPoint = known;
               break;
            }
         }
         items.insert( insertPoint, {pItem, nullptr,
            // Hints no longer matter:
            {}} );
         return true;
      }
   }

   return false;
}

auto CollectedItems::InsertNewItemUsingHint(
   BaseItem *pItem, const OrderingHint &hint, bool force ) -> bool
{
   auto begin = items.begin(), end = items.end();

   // pItem should have a name; if not, ignore the hint and put it at the
   // end.
   auto insertPoint = end;

   if ( !pItem->name.empty() ) {
      // Use the placement hint.
      // If more than one item request the same placement, they
      // end up in the sequence in which they were merged (at least in
      // cases other than After)
      switch ( hint.type ) {
         case OrderingHint::Before:
         case OrderingHint::After:
            // Default to the end if the name is not found.
            insertPoint = Find( hint.name );
            if ( insertPoint == end ) {
               if ( !force )
                  return false;
            }
            else if ( hint.type == OrderingHint::After )
               ++insertPoint;
            break;
         case OrderingHint::Begin:
            insertPoint = begin;
            break;
         case OrderingHint::End:
            break;
         case OrderingHint::Unspecified:
         default:
            if ( !force )
               return false;
            break;
      }
   }

   // Insert the item; the hint has been used and no longer matters
   items.insert( insertPoint, {pItem, nullptr,
      // Hints no longer matter:
      {}} );
   return true;
}

auto CollectedItems::SubordinateSingleItem( Item &found, BaseItem *pItem )
   -> void
{
   auto subGroup = std::make_shared<TransparentGroupItem<>>( pItem->name,
      std::make_unique<SharedItem>(
         // shared pointer with vacuous deleter
         std::shared_ptr<BaseItem>( pItem, [](void*){} ) ) );
   found.mergeLater = subGroup.get();
   computedItems.push_back( subGroup );
}

auto CollectedItems::MergeItem(
   Visitor &visitor, ItemOrdering &itemOrdering, BaseItem *pItem ) -> bool
{
   // Assume no null pointers in the registry
   const auto &name = pItem->name;
   auto found = Find( name );
   if (found != items.end()) {
      // Collision of names between collection and registry!
      // There are 2 * 2 = 4 cases, as each of the two are group items or
      // not.
      auto pCollectionGroup = dynamic_cast< GroupItem * >( found->visitNow );
      auto pRegistryGroup = dynamic_cast< GroupItem * >( pItem );
      if (pCollectionGroup) {
         if (pRegistryGroup) {
            // This is the expected case of collision.
            // Subordinate items from one of the groups will be merged in
            // another call to MergeItems at a lower level of path.
            // Note, however, that at most one of the two should be other
            // than a plain grouping item; if not, we must lose the extra
            // information carried by one of them.
            bool pCollectionGrouping = pCollectionGroup->Transparent();
            auto pRegistryGrouping = pRegistryGroup->Transparent();
            if ( !(pCollectionGrouping || pRegistryGrouping) )
               ReportGroupGroupCollision( itemOrdering.key, name );

            if ( pCollectionGrouping && !pRegistryGrouping ) {
               // Swap their roles
               found->visitNow = pRegistryGroup;
               found->mergeLater = pCollectionGroup;
            }
            else
               found->mergeLater = pRegistryGroup;
         }
         else {
            // Registered non-group item collides with a previously defined
            // group.
            // Resolve this by subordinating the non-group item below
            // that group.
            SubordinateSingleItem( *found, pItem );
         }
      }
      else {
         if (pRegistryGroup) {
            // Subordinate the previously merged single item below the
            // newly merged group.
            // In case the name occurred in two different static registries,
            // the final merge is the same, no matter which is treated first.
            auto demoted = found->visitNow;
            found->visitNow = pRegistryGroup;
            SubordinateSingleItem( *found, demoted );
         }
         else
            // Collision of non-group items is the worst case!
            // The later-registered item is lost.
            // Which one you lose might be unpredictable when both originate
            // from static registries.
            ReportItemItemCollision( itemOrdering.key, name );
      }
      return true;
   }
   else
      // A name is registered that is not known in the collection.
      return false;
}

auto CollectedItems::MergeItems(
  Visitor &visitor, ItemOrdering &itemOrdering,
  const BaseItemPtrs &toMerge, const OrderingHint &hint ) -> void
{
   // First do expansion of nameless groupings, and caching of computed
   // items, just as for the previously defined menus.
   CollectedItems newCollection{ {}, computedItems };
   CollectItems( visitor, newCollection, toMerge, hint );

   // Try to merge each, resolving name collisions with items already in the
   // tree, and collecting those with names that don't collide.
   using NewItem = std::pair< BaseItem*, OrderingHint >;
   std::vector< NewItem > newItems;
   for ( const auto &item : newCollection.items )
      if ( !MergeItem( visitor, itemOrdering, item.visitNow ) )
          newItems.push_back( { item.visitNow, item.hint } );

   // There may still be unresolved name collisions among the NEW items,
   // so first find their sorted order.
   static auto majorComp = [](const NewItem &a, const NewItem &b) {
      // Descending sort!
      return a.first->name > b.first->name;
   };
   auto minorComp = [](const NewItem &a, const NewItem &b){
      // Sort items with specified hints earlier.
      return a.second < b.second;
   };
   auto comp = [&](const NewItem &a, const NewItem &b){
      if ( majorComp( a, b ) )
         return true;
      if ( majorComp( b, a ) )
         return false;
      return minorComp( a, b );
   };
   std::sort( newItems.begin(), newItems.end(), comp );

   // Choose placements for items with NEW names.
   // Outer loop over trial passes.
   int iPass = 0;
   while( !newItems.empty() ) {
      // Inner loop over ranges of like-named items.
      // Do it right to left, to shrink array faster and avoid invalidating rend.
      auto right = newItems.rbegin();
      auto rend = newItems.rend();
      bool forceNext = true;
      while ( right != rend ) {
         // Find the range
         using namespace std::placeholders;
         auto left = std::find_if(
            right + 1, rend, std::bind( majorComp, _1, *right ) );

         // Try to place the first item of the range.
         // If such an item is a group, then we always retain the kind of
         // grouping that was registered.  (Which doesn't always happen when
         // there is name collision in MergeItem.)
         auto iter = left.base();
         auto &item = *iter;
         auto pItem = item.first;
         const auto &hint = item.second;
         bool success = true;
         if ( iPass == 0 )
            // A first pass consults preferences.
            success = InsertNewItemUsingPreferences( itemOrdering, pItem );
         else {
            // Later passes for choosing placements.
            bool forceNow = iPass == -1;
            // Maybe it fails in this pass, because a placement refers to some
            // other name that has not yet been placed.
            success = InsertNewItemUsingHint( pItem, hint, forceNow );
            wxASSERT( !forceNow || success );
            // While some progress is made, don't force final placements.
            if ( success )
               forceNext = false;
         }

         if ( success ) {
            // Resolve collisions among remaining like-named items.
            ++iter;
            if ( iter != right.base() && iPass != 0 &&
                iter->second.type != OrderingHint::Unspecified &&
                !( iter->second == hint ) ) {
               // A diagnostic message sometimes
               ReportConflictingPlacements( itemOrdering.key, pItem->name );
            }
            while ( iter != right.base() )
               // Re-invoke MergeItem for this item, which is known to have a name
               // collision, so ignore the return value.
               MergeItem( visitor, itemOrdering, iter++ -> first );
            newItems.erase( left.base(), right.base() );
         }

         right = left;
      }
      iPass = forceNext ? -1 : 1;
   }
}

// forward declaration for mutually recursive functions
void VisitItem(
   Registry::Visitor &visitor, CollectedItems &collection,
   Path &path, BaseItem *pItem,
   GroupItem *pToMerge, const OrderingHint &hint,
   bool &doFlush );
void VisitItems(
   Registry::Visitor &visitor, CollectedItems &collection,
   Path &path, GroupItem *pGroup,
   GroupItem *pToMerge, const OrderingHint &hint,
   bool &doFlush )
{
   // Make a NEW collection for this subtree, sharing the memo cache
   CollectedItems newCollection{ {}, collection.computedItems };

   // Gather items at this level
   // (The ordering hint is irrelevant when not merging items in)
   CollectItems( visitor, newCollection, pGroup->items, {} );

   path.push_back( pGroup->name.GET() );

   // Merge with the registry
   if ( pToMerge )
   {
      ItemOrdering itemOrdering{ path };
      newCollection.MergeItems( visitor, itemOrdering, pToMerge->items, hint );

      // Remember the NEW ordering, if there was any need to use the old.
      // This makes a side effect in preferences.
      if ( itemOrdering.gotOrdering ) {
         wxString newValue;
         for ( const auto &item : newCollection.items ) {
            const auto &name = item.visitNow->name;
            if ( !name.empty() )
               newValue += newValue.empty()
                  ? name.GET()
                  : ',' + name.GET();
         }
         if (newValue != itemOrdering.strValue) {
            gPrefs->Write( itemOrdering.key, newValue );
            doFlush = true;
         }
      }
   }

   // Now visit them
   for ( const auto &item : newCollection.items )
      VisitItem( visitor, collection, path,
         item.visitNow, item.mergeLater, item.hint,
         doFlush );

   path.pop_back();
}
void VisitItem(
   Registry::Visitor &visitor, CollectedItems &collection,
   Path &path, BaseItem *pItem,
   GroupItem *pToMerge, const OrderingHint &hint,
   bool &doFlush )
{
   if (!pItem)
      return;

   if (const auto pSingle =
       dynamic_cast<SingleItem*>( pItem )) {
      visitor.Visit( *pSingle, path );
   }
   else
   if (const auto pGroup =
       dynamic_cast<GroupItem*>( pItem )) {
      visitor.BeginGroup( *pGroup, path );
      // recursion
      VisitItems(
         visitor, collection, path, pGroup, pToMerge, hint, doFlush );
      visitor.EndGroup( *pGroup, path );
   }
   else
      wxASSERT( false );
}

}

namespace Registry {

void Visit( Visitor &visitor, BaseItem *pTopItem, GroupItem *pRegistry )
{
   std::vector< BaseItemSharedPtr > computedItems;
   bool doFlush = false;
   CollectedItems collection{ {}, computedItems };
   Path emptyPath;
   VisitItem(
      visitor, collection, emptyPath, pTopItem,
      pRegistry, pRegistry->orderingHint, doFlush );
   // Flush any writes done by MergeItems()
   if (doFlush)
      gPrefs->Flush();
}

}

/// CreateMenusAndCommands builds the menus, and also rebuilds them after
/// changes in configured preferences - for example changes in key-bindings
/// affect the short-cut key legend that appears beside each command,

MenuTable::BaseItemSharedPtr FileMenu();

MenuTable::BaseItemSharedPtr EditMenu();

MenuTable::BaseItemSharedPtr SelectMenu();

MenuTable::BaseItemSharedPtr ViewMenu();

MenuTable::BaseItemSharedPtr TransportMenu();

MenuTable::BaseItemSharedPtr TracksMenu();

MenuTable::BaseItemSharedPtr GenerateMenu();
MenuTable::BaseItemSharedPtr EffectMenu();
MenuTable::BaseItemSharedPtr AnalyzeMenu();
MenuTable::BaseItemSharedPtr ToolsMenu();

MenuTable::BaseItemSharedPtr WindowMenu();

MenuTable::BaseItemSharedPtr ExtraMenu();

MenuTable::BaseItemSharedPtr HelpMenu();

namespace {
static Registry::GroupItem &sRegistry()
{
   static Registry::TransparentGroupItem<> registry{ MenuPathStart };
   return registry;
}
}

// Table of menu factories.
// TODO:  devise a registration system instead.
static const auto menuTree = MenuTable::Items( MenuPathStart
   , FileMenu()
   , EditMenu()
   , SelectMenu()
   , ViewMenu()
   , TransportMenu()
   , TracksMenu()
   , GenerateMenu()
   , EffectMenu()
   , AnalyzeMenu()
   , ToolsMenu()
   , WindowMenu()
   , ExtraMenu()
   , HelpMenu()
);

namespace {
using namespace MenuTable;
struct MenuItemVisitor : MenuVisitor
{
   MenuItemVisitor( AudacityProject &proj, CommandManager &man )
      : MenuVisitor(proj), manager( man ) {}

   void BeginGroup( GroupItem &item, const Path& ) override
   {
      auto pItem = &item;
      if (const auto pMenu =
          dynamic_cast<MenuItem*>( pItem )) {
         manager.BeginMenu( pMenu->title );
      }
      else
      if (const auto pConditionalGroup =
          dynamic_cast<ConditionalGroupItem*>( pItem )) {
         const auto flag = pConditionalGroup->condition();
         if (!flag)
            manager.BeginOccultCommands();
         // to avoid repeated call of condition predicate in EndGroup():
         flags.push_back(flag);
      }
      else
      if ( pItem->Transparent() ) {
      }
      else
      if ( const auto pGroup = dynamic_cast<MenuSection*>( pItem ) ) {
         manager.AddSeparator();
      }
      else
         wxASSERT( false );
   }

   void EndGroup( GroupItem &item, const Path& ) override
   {
      auto pItem = &item;
      if (const auto pMenu =
          dynamic_cast<MenuItem*>( pItem )) {
         manager.EndMenu();
      }
      else
      if (const auto pConditionalGroup =
          dynamic_cast<ConditionalGroupItem*>( pItem )) {
         const bool flag = flags.back();
         if (!flag)
            manager.EndOccultCommands();
         flags.pop_back();
      }
      else
      if ( pItem->Transparent() ) {
      }
      else
      if ( const auto pGroup = dynamic_cast<MenuSection*>( pItem ) ) {
         manager.AddSeparator();
      }
      else
         wxASSERT( false );
   }

   void Visit( SingleItem &item, const Path& ) override
   {
      auto pItem = &item;
      if (const auto pCommand =
          dynamic_cast<CommandItem*>( pItem )) {
         manager.AddItem( project,
            pCommand->name, pCommand->label_in,
            pCommand->finder, pCommand->callback,
            pCommand->flags, pCommand->options
         );
      }
      else
      if (const auto pCommandList =
         dynamic_cast<CommandGroupItem*>( pItem ) ) {
         manager.AddItemList(pCommandList->name,
            pCommandList->items.data(), pCommandList->items.size(),
            pCommandList->finder, pCommandList->callback,
            pCommandList->flags, pCommandList->isEffect);
      }
      else
      if (const auto pSpecial =
          dynamic_cast<SpecialItem*>( pItem )) {
         const auto pCurrentMenu = manager.CurrentMenu();
         wxASSERT( pCurrentMenu );
         pSpecial->fn( project, *pCurrentMenu );
      }
      else
         wxASSERT( false );
   }

   CommandManager &manager;
   std::vector<bool> flags;
};
}

void MenuCreator::CreateMenusAndCommands(AudacityProject &project)
{
   auto &commandManager = CommandManager::Get( project );

   // The list of defaults to exclude depends on
   // preference wxT("/GUI/Shortcuts/FullDefaults"), which may have changed.
   commandManager.SetMaxList();

   auto menubar = commandManager.AddMenuBar(wxT("appmenu"));
   wxASSERT(menubar);

   MenuItemVisitor visitor{ project, commandManager };
   MenuManager::Visit( visitor );

   GetProjectFrame( project ).SetMenuBar(menubar.release());

   mLastFlags = AlwaysEnabledFlag;

#if defined(__WXDEBUG__)
//   c->CheckDups();
#endif
}

void MenuManager::Visit( MenuVisitor &visitor )
{
   Registry::Visit( visitor, menuTree.get(), &sRegistry() );
}

// TODO: This surely belongs in CommandManager?
void MenuManager::ModifyUndoMenuItems(AudacityProject &project)
{
   TranslatableString desc;
   auto &undoManager = UndoManager::Get( project );
   auto &commandManager = CommandManager::Get( project );
   int cur = undoManager.GetCurrentState();

   if (undoManager.UndoAvailable()) {
      undoManager.GetShortDescription(cur, &desc);
      commandManager.Modify(wxT("Undo"),
                             XO("&Undo %s").Format( desc ));
      commandManager.Enable(wxT("Undo"),
         ProjectHistory::Get( project ).UndoAvailable());
   }
   else {
      commandManager.Modify(wxT("Undo"),
                            XO("&Undo"));
   }

   if (undoManager.RedoAvailable()) {
      undoManager.GetShortDescription(cur+1, &desc);
      commandManager.Modify(wxT("Redo"),
                             XO("&Redo %s").Format( desc));
      commandManager.Enable(wxT("Redo"),
         ProjectHistory::Get( project ).RedoAvailable());
   }
   else {
      commandManager.Modify(wxT("Redo"),
                            XO("&Redo"));
      commandManager.Enable(wxT("Redo"), false);
   }
}

// Get hackcess to a protected method
class wxFrameEx : public wxFrame
{
public:
   using wxFrame::DetachMenuBar;
};

void MenuCreator::RebuildMenuBar(AudacityProject &project)
{
   // On OSX, we can't rebuild the menus while a modal dialog is being shown
   // since the enabled state for menus like Quit and Preference gets out of
   // sync with wxWidgets idea of what it should be.
#if defined(__WXMAC__) && defined(__WXDEBUG__)
   {
      wxDialog *dlg =
         wxDynamicCast(wxGetTopLevelParent(wxWindow::FindFocus()), wxDialog);
      wxASSERT((!dlg || !dlg->IsModal()));
   }
#endif

   // Delete the menus, since we will soon recreate them.
   // Rather oddly, the menus don't vanish as a result of doing this.
   {
      auto &window = static_cast<wxFrameEx&>( GetProjectFrame( project ) );
      wxWindowPtr<wxMenuBar> menuBar{ window.GetMenuBar() };
      window.DetachMenuBar();
      // menuBar gets deleted here
   }

   CommandManager::Get( project ).PurgeData();

   CreateMenusAndCommands(project);

   ModuleManager::Get().Dispatch(MenusRebuilt);
}

void MenuManager::OnUndoRedo( wxCommandEvent &evt )
{
   evt.Skip();
   ModifyUndoMenuItems( mProject );
   UpdateMenus();
}

namespace{
   using Predicates = std::vector< ReservedCommandFlag::Predicate >;
   Predicates &RegisteredPredicates()
   {
      static Predicates thePredicates;
      return thePredicates;
   }
   std::vector< CommandFlagOptions > &Options()
   {
      static std::vector< CommandFlagOptions > options;
      return options;
   }
}

ReservedCommandFlag::ReservedCommandFlag(
   const Predicate &predicate, const CommandFlagOptions &options )
{
   static size_t sNextReservedFlag = 0;
   // This will throw std::out_of_range if the constant NCommandFlags is too
   // small
   set( sNextReservedFlag++ );
   RegisteredPredicates().emplace_back( predicate );
   Options().emplace_back( options );
}

CommandFlag MenuManager::GetUpdateFlags( bool checkActive ) const
{
   // This method determines all of the flags that determine whether
   // certain menu items and commands should be enabled or disabled,
   // and returns them in a bitfield.  Note that if none of the flags
   // have changed, it's not necessary to even check for updates.

   // static variable, used to remember flags for next time.
   static CommandFlag lastFlags;

   CommandFlag flags, quickFlags;

   const auto &options = Options();
   size_t ii = 0;
   for ( const auto &predicate : RegisteredPredicates() ) {
      if ( options[ii].quickTest ) {
         quickFlags[ii] = true;
         if( predicate( mProject ) )
            flags[ii] = true;
      }
      ++ii;
   }

   if ( checkActive && !GetProjectFrame( mProject ).IsActive() )
      // quick 'short-circuit' return.
      flags = (lastFlags & ~quickFlags) | flags;
   else {
      ii = 0;
      for ( const auto &predicate : RegisteredPredicates() ) {
         if ( !options[ii].quickTest && predicate( mProject ) )
            flags[ii] = true;
         ++ii;
      }
   }

   lastFlags = flags;
   return flags;
}

void MenuManager::ModifyAllProjectToolbarMenus()
{
   for (auto pProject : AllProjects{}) {
      auto &project = *pProject;
      MenuManager::Get(project).ModifyToolbarMenus(project);
   }
}

void MenuManager::ModifyToolbarMenus(AudacityProject &project)
{
   // Refreshes can occur during shutdown and the toolmanager may already
   // be deleted, so protect against it.
   auto &toolManager = ToolManager::Get( project );

   auto &commandManager = CommandManager::Get( project );

   auto &settings = ProjectSettings::Get( project );

   commandManager.Check(wxT("ShowScrubbingTB"),
                         toolManager.IsVisible(ScrubbingBarID));
   commandManager.Check(wxT("ShowDeviceTB"),
                         toolManager.IsVisible(DeviceBarID));
   commandManager.Check(wxT("ShowEditTB"),
                         toolManager.IsVisible(EditBarID));
   commandManager.Check(wxT("ShowMeterTB"),
                         toolManager.IsVisible(MeterBarID));
   commandManager.Check(wxT("ShowRecordMeterTB"),
                         toolManager.IsVisible(RecordMeterBarID));
   commandManager.Check(wxT("ShowPlayMeterTB"),
                         toolManager.IsVisible(PlayMeterBarID));
   commandManager.Check(wxT("ShowMixerTB"),
                         toolManager.IsVisible(MixerBarID));
   commandManager.Check(wxT("ShowSelectionTB"),
                         toolManager.IsVisible(SelectionBarID));
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   commandManager.Check(wxT("ShowSpectralSelectionTB"),
                         toolManager.IsVisible(SpectralSelectionBarID));
#endif
   commandManager.Check(wxT("ShowToolsTB"),
                         toolManager.IsVisible(ToolsBarID));
   commandManager.Check(wxT("ShowTranscriptionTB"),
                         toolManager.IsVisible(TranscriptionBarID));
   commandManager.Check(wxT("ShowTransportTB"),
                         toolManager.IsVisible(TransportBarID));

   // Now, go through each toolbar, and call EnableDisableButtons()
   for (int i = 0; i < ToolBarCount; i++) {
      auto bar = toolManager.GetToolBar(i);
      if (bar)
         bar->EnableDisableButtons();
   }

   // These don't really belong here, but it's easier and especially so for
   // the Edit toolbar and the sync-lock menu item.
   bool active;
   gPrefs->Read(wxT("/AudioIO/SoundActivatedRecord"),&active, false);
   commandManager.Check(wxT("SoundActivation"), active);
#ifdef EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
   gPrefs->Read(wxT("/AudioIO/AutomatedInputLevelAdjustment"),&active, false);
   commandManager.Check(wxT("AutomatedInputLevelAdjustmentOnOff"), active);
#endif

   active = TracksPrefs::GetPinnedHeadPreference();
   commandManager.Check(wxT("PinnedHead"), active);

#ifdef EXPERIMENTAL_DA
   gPrefs->Read(wxT("/AudioIO/Duplex"),&active, false);
#else
   gPrefs->Read(wxT("/AudioIO/Duplex"),&active, true);
#endif
   commandManager.Check(wxT("Overdub"), active);
   gPrefs->Read(wxT("/AudioIO/SWPlaythrough"),&active, false);
   commandManager.Check(wxT("SWPlaythrough"), active);

   gPrefs->Read(wxT("/GUI/SyncLockTracks"), &active, false);
   settings.SetSyncLock(active);

   commandManager.Check(wxT("SyncLock"), active);
   gPrefs->Read(wxT("/GUI/TypeToCreateLabel"),&active, false);
   commandManager.Check(wxT("TypeToCreateLabel"), active);
}

namespace
{
   using MenuItemEnablers = std::vector<MenuItemEnabler>;
   MenuItemEnablers &Enablers()
   {
      static MenuItemEnablers enablers;
      return enablers;
   }
}

RegisteredMenuItemEnabler::RegisteredMenuItemEnabler(
   const MenuItemEnabler &enabler )
{
   Enablers().emplace_back( enabler );
}

// checkActive is a temporary hack that should be removed as soon as we
// get multiple effect preview working
void MenuManager::UpdateMenus( bool checkActive )
{
   auto &project = mProject;

   auto flags = GetUpdateFlags(checkActive);
   // Return from this function if nothing's changed since
   // the last time we were here.
   if (flags == mLastFlags)
      return;
   mLastFlags = flags;

   auto flags2 = flags;

   // We can enable some extra items if we have select-all-on-none.
   //EXPLAIN-ME: Why is this here rather than in GetUpdateFlags()?
   //ANSWER: Because flags2 is used in the menu enable/disable.
   //The effect still needs flags to determine whether it will need
   //to actually do the 'select all' to make the command valid.

   for ( const auto &enabler : Enablers() ) {
      auto actual = enabler.actualFlags();
      if (
         enabler.applicable( project ) && (flags & actual) == actual
      )
         flags2 |= enabler.possibleFlags();
   }

   auto &commandManager = CommandManager::Get( project );

   // With select-all-on-none, some items that we don't want enabled may have
   // been enabled, since we changed the flags.  Here we manually disable them.
   // 0 is grey out, 1 is Autoselect, 2 is Give warnings.
   commandManager.EnableUsingFlags(
      flags2, // the "lax" flags
      (mWhatIfNoSelection == 0 ? flags2 : flags) // the "strict" flags
   );

   MenuManager::ModifyToolbarMenus(project);
}

/// The following method moves to the previous track
/// selecting and unselecting depending if you are on the start of a
/// block or not.

void MenuCreator::RebuildAllMenuBars()
{
   for( auto p : AllProjects{} ) {
      MenuManager::Get(*p).RebuildMenuBar(*p);
#if defined(__WXGTK__)
      // Workaround for:
      //
      //   http://bugzilla.audacityteam.org/show_bug.cgi?id=458
      //
      // This workaround should be removed when Audacity updates to wxWidgets 3.x which has a fix.
      auto &window = GetProjectFrame( *p );
      wxRect r = window.GetRect();
      window.SetSize(wxSize(1,1));
      window.SetSize(r.GetSize());
#endif
   }
}

bool MenuManager::ReportIfActionNotAllowed(
   const TranslatableString & Name, CommandFlag & flags, CommandFlag flagsRqd )
{
   auto &project = mProject;
   bool bAllowed = TryToMakeActionAllowed( flags, flagsRqd );
   if( bAllowed )
      return true;
   auto &cm = CommandManager::Get( project );
   TellUserWhyDisallowed( Name, flags & flagsRqd, flagsRqd);
   return false;
}

/// Determines if flags for command are compatible with current state.
/// If not, then try some recovery action to make it so.
/// @return whether compatible or not after any actions taken.
bool MenuManager::TryToMakeActionAllowed(
   CommandFlag & flags, CommandFlag flagsRqd )
{
   auto &project = mProject;

   if( flags.none() )
      flags = GetUpdateFlags();

   // Visit the table of recovery actions
   auto &enablers = Enablers();
   auto iter = enablers.begin(), end = enablers.end();
   while ((flags & flagsRqd) != flagsRqd && iter != end) {
      const auto &enabler = *iter;
      auto actual = enabler.actualFlags();
      auto MissingFlags = (~flags & flagsRqd);
      if (
         // Do we have the right precondition?
         (flags & actual) == actual
      &&
         // Can we get the condition we need?
         (MissingFlags & enabler.possibleFlags()).any()
      ) {
         // Then try the function
         enabler.tryEnable( project, flagsRqd );
         flags = GetUpdateFlags();
      }
      ++iter;
   }
   return (flags & flagsRqd) == flagsRqd;
}

void MenuManager::TellUserWhyDisallowed(
   const TranslatableString & Name, CommandFlag flagsGot, CommandFlag flagsRequired )
{
   // The default string for 'reason' is a catch all.  I hope it won't ever be seen
   // and that we will get something more specific.
   auto reason = XO("There was a problem with your last action. If you think\nthis is a bug, please tell us exactly where it occurred.");
   // The default title string is 'Disallowed'.
   auto untranslatedTitle = XO("Disallowed");
   wxString helpPage;

   bool enableDefaultMessage = true;
   bool defaultMessage = true;

   auto doOption = [&](const CommandFlagOptions &options) {
      if ( options.message ) {
         reason = options.message( Name );
         defaultMessage = false;
         if ( !options.title.empty() )
            untranslatedTitle = options.title;
         helpPage = options.helpPage;
         return true;
      }
      else {
         enableDefaultMessage =
            enableDefaultMessage && options.enableDefaultMessage;
         return false;
      }
   };

   const auto &alloptions = Options();
   auto missingFlags = flagsRequired & ~flagsGot;

   // Find greatest priority
   unsigned priority = 0;
   for ( const auto &options : alloptions )
      priority = std::max( priority, options.priority );

   // Visit all unsatisfied conditions' options, by descending priority,
   // stopping when we find a message
   ++priority;
   while( priority-- ) {
      size_t ii = 0;
      for ( const auto &options : alloptions ) {
         if (
            priority == options.priority
         &&
            missingFlags[ii]
         &&
            doOption( options ) )
            goto done;

         ++ii;
      }
   }
   done:

   if (
      // didn't find a message
      defaultMessage
   &&
      // did find a condition that suppresses the default message
      !enableDefaultMessage
   )
      return;

   // Does not have the warning icon...
   ShowErrorDialog(
      NULL,
      untranslatedTitle,
      reason,
      helpPage);
}
