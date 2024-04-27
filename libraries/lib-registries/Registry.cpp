/**********************************************************************

Audacity: A Digital Audio Editor

Registry.cpp

Paul Licameli split from Menus.cpp

**********************************************************************/

#include "Registry.h"

#include <unordered_set>

#include <wx/log.h>

#include "BasicUI.h"

using namespace Registry;
using namespace Registry::detail;

namespace {
struct ItemOrdering;

//! Used only internally
struct PlaceHolder : GroupItemBase {
   PlaceHolder(const Identifier &identifier, Ordering ordering)
      : GroupItemBase{ identifier }
      , ordering{ ordering == Strong ? Weak : ordering }
   {}
   ~PlaceHolder() = default;
   Ordering GetOrdering() const override
   {
      return ordering;
   }
   Ordering ordering;
};

struct CollectedItems
{
   //! @invariant `!MergeLater() || dynamic_cast<GroupItemBase *>(VisitNow())`
   class Item {
   public:
      Item(BaseItem *visitNow, OrderingHint hint = {})
         : mVisitNow{ visitNow }, mHint{ hint }
      {
         assert(CheckInvariant());
      }

      BaseItem *VisitNow() const { return mVisitNow; }
      //! @pre !MergeLater() || dynamic_cast<GroupItemBase *>(visitNow)
      void SetVisitNow(BaseItem *visitNow)
      {
         assert(!MergeLater() || dynamic_cast<GroupItemBase *>(visitNow));
         mVisitNow = visitNow;
         assert(CheckInvariant());
      }

      const OrderingHint &Hint() const { return mHint; }

      PlaceHolder *MergeLater() const { return mMergeLater; }
      //! @pre dynamic_cast<GroupItemBase *>(VisitNow())
      void SetMergeLater(PlaceHolder &mergeLater)
      {
         assert(dynamic_cast<GroupItemBase *>(VisitNow()));
         mMergeLater = &mergeLater;
         assert(CheckInvariant());
      }
   private:
      bool CheckInvariant() const
      {
         return !MergeLater() || dynamic_cast<GroupItemBase *>(VisitNow());
      }

      // Predefined, or merged from registry already:
      BaseItem *mVisitNow{};

      // Ordering hint for the merged item:
      OrderingHint mHint{};

      // Non-null only when visitNow is a group item
      // Corresponding item from the registry, its sub-items to be merged:
      PlaceHolder *mMergeLater{};
   };
   std::vector< BaseItemSharedPtr > &computedItems;
   std::vector< Item > items;
   std::unordered_set<Identifier> mResolvedConflicts;

   // A linear search.  Smarter search may not be worth the effort.
   using Iterator = decltype( items )::iterator;
   auto Find( const Identifier &name ) -> Iterator
   {
      auto end = items.end();
      return name.empty()
         ? end
         : std::find_if( items.begin(), end,
            [&]( const Item& item ){
               return name == item.VisitNow()->name; } );
   }

   auto InsertNewItemUsingPreferences(
      const ItemOrdering &itemOrdering, BaseItem *pItem ) -> bool;

   auto InsertNewItemUsingHint( ItemOrdering &itemOrdering,
      BaseItem *pItem, const OrderingHint &hint, size_t endItemsCount,
      bool force )
         -> bool;

   auto MergeLater(Item &found, const Identifier &name,
      GroupItemBase::Ordering ordering) -> PlaceHolder *;

   void SubordinateSingleItem(Item &found, BaseItem *pItem);

   void SubordinateMultipleItems(Item &found, GroupItemBase &items);

   auto MergeWithExistingItem(const ItemOrdering &itemOrdering,
      BaseItem *pItem, OrderingHint::ConflictResolutionPolicy policy) -> bool;

   using NewItem = std::pair< BaseItem*, OrderingHint >;
   using NewItems = std::vector< NewItem >;

   auto InsertFirstNamedItem(ItemOrdering &itemOrdering,
      NewItem &item, size_t endItemsCount, bool force)
         -> bool;

   auto MergeLikeNamedItems(const ItemOrdering &itemOrdering,
      NewItems::const_iterator left, NewItems::const_iterator right) -> void;

   void MergeItemsAscendingNamesPass(ItemOrdering &itemOrdering,
      NewItems &newItems, int iPass, size_t endItemsCount, bool force);

   void MergeItemsDescendingNamesPass(ItemOrdering &itemOrdering,
      NewItems &newItems, int iPass, size_t endItemsCount, bool force);

   void MergeItems(ItemOrdering &itemOrdering,
      const GroupItemBase &toMerge, const OrderingHint &hint,
      void *pComputedItemContext);
};

// When a computed or indirect item, or nameless grouping, specifies a hint and
// the subordinate does not, propagate the hint.
// Likewise for conflict resolution.
OrderingHint ChooseHint(BaseItem *delegate, const OrderingHint &hint)
{
   auto result =
      !delegate || delegate->orderingHint.type == OrderingHint::Unspecified
      ? hint
      : delegate->orderingHint;
   auto policy =
      !delegate || delegate->orderingHint.policy == OrderingHint::None
      ? hint.policy
      : delegate->orderingHint.policy;
   result.policy = policy;
   return result;
}

// "Collection" of items is the first pass of visitation, and resolves
// delegation and delayed computation and splices anonymous group nodes.
// This first pass is done at each group, starting with a top-level group.
// This pass does not descend to the leaves.  Rather, the visitation passes
// alternate as the entire tree is recursively visited.

// forward declaration for mutually recursive functions
void CollectItem(CollectedItems &collection, BaseItem *Item,
   const OrderingHint &hint, void *pComputedItemContext);
void CollectItems(CollectedItems &collection, const GroupItemBase &items,
   const OrderingHint &hint, void *pComputedItemContext)
{
   for ( auto &item : items )
      CollectItem(collection, item.get(),
         ChooseHint(item.get(), hint), pComputedItemContext);
}
void CollectItem(CollectedItems &collection,
   BaseItem *pItem, const OrderingHint &hint, void *pComputedItemContext)
{
   if (!pItem)
      return;

   using namespace Registry;
   if (const auto pIndirect =
       dynamic_cast<IndirectItemBase*>(pItem)) {
      auto delegate = pIndirect->ptr.get();
      if (delegate)
         // recursion
         CollectItem(collection, delegate,
            ChooseHint(delegate, ChooseHint(pIndirect, hint)),
            pComputedItemContext);
   }
   else
   if (const auto pComputed =
       dynamic_cast<ComputedItemBase*>(pItem)) {
      auto result = pComputed->factory(pComputedItemContext);
      if (result) {
         // Guarantee long enough lifetime of the result
         collection.computedItems.push_back( result );
         // recursion
         CollectItem(collection, result.get(),
            ChooseHint(result.get(), ChooseHint(pComputed, hint)),
            pComputedItemContext );
      }
   }
   else
   if (auto pGroup = dynamic_cast<GroupItemBase*>(pItem)) {
      if (pGroup->GetOrdering() == GroupItemBase::Anonymous)
         // anonymous grouping item is transparent to path calculations
         // collect group members now
         // recursion
         CollectItems(collection, *pGroup,
            ChooseHint(pGroup, hint), pComputedItemContext);
      else
         // all other group items
         // defer collection of members until collecting at next lower level
         collection.items.push_back({ pItem, hint });
   }
   else {
      wxASSERT( dynamic_cast<SingleItem*>(pItem) );
      // common to all single items
      collection.items.push_back({ pItem, hint });
   }
}

std::unordered_set< wxString > sBadPaths;
void BadPath(
  const TranslatableString &format, const wxString &key, const Identifier &name )
{
  // Warn, but not more than once in a session for each bad path
  auto badPath = key + '/' + name.GET();
  if ( sBadPaths.insert( badPath ).second ) {
     auto msg = TranslatableString{ format }.Format( badPath );
     // debug message
     wxLogDebug( msg.Debug() );
#ifdef IS_ALPHA
     // user-visible message
     BasicUI::ShowMessageBox( msg );
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

using Path = VisitBase::Path;

// This structure caches left-to-right ordering of an internal node of a
// registry, as determined by config file, and does linear lookup of names
struct ItemOrdering {
   wxString key;

   ItemOrdering(const Path &path)
   {
      // The set of path names determines only an unordered tree.
      // We want an ordering of the tree that is stable across runs.
      // The last used ordering for this node can be found in preferences at
      // this key:
      wxArrayString strings;
      for (const auto &id : path)
         strings.push_back( id.GET() );
      // Components of the path are assumed not to contain '/'
      key = '/' + ::wxJoin( strings, '/', '\0' );
   }

   // Retrieve the old ordering on demand, if needed to merge something.
   mutable bool gotOrdering = false;
   mutable wxString strValue;
   mutable wxArrayString ordering;

   // Insert the component at the given index and maintain uniqueness
   //! @pre `index <= ordering.size()`
   //! @pre ordering contains unique names
   void Insert(const wxString &name, size_t index) {
      assert(index <= ordering.size());
      auto begin = ordering.begin();
      const auto iter = ordering.insert(begin + index, name);
      begin = ordering.begin(); // beware relocation
      auto found = std::find(begin, iter, name);
      if (found != iter)
         ordering.erase(found);
      else {
         const auto end = ordering.end();
         found = std::find(iter + 1, end, name);
         if (found != end)
            ordering.erase(found);
      }
   }

   // Establish uniqueness of names in the sequence loaded from preferences
   // A quadratic time algorithm on what is probably a very short sequence
   static void Uniquify(wxArrayString &names) {
      for (size_t ii = 0; ii < names.size();) {
         const auto begin = names.begin(),
            iter = begin + ii,
            iter2 = std::find(begin, iter, *iter);
         if (iter != iter2)
            names.erase(iter2);
         else
            ++ii;
      }
   }

   auto Get() const -> const wxArrayString & {
      if (!gotOrdering) {
         gPrefs->Read(key, &strValue);
         ordering = ::wxSplit( strValue, ',' );
         Uniquify(ordering);
         gotOrdering = true;
      }
      return ordering;
   };

   int Find(Identifier component) {
      auto &components = Get();
      const auto begin = components.begin(),
         end = components.end(),
         found = std::find(begin, end, component.GET());
      if (found == end)
         return -1;
      return found - begin;
   }

   bool Save() const {
      // Remember the new ordering, if there was any need to use the old.
      // This makes a side effect in preferences.
      if (gotOrdering) {
         wxString newValue;
         for (const auto &name : ordering) {
            if (!name.empty())
               newValue += newValue.empty()
                  ? name
                  : ',' + name;
         }
         if (newValue != strValue) {
            gPrefs->Write(key, newValue);
            strValue.swap(newValue);
            return true;
         }
      }
      return false;
   }
};

// For each group node, this is called only in the first pass of merging of
// items.  It might fail to place an item in the first visitation of a
// registry, but then succeed in later visitations in the same or later
// runs of the program, because of persistent side-effects on the
// preferences done at the very end of the visitation.
// This function will succeed whenever the item's name is in the ordering.
auto CollectedItems::InsertNewItemUsingPreferences(
   const ItemOrdering &itemOrdering, BaseItem *pItem)
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

   auto &name = pItem->name;
   if ( !name.empty() ) {
      // Check saved ordering first, and rebuild that as well as is possible
      auto &ordering = itemOrdering.Get();
      auto begin2 = ordering.begin(), end2 = ordering.end(),
         found2 = std::find( begin2, end2, name );
      if ( found2 != end2 ) {
         // Insert the item.  This procedure depends on the items later in the
         // ordering (when present in the program run) being inserted already.
         auto insertPoint = items.end();
         // Find the next name in the saved ordering that is known already
         // in the collection.
         // (Maybe preferences mention nonexistent items.  Assume all existing
         // ones later in preference ordering have been placed.)
         while ( ++found2 != end2 ) {
            auto known = Find( *found2 );
            if ( known != insertPoint ) {
               insertPoint = known;
               break;
            }
         }
         // Hints no longer matter:
         items.insert(insertPoint, { pItem });
         return true;
      }
   }

   return false;
}

// For each group node, this may be called in the second and later passes
// of merging of items.  It will succeed if force is true.
auto CollectedItems::InsertNewItemUsingHint( ItemOrdering &itemOrdering,
   BaseItem *pItem, const OrderingHint &hint, size_t endItemsCount,
   bool force ) -> bool
{
   auto begin = items.begin(), end = items.end(),
      insertPoint = end - endItemsCount;
   auto &ordering = itemOrdering.ordering;
   auto orderingInsertPoint = ordering.end();
   if (!itemOrdering.ordering.empty())
      // Previous calls to this function for the present group node and the
      // End hint type will have inserted all such names at the end of
      // itemOrdering
      orderingInsertPoint -= endItemsCount;

   // pItem should have a name; if not, ignore the hint, and put it at the
   // default place, but only if in the final pass.
   if ( pItem->name.empty() ) {
      if ( !force )
         return false;
   }
   else {
      switch ( hint.type ) {
         case OrderingHint::Before:
         case OrderingHint::After: {
            // Default to the end if the name is not found.
            auto found = Find( hint.name );
            if ( found == end ) {
               if ( !force )
                  return false;
               else {
                  insertPoint = found;
                  orderingInsertPoint = ordering.end();
               }
            }
            else {
               insertPoint = found;
               orderingInsertPoint = std::find(
                  ordering.begin(), ordering.end(), hint.name);
               if ( hint.type == OrderingHint::After ) {
                  ++insertPoint;
                  if (orderingInsertPoint != ordering.end())
                     ++orderingInsertPoint;
               }
            }
            break;
         }
         case OrderingHint::Begin:
            insertPoint = begin;
            orderingInsertPoint = ordering.begin();
            break;
         case OrderingHint::End:
            orderingInsertPoint = ordering.end();
            insertPoint = end;
            break;
         case OrderingHint::Unspecified:
         default:
            if (!force)
               return false;
            break;
      }
   }

   // Insert the item; the hint has been used and no longer matters
   items.insert(insertPoint, { pItem });

   // update the ordering preference too, so as not to lose any information
   // in it, in case of named but not yet loaded items mentioned in the
   // preferences
   if (!itemOrdering.ordering.empty() && !pItem->name.empty()) {
      itemOrdering.Insert(pItem->name.GET(),
         orderingInsertPoint - itemOrdering.ordering.begin());
   }
   return true;
}

// Create, on demand, a temporary transparent group item
//! @pre `found.VisitNow()` points to a GroupItemBase
auto CollectedItems::MergeLater(Item &found, const Identifier &name,
   GroupItemBase::Ordering ordering) -> PlaceHolder *
{
   assert(dynamic_cast<GroupItemBase*>(found.VisitNow()));
   // Therefore found.SetMergeLater can be called

   auto subGroup = found.MergeLater();
   if (!subGroup) {
      auto newGroup = std::make_shared<PlaceHolder>(name, ordering);
      computedItems.push_back(newGroup);
      subGroup = newGroup.get();
      found.SetMergeLater(*subGroup);
   }
   return subGroup;
}

//! @pre `found.VisitNow()` points to a GroupItemBase
void CollectedItems::SubordinateSingleItem(Item &found, BaseItem *pItem)
{
   assert(dynamic_cast<GroupItemBase*>(found.VisitNow()));
   MergeLater(found, pItem->name, GroupItemBase::Weak)->push_back(
      std::make_unique<IndirectItemBase>(
         // shared pointer with vacuous deleter
         std::shared_ptr<BaseItem>(pItem, [](void*){})));
}

//! @pre `found.VisitNow()` points to a GroupItemBase
void CollectedItems::SubordinateMultipleItems(Item &found, GroupItemBase &items)
{
   assert(dynamic_cast<GroupItemBase*>(found.VisitNow()));
   auto subGroup = MergeLater(found, items.name, items.GetOrdering());
   for (const auto &pItem : items)
      subGroup->push_back(std::make_unique<IndirectItemBase>(
         // shared pointer with vacuous deleter
         std::shared_ptr<BaseItem>(pItem.get(), [](void*){})));
}

auto CollectedItems::MergeWithExistingItem(const ItemOrdering &itemOrdering,
   BaseItem *pItem, OrderingHint::ConflictResolutionPolicy policy) -> bool
{
   // Assume no null pointers remain after CollectItems:
   const auto &name = pItem->name;
   const auto found = Find( name );
   if (found != items.end()) {
      // Collision of names between collection and registry!
      // There are 2 * 2 = 4 cases, as each of the two are group items or
      // not.
      const auto pRegistryGroup = dynamic_cast<GroupItemBase *>(pItem);
      if (const auto pCollectionGroup =
         dynamic_cast<GroupItemBase *>(found->VisitNow()))
      {
         // Therefore preconditions of Subordinate calls below are satisfied
         if (pRegistryGroup) {
            // This is the expected case of collision.
            // Subordinate items from one of the groups will be merged in
            // another call to MergeItems at a lower level of path.
            // Note, however, that at most one of the two should be a
            // strongly ordered item; if not, we must lose the extra
            // information carried by one of them.
            bool pCollectionGrouping =
               (pCollectionGroup->GetOrdering() != GroupItemBase::Strong);
            auto pRegistryGrouping =
               (pRegistryGroup->GetOrdering() != GroupItemBase::Strong);
            if ( !(pCollectionGrouping || pRegistryGrouping) )
               ReportGroupGroupCollision( itemOrdering.key, name );

            if ( pCollectionGrouping && !pRegistryGrouping ) {
               // Swap their roles
               found->SetVisitNow(pRegistryGroup);
               SubordinateMultipleItems(*found, *pCollectionGroup);
            }
            else
               SubordinateMultipleItems(*found, *pRegistryGroup);
         }
         else {
            // Registered non-group item collides with a previously defined
            // group.
            // Resolve this by subordinating the non-group item below
            // that group.
            SubordinateSingleItem(*found, pItem);
         }
      }
      else {
         // Because of invariant of *found:
         assert(!found->MergeLater()); // So SetVisitNow below is safe
         if (pRegistryGroup) {
            // Subordinate the previously merged single item below the
            // newly merged group.
            // In case the name occurred in two different static registries,
            // the final merge is the same, no matter which is treated first.
            auto demoted = found->VisitNow();
            found->SetVisitNow(pRegistryGroup);
            // Now precondition is satisfied
            SubordinateSingleItem(*found, demoted);
         }
         else {
            // Collision of non-group items.
            // Try conflict resolution.
            switch( policy ) {
            case OrderingHint::Ignore:
               break;
            case OrderingHint::Replace:
               // At most one item with this policy may be substituted
               if (mResolvedConflicts.insert(name).second) {
                  found->SetVisitNow(pItem);
                  break;
               }
               [[fallthrough]] ;
            case OrderingHint::Error:
            default:
               // Unresolved collision of non-group items is the worst case!
               // The later-registered item is lost.
               // Which one you lose might be unpredictable when both originate
               // from static registries.
               ReportItemItemCollision( itemOrdering.key, name );
               break;
            }
         }
      }
      return true;
   }
   else
      // A name is registered that is not known in the collection.
      return false;
}

auto CollectedItems::InsertFirstNamedItem(ItemOrdering &itemOrdering,
   NewItem &item, size_t endItemsCount, bool force)
   -> bool
{
   // Try to place the first item of the range.
   // If such an item is a group, then we always retain the kind of
   // grouping that was registered.  (Which doesn't always happen when
   // there is name collision in MergeWithExistingItem.)

   // Maybe it fails in this pass, because a placement refers to some
   // other name that has not yet been placed.
   bool success = InsertNewItemUsingHint(itemOrdering,
      item.first, item.second, endItemsCount, force);
   // The function promises to succeed when force is true.
   assert(!force || success);

   return success;
}

void CollectedItems::MergeLikeNamedItems(const ItemOrdering &itemOrdering,
   NewItems::const_iterator left, NewItems::const_iterator right)
{
   // Resolve collisions among remaining like-named items.
   auto iter = left;
   auto &item = *iter;
   auto pItem = item.first;
   const auto &hint = item.second;
   ++iter;
   while ( iter != right ) {
      if ( iter->second.type != OrderingHint::Unspecified &&
          !( iter->second == hint ) ) {
         // A diagnostic message sometimes
         ReportConflictingPlacements( itemOrdering.key, pItem->name );
      }
      // Re-invoke MergeWithExistingItem for this item, which is known
      // to have a name collision, so ignore the return value.
      MergeWithExistingItem(itemOrdering, iter->first, iter->second.policy );
      ++iter;
   }
}

inline bool MajorComp(
   const CollectedItems::NewItem &a, const CollectedItems::NewItem &b) {
   // Descending sort!
   return a.first->name > b.first->name;
};
inline bool MinorComp(
   const CollectedItems::NewItem &a, const CollectedItems::NewItem &b){
   // Sort by hint type.
   // This sorts items with unspecified hints last.
   return a.second < b.second;
};
inline bool Comp(
   const CollectedItems::NewItem &a, const CollectedItems::NewItem &b){
   if ( MajorComp( a, b ) )
      return true;
   if ( MajorComp( b, a ) )
      return false;
   return MinorComp( a, b );
};

void CollectedItems::MergeItemsAscendingNamesPass(ItemOrdering &itemOrdering,
   NewItems &newItems, const int iPass, size_t endItemsCount, bool force)
{
   // Inner loop over ranges of like-named items.
   auto rright = newItems.rbegin();
   auto rend = newItems.rend();
   while ( rright != rend ) {
      // Find the range
      using namespace std::placeholders;
      auto rleft = std::find_if(
         rright + 1, rend, std::bind( MajorComp, _1, *rright ) );

      auto left = rleft.base(), right = rright.base();

      bool success = (left->second.type == iPass) &&
         InsertFirstNamedItem(itemOrdering, *left, endItemsCount, force);

      if (success)
         MergeLikeNamedItems(itemOrdering, left, right);

      if (success) {
         auto diff = rend - rleft;
         newItems.erase( left, right );
         rend = newItems.rend();
         rleft = rend - diff;
      }
      rright = rleft;
   }
}

void CollectedItems::MergeItemsDescendingNamesPass(ItemOrdering &itemOrdering,
   NewItems &newItems, const int iPass, size_t endItemsCount, bool force)
{
   // Inner loop over ranges of like-named items.
   auto left = newItems.begin();
   while ( left != newItems.end() ) {
      // Find the range
      using namespace std::placeholders;
      auto right = std::find_if(
         left + 1, newItems.end(), std::bind( MajorComp, *left, _1 ) );

      bool success = (left->second.type == iPass) &&
         InsertFirstNamedItem(itemOrdering, *left, endItemsCount, force );

      if (success)
         MergeLikeNamedItems(itemOrdering, left, right);

      if (success)
         left = newItems.erase( left, right );
      else
         left = right;
   }
};

void CollectedItems::MergeItems(ItemOrdering &itemOrdering,
   const GroupItemBase &toMerge, const OrderingHint &hint,
   void *pComputedItemContext)
{
   NewItems newItems;

   {
      // First do expansion of nameless groupings, and caching of computed
      // items, just as for the previously collected items.
      CollectedItems newCollection{ computedItems };
      CollectItems(newCollection, toMerge, hint, pComputedItemContext);

      // Try to merge each, resolving name collisions with items already in the
      // tree, and collecting those with names that don't collide.
      for (const auto &item : newCollection.items)
         if (!MergeWithExistingItem(itemOrdering,
            item.VisitNow(), item.Hint().policy))
             newItems.push_back({ item.VisitNow(), item.Hint() });
   }

   // Choose placements for items with NEW names.
   auto begin = newItems.begin(), end = newItems.end();

   // Segregate the ones that are placed by preferences.
   const auto middle = std::partition(begin, end, [&](const NewItem &item){
      return -1 == itemOrdering.Find(item.first->name); });

   // Sort those according to their (descending) place in the preferences, to
   // satisfy the assumptions made in InsertNewItemUsingPreferences
   sort(middle, end, [&](const NewItem &a, const NewItem &b){ return
      itemOrdering.Find(a.first->name) > itemOrdering.Find(b.first->name); });

   // Process them
   for (auto iter = middle; iter != end;) {
      auto pItem = iter->first;
      auto &name = pItem->name;
      bool success = InsertNewItemUsingPreferences(itemOrdering, pItem);
      // Will succeed because the name is in the ordering
      assert(success);

      auto right = iter + 1;
      while (right != end && right->first->name == name)
         ++right;
      MergeLikeNamedItems(itemOrdering, iter, right);
      iter = right;
   }

   end = newItems.erase(middle, end);

   // Sort others so that like named items are together, and for the same name,
   // items with more specific ordering hints come earlier.
   sort(begin, end, Comp);

   // Outer loop over trial passes.
   int iPass = 0;
   bool force = false;
   size_t oldSize = newItems.size();
   int endItemsCount = 0;
   auto prevSize = oldSize;
   while( !newItems.empty() )
   {
      // If several items have the same hint, we try to preserve the sort by
      // name (an internal identifier, not necessarily user visible), just to
      // have some determinacy.  That requires passing one or the other way
      // over newItems.
      bool descending =
         ( iPass == OrderingHint::After || iPass == OrderingHint::Begin );

      if ( descending )
         MergeItemsDescendingNamesPass(itemOrdering,
            newItems, iPass, endItemsCount, force);
      else
         MergeItemsAscendingNamesPass(itemOrdering,
            newItems, iPass, endItemsCount, force);

      auto newSize = newItems.size();

      if (iPass == OrderingHint::End)
         // Remember how many were placed; so that default placement is
         // before all explicit End items, but after other items
         endItemsCount += prevSize - newSize;
      assert(endItemsCount >= 0);

      ++iPass;
      if ( iPass == OrderingHint::Unspecified ) {
         // Don't place the Unspecified until we have passed through the other
         // ordering hint types with no further progress in placement of
         // other items, and then once more, forcing placement with Before and
         // After hints that reference a nonexistent item.
         if ( !force ) {
            // Begin and End placements always succeed, so don't retry them.
            iPass = OrderingHint::Before;
            // Retry placement of Before and After items, in case they
            // depended on placement of other items that were not yet placed.
            force = (oldSize == newSize);
            oldSize = newSize;
         }
      }

      prevSize = newSize;
   }
}

// forward declaration for mutually recursive functions
void VisitItem(VisitBase &state,
   const VisitorBase &visitor, const BaseItem *pItem,
   const GroupItemBase *pRegistry, const OrderingHint &hint,
   bool &doFlush, void *pComputedItemContext);
void VisitItems(VisitBase &state,
   const VisitorBase &visitor, const GroupItemBase &group,
   const GroupItemBase *pRegistry, const OrderingHint &hint,
   bool &doFlush, void *pComputedItemContext)
{
   // Make a NEW collection for this subtree, sharing the memo cache
   CollectedItems newCollection{ state.computedItems };

   // Gather items at this level
   // (The ordering hint is irrelevant when not merging items in)
   CollectItems(newCollection, group, {},
      pComputedItemContext);

   state.path.push_back(group.name.GET());

   // Merge with the registry
   if (pRegistry) {
      ItemOrdering itemOrdering{ state.path };
      newCollection.MergeItems(itemOrdering, *pRegistry, hint,
         pComputedItemContext);
      doFlush = itemOrdering.Save() || doFlush;
   }

   // Now visit them
   for (const auto &item : newCollection.items)
      VisitItem(state, visitor, item.VisitNow(), item.MergeLater(), item.Hint(),
         doFlush, pComputedItemContext);

   state.path.pop_back();
}
void VisitItem(VisitBase &state,
   const VisitorBase &visitor, const BaseItem *pItem,
   const GroupItemBase *pRegistry, const OrderingHint &hint,
   bool &doFlush, void *pComputedItemContext)
{
   if (!pItem)
      return;

   if (const auto pSingle =
       dynamic_cast<const SingleItem*>(pItem)) {
      // Topmost call won't come to this branch.
      // This is a recursive call that passed members of CollectecItems::Item
      // So the invariant property of CollectedItems::Item proves:b
      assert(!pRegistry);
      visitor.Visit(*pSingle, state.path);
   }
   else
   if (const auto pGroup =
       dynamic_cast<const GroupItemBase*>(pItem)) {
      visitor.BeginGroup(*pGroup, state.path);
      // recursion
      VisitItems(state, visitor, *pGroup, pRegistry, hint, doFlush,
         pComputedItemContext);
      visitor.EndGroup(*pGroup, state.path);
   }
   else
      wxASSERT( false );
}

}

namespace Registry {

EmptyContext EmptyContext::Instance;

BaseItem::~BaseItem() {}

IndirectItemBase::~IndirectItemBase() {}

ComputedItemBase::~ComputedItemBase() {}

SingleItem::~SingleItem() {}

GroupItemBase::~GroupItemBase() {}
auto GroupItemBase::GetOrdering() const -> Ordering { return Strong; }

VisitorBase::~VisitorBase() = default;

void detail::VisitBase::DoVisit(const VisitorBase &visitor,
   const GroupItemBase *pTopItem,
   const GroupItemBase *pRegistry, void *pComputedItemContext)
{
   assert(pComputedItemContext);
   computedItems.clear();
   path.clear();
   bool doFlush = false;
   VisitItem(*this, visitor, pTopItem,
      pRegistry, pRegistry->orderingHint, doFlush, pComputedItemContext);
   // Flush any writes done by MergeItems()
   if (doFlush)
      gPrefs->Flush();
}

OrderingPreferenceInitializer::OrderingPreferenceInitializer(
   Literal root, Pairs pairs )
   : mPairs{ std::move( pairs ) }
   , mRoot{ root }
{
   (*this)();
}

void OrderingPreferenceInitializer::operator () ()
{
   // Default, as needed, any registry items that specify left-to-right
   // orderings of internal nodes.
   bool doFlush = false;
   for (const auto &pair : mPairs) {
      const auto key = wxString{'/'} + mRoot + pair.first;
      if ( gPrefs->Read(key).empty() ) {
         gPrefs->Write( key, pair.second );
         doFlush = true;
      }
   }
   
   if (doFlush)
      gPrefs->Flush();
}

void detail::RegisterItem(GroupItemBase &registry, const Placement &placement,
   BaseItemPtr pItem)
{
   // Since registration determines only an unordered tree of menu items,
   // we can sort children of each node lexicographically for our convenience.
   std::vector<BaseItemPtr> *pItems{};
   struct Comparator {
      bool operator()
         ( const Identifier &component, const BaseItemPtr& pItem ) const {
            return component < pItem->name; }
      bool operator()
         ( const BaseItemPtr& pItem, const Identifier &component ) const {
            return pItem->name < component; }
   };
   auto find = [&pItems]( const Identifier &component ){ return std::equal_range(
      pItems->begin(), pItems->end(), component, Comparator() ); };

   auto pNode = &registry;
   pItems = &pNode->items;

   const auto pathComponents = ::wxSplit( placement.path, '/' );
   auto pComponent = pathComponents.begin(), end = pathComponents.end();

   // Descend the registry hierarchy, while groups matching the path components
   // can be found
   auto debugPath = wxString{'/'} + registry.name.GET();
   while ( pComponent != end ) {
      const auto &pathComponent = *pComponent;

      // Try to find an item already present that is a group item with the
      // same name; we don't care which if there is more than one.
      const auto range = find( pathComponent );
      const auto iter2 = std::find_if( range.first, range.second,
         [](const BaseItemPtr &pItem){
            return dynamic_cast< GroupItemBase* >( pItem.get() ); } );

      if ( iter2 != range.second ) {
         // A matching group in the registry, so descend
         pNode = static_cast< GroupItemBase* >( iter2->get() );
         pItems = &pNode->items;
         debugPath += '/' + pathComponent;
         ++pComponent;
      }
      else
         // Insert at this level;
         // If there are no more path components, and a name collision of
         // the added item with something already in the registry, don't resolve
         // it yet in this function, but see MergeItems().
         break;
   }

   // Create path group items for remaining components
   while ( pComponent != end ) {
      auto newNode =
         std::make_unique<PlaceHolder>(*pComponent, GroupItemBase::Weak);
      pNode = newNode.get();
      pItems->insert( find( pNode->name ).second, std::move( newNode ) );
      pItems = &pNode->items;
      ++pComponent;
   }

   // Remember the hint, to be used later in merging.
   pItem->orderingHint = placement.hint;

   // Now insert the item.
   pItems->insert( find( pItem->name ).second, std::move( pItem ) );
}

template struct GroupItem<DefaultTraits>;
}
