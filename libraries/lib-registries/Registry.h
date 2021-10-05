/**********************************************************************

Audacity: A Digital Audio Editor

Registry.h

Paul Licameli split from CommandManager.h

**********************************************************************/

#ifndef __AUDACITY_REGISTRY__
#define __AUDACITY_REGISTRY__

#include "Prefs.h"

// Define classes and functions that associate parts of the user interface
// with path names
namespace Registry {
   // Items in the registry form an unordered tree, but each may also describe a
   // desired insertion point among its peers.  The request might not be honored
   // (as when the other name is not found, or when more than one item requests
   // the same ordering), but this is not treated as an error.
   struct OrderingHint
   {
      // The default Unspecified hint is just like End, except that in case the
      // item is delegated to (by a SharedItem, ComputedItem, or nameless
      // transparent group), the delegating item's hint will be used instead
      enum Type : int {
         Before, After,
         Begin, End,
         Unspecified // keep this last
      } type{ Unspecified };

      // name of some other BaseItem; significant only when type is Before or
      // After:
      Identifier name;

      OrderingHint() {}
      OrderingHint( Type type_, const wxString &name_ = {} )
         : type(type_), name(name_) {}

      bool operator == ( const OrderingHint &other ) const
      { return name == other.name && type == other.type; }

      bool operator < ( const OrderingHint &other ) const
      {
         // This sorts unspecified placements later
         return std::make_pair( type, name ) <
            std::make_pair( other.type, other.name );
      }
   };

   // TODO C++17: maybe use std::variant (discriminated unions) to achieve
   // polymorphism by other means, not needing unique_ptr and dynamic_cast
   // and using less heap.
   // Most items in the table will be the large ones describing commands, so the
   // waste of space in unions for separators and sub-menus should not be
   // large.
   struct REGISTRIES_API BaseItem {
      // declare at least one virtual function so dynamic_cast will work
      explicit
      BaseItem( const Identifier &internalName )
         : name{ internalName }
      {}
      virtual ~BaseItem();

      const Identifier name;

      OrderingHint orderingHint;
   };
   using BaseItemPtr = std::unique_ptr<BaseItem>;
   using BaseItemSharedPtr = std::shared_ptr<BaseItem>;
   using BaseItemPtrs = std::vector<BaseItemPtr>;

   class Visitor;
   

   // An item that delegates to another held in a shared pointer; this allows
   // static tables of items to be computed once and reused
   // The name of the delegate is significant for path calculations, but the
   // SharedItem's ordering hint is used if the delegate has none
   struct REGISTRIES_API SharedItem final : BaseItem {
      explicit SharedItem( const BaseItemSharedPtr &ptr_ )
         : BaseItem{ wxEmptyString }
         , ptr{ ptr_ }
      {}
      ~SharedItem() override;

      BaseItemSharedPtr ptr;
   };

   // A convenience function
   inline std::unique_ptr<SharedItem> Shared( const BaseItemSharedPtr &ptr )
      { return std::make_unique<SharedItem>( ptr ); }

   // An item that computes some other item to substitute for it, each time
   // the ComputedItem is visited
   // The name of the substitute is significant for path calculations, but the
   // ComputedItem's ordering hint is used if the substitute has none
   struct REGISTRIES_API ComputedItem final : BaseItem {
      // The type of functions that generate descriptions of items.
      // Return type is a shared_ptr to let the function decide whether to
      // recycle the object or rebuild it on demand each time.
      // Return value from the factory may be null
      template< typename VisitorType >
      using Factory = std::function< BaseItemSharedPtr( VisitorType & ) >;

      using DefaultVisitor = Visitor;

      explicit ComputedItem( const Factory< DefaultVisitor > &factory_ )
         : BaseItem( wxEmptyString )
         , factory{ factory_ }
      {}
      ~ComputedItem() override;

      Factory< DefaultVisitor > factory;
   };

   // Common abstract base class for items that are not groups
   struct REGISTRIES_API SingleItem : BaseItem {
      using BaseItem::BaseItem;
      ~SingleItem() override = 0;
   };

   // Common abstract base class for items that group other items
   struct REGISTRIES_API GroupItem : BaseItem {
      using BaseItem::BaseItem;

      // Construction from an internal name and a previously built-up
      // vector of pointers
      GroupItem( const Identifier &internalName, BaseItemPtrs &&items_ )
         : BaseItem{ internalName }, items{ std::move( items_ ) }
      {}
      GroupItem( const GroupItem& ) PROHIBITED;
      ~GroupItem() override = 0;

      // Whether the item is non-significant for path naming
      // when it also has an empty name
      virtual bool Transparent() const = 0;

      BaseItemPtrs items;
   };
   
   // GroupItem adding variadic constructor conveniences
   template< typename VisitorType = ComputedItem::DefaultVisitor >
   struct InlineGroupItem : GroupItem {
      using GroupItem::GroupItem;
      // In-line, variadic constructor that doesn't require building a vector
      template< typename... Args >
         InlineGroupItem( const Identifier &internalName, Args&&... args )
         : GroupItem( internalName )
         { Append( std::forward< Args >( args )... ); }

   private:
      // nullary overload grounds the recursion
      void Append() {}
      // recursive overload
      template< typename Arg, typename... Args >
         void Append( Arg &&arg, Args&&... moreArgs )
         {
            // Dispatch one argument to the proper overload of AppendOne.
            // std::forward preserves rvalue/lvalue distinction of the actual
            // argument of the constructor call; that is, it inserts a
            // std::move() if and only if the original argument is rvalue
            AppendOne( std::forward<Arg>( arg ) );
            // recur with the rest of the arguments
            Append( std::forward<Args>(moreArgs)... );
         };

      // Move one unique_ptr to an item into our array
      void AppendOne( BaseItemPtr&& ptr )
      {
         items.push_back( std::move( ptr ) );
      }
      // This overload allows a lambda or function pointer in the variadic
      // argument lists without any other syntactic wrapping, and also
      // allows implicit conversions to type Factory.
      // (Thus, a lambda can return a unique_ptr<BaseItem> rvalue even though
      // Factory's return type is shared_ptr, and the needed conversion is
      // applied implicitly.)
      void AppendOne( const ComputedItem::Factory<VisitorType> &factory )
      {
         auto adaptedFactory = [factory]( Registry::Visitor &visitor ){
            return factory( dynamic_cast< VisitorType& >( visitor ) );
         };
         AppendOne( std::make_unique<ComputedItem>( adaptedFactory ) );
      }
      // This overload lets you supply a shared pointer to an item, directly
      template<typename Subtype>
      void AppendOne( const std::shared_ptr<Subtype> &ptr )
      { AppendOne( std::make_unique<SharedItem>(ptr) ); }
   };

   // Inline group item also specifying transparency
   template< bool transparent,
      typename VisitorType = ComputedItem::DefaultVisitor >
   struct ConcreteGroupItem : InlineGroupItem< VisitorType >
   {
      using InlineGroupItem< VisitorType >::InlineGroupItem;
      ~ConcreteGroupItem() {}
      bool Transparent() const override { return transparent; }
   };

   // Concrete subclass of GroupItem that adds nothing else
   // TransparentGroupItem with an empty name is transparent to item path calculations
   // and propagates its ordering hint if subordinates don't specify hints
   // and it does specify one
   template< typename VisitorType = ComputedItem::DefaultVisitor >
   struct TransparentGroupItem final : ConcreteGroupItem< true, VisitorType >
   {
      using ConcreteGroupItem< true, VisitorType >::ConcreteGroupItem;
      ~TransparentGroupItem() override {}
   };

   // The /-separated path is relative to the GroupItem supplied to
   // RegisterItem.
   // For instance, wxT("Transport/Cursor") to locate an item under a sub-menu
   // of a main menu
   struct Placement {
      wxString path;
      OrderingHint hint;

      Placement( const wxString &path_, const OrderingHint &hint_ = {} )
         : path( path_ ), hint( hint_ )
      {}
   };

   // registry collects items, before consulting preferences and ordering
   // hints, and applying the merge procedure to them.
   // This function puts one more item into the registry.
   // The sequence of calls to RegisterItem has no significance for
   // determining the visitation ordering.  When sequence is important, register
   // a GroupItem.
   REGISTRIES_API
   void RegisterItem( GroupItem &registry, const Placement &placement,
      BaseItemPtr pItem );
   
   // Define actions to be done in Visit.
   // Default implementations do nothing
   // The supplied path does not include the name of the item
   class REGISTRIES_API Visitor
   {
   public:
      virtual ~Visitor();
      using Path = std::vector< Identifier >;
      virtual void BeginGroup( GroupItem &item, const Path &path );
      virtual void EndGroup( GroupItem &item, const Path &path );
      virtual void Visit( SingleItem &item, const Path &path );
   };

   // Top-down visitation of all items and groups in a tree rooted in
   // pTopItem, as merged with pRegistry.
   // The merger of the trees is recomputed in each call, not saved.
   // So neither given tree is modified.
   // But there may be a side effect on preferences to remember the ordering
   // imposed on each node of the unordered tree of registered items; each item
   // seen in the registry for the first time is placed somehere, and that
   // ordering should be kept the same thereafter in later runs (which may add
   // yet other previously unknown items).
   REGISTRIES_API void Visit(
      Visitor &visitor,
      BaseItem *pTopItem,
      const GroupItem *pRegistry = nullptr );

   // Typically a static object.  Constructor initializes certain preferences
   // if they are not present.  These preferences determine an extrinsic
   // visitation ordering for registered items.  This is needed in some
   // places that have migrated from a system of exhaustive listings, to a
   // registry of plug-ins, and something must be done to preserve old
   // behavior.  It can be done in the central place using string literal
   // identifiers only, not requiring static compilation or linkage dependency.
   struct REGISTRIES_API
   OrderingPreferenceInitializer : PreferenceInitializer {
      using Literal = const wxChar *;
      using Pair = std::pair< Literal, Literal >;
      using Pairs = std::vector< Pair >;
      OrderingPreferenceInitializer(
         // Specifies the topmost preference section:
         Literal root,
         // Specifies /-separated Registry paths relative to root
         // (these should be blank or start with / and not end with /),
         // each with a ,-separated sequence of identifiers, which specify a
         // desired ordering at one node of the tree:
         Pairs pairs );

      void operator () () override;

   private:
      Pairs mPairs;
      Literal mRoot;
   };
}

#endif

