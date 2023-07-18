/**********************************************************************

Audacity: A Digital Audio Editor

Registry.h

Paul Licameli split from CommandManager.h

**********************************************************************/

#ifndef __AUDACITY_REGISTRY__
#define __AUDACITY_REGISTRY__

#include "Prefs.h"
#include "Composite.h"
#include <type_traits>

// Define classes and functions that associate parts of the user interface
// with path names
namespace Registry {
   struct EmptyContext{};
   struct DefaultTraits {
      using ComputedItemContextType = EmptyContext;
   };

   // Items in the registry form an unordered tree, but each may also describe a
   // desired insertion point among its peers.  The request might not be honored
   // (as when the other name is not found, or when more than one item requests
   // the same ordering), but this is not treated as an error.
   struct OrderingHint
   {
      /*!
       The default Unspecified hint is just like End, except that in case the
       item is delegated to (by an IndirectItem, ComputedItem, or anonymous
       group), the delegating item's hint will be used instead
       */
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

   struct Placement;
   struct GroupItemBase;

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
   

namespace detail {
   struct REGISTRIES_API IndirectItemBase : BaseItem {
      explicit IndirectItemBase(const BaseItemSharedPtr &ptr)
         : BaseItem{ wxEmptyString }
         , ptr{ ptr }
      {}
      ~IndirectItemBase() override;

      BaseItemSharedPtr ptr;
   };

   //! An item that delegates to another held in a shared pointer
   /*!
    This allows static tables of items to be computed once and reused.
    The name of the delegate is significant for path calculations, but the
    IndirectItem's ordering hint is used if the delegate has none
    */
   template<typename Item>
   struct IndirectItem final : detail::IndirectItemBase {
      using ItemType = Item;
      static_assert(std::is_base_of_v<BaseItem, ItemType>);
      explicit IndirectItem(const std::shared_ptr<Item> &ptr)
         : IndirectItemBase{ ptr }
      {}
   };
}

   //! A convenience function
   template<typename Item>
   inline std::unique_ptr<detail::IndirectItem<Item>> Indirect(
      const std::shared_ptr<Item> &ptr)
   {
      return std::make_unique<detail::IndirectItem<Item>>( ptr );
   }

namespace detail {
   struct REGISTRIES_API ComputedItemBase : BaseItem {
      using TypeErasedFactory = std::function<BaseItemSharedPtr(void*)>;

      explicit ComputedItemBase(const TypeErasedFactory &factory)
         : BaseItem(wxEmptyString)
         , factory{ move(factory) }
      {}
      ~ComputedItemBase() override;

      TypeErasedFactory factory;
   };

   //! An item that computes some other item to substitute for it, each time
   //! it is visited
   /*!
    The name of the substitute is significant for path calculations, but the
    ComputedItem's ordering hint is used if the substitute has none
    */
   template<typename Context, typename Item>
   struct ComputedItem final : detail::ComputedItemBase {
      using ItemType = Item;
      static_assert(std::is_base_of_v<BaseItem, ItemType>);

      //! Type-erasing constructor template
      /*!
       Return type of the factory may be shared_ptr or unique_ptr.
       The result is converted to shared_ptr. This lets the function decide
       whether to recycle the object or rebuild it on demand each time.
       Return value from the factory may be null.
       */
      template<typename Factory> ComputedItem(const Factory &factory)
         : ComputedItemBase{ [factory](void *p) -> BaseItemSharedPtr {
            // p comes from RegistryVisitor::GetComputedItemContext which must
            // return non-null
            assert(p);
            return factory(*static_cast<Context *>(p));
         } }
      {}
   };
}

   //! Common abstract base class for items that are not groups
   struct REGISTRIES_API SingleItem : BaseItem {
      using BaseItem::BaseItem;
      ~SingleItem() override = 0;
   };

   //! Common abstract base class for items that group other items
   struct REGISTRIES_API GroupItemBase : Composite::Base<
      BaseItem, std::unique_ptr<BaseItem>, const Identifier &
   > {
      using Base::Base;
   
      GroupItemBase(const GroupItemBase&) = delete;
      GroupItemBase& operator=(const GroupItemBase&) = delete;
      ~GroupItemBase() override = 0;

      //! Choose treatment of the children of the group when merging trees
      enum Ordering {
         //! Item's name is ignored (omitted from paths) and sub-items are
         //! merged individually, sequenced by preferences or ordering hints
         Anonymous,
         //! Item's name is significant in paths, but its sequence of children
         //! may be overridden if it merges with another group at the same path
         Weak,
         //! Item's name is significant and it is intended to be the unique
         //! strongly ordered group at its path (but this could fail and
         //! cause an alpha-build-only error message during merging)
         Strong,
      };

      //! Default implementation returns Strong
      virtual Ordering GetOrdering() const;

   private:
      friend REGISTRIES_API void RegisterItem(GroupItemBase &registry,
         const Placement &placement, BaseItemPtr pItem);
   };

   // Forward declarations necessary before customizing Composite::Traits
   template<typename RegistryTraits> struct GroupItem;
   namespace detail {
      template<typename RegistryTraits> struct Builder;
   }
}

template<typename RegistryTraits> struct Composite::Traits<
   Registry::GroupItemBase, Registry::GroupItem<RegistryTraits>
> {
   static constexpr auto ItemBuilder =
   Registry::detail::Builder<RegistryTraits>{};
   //! Prohibit pushing-back of type-unchecked BaseItemPtr directly
   template<typename T> static constexpr auto enables_item_type_v = true;
};

namespace Registry {
   //! Extends GroupItemBase with a variadic constructor
   /*!
    @tparam RegistryTraits defines associated types
    */
   template<typename RegistryTraits>
   struct GroupItem : Composite::Builder<
      GroupItemBase, GroupItem<RegistryTraits>, const Identifier &
   > {
      ~GroupItem() override = default;
      using Composite::Builder<
         GroupItemBase, GroupItem<RegistryTraits>, const Identifier &
      >::Builder;
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

namespace detail {
   template<typename RegistryTraits>
   struct Builder {
      using Context = typename RegistryTraits::ComputedItemContextType;

      template<typename ItemType>
      auto operator () (std::unique_ptr<ItemType> ptr) const {
         return move(ptr);
      }
      //! This overload allows a lambda or function pointer in the variadic
      //! argument lists without any other syntactic wrapping.
      template<typename Factory,
         typename ItemType =
         typename std::invoke_result_t<Factory, Context&>::element_type
      >
      auto operator () (const Factory &factory) const {
         return std::make_unique<ComputedItem<Context, ItemType>>(factory);
      }
      //! This overload lets you supply a shared pointer to an item, directly
      template<typename ItemType>
      auto operator () (const std::shared_ptr<ItemType> &ptr) const {
         return std::make_unique<IndirectItem<ItemType>>(ptr);
      }
   };
}

   // registry collects items, before consulting preferences and ordering
   // hints, and applying the merge procedure to them.
   // This function puts one more item into the registry.
   // The sequence of calls to RegisterItem has no significance for
   // determining the visitation ordering.  When sequence is important, register
   // a GroupItem.
   REGISTRIES_API
   void RegisterItem( GroupItemBase &registry, const Placement &placement,
      BaseItemPtr pItem //!< Registry takes ownership
   );
   
   //! Generates classes whose instances register items at construction
   /*!
       Usually constructed statically
       @tparam Item inherits `BaseItem`
       @tparam RegistryClass defines static member `Registry()`
          returning `GroupItemBase&`
    */
   template<typename Item, typename RegistryClass = Item> class RegisteredItem {
   public:
      RegisteredItem(std::unique_ptr<Item> pItem, const Placement &placement)
      {
         if (pItem)
            RegisterItem(
               RegistryClass::Registry(), placement, std::move( pItem ) );
      }
   };
   
   // Define actions to be done in Visit.
   // Default implementations do nothing
   // The supplied path does not include the name of the item
   class REGISTRIES_API Visitor
   {
   public:
      virtual ~Visitor();
      using Path = std::vector< Identifier >;
      //! Get context given to Computed items during visitation
      /*!
       Default returns a pointer to a static EmptyContext
       @post result: `result != nullptr`
       */
      virtual void *GetComputedItemContext();
      virtual void BeginGroup( GroupItemBase &item, const Path &path );
      virtual void EndGroup( GroupItemBase &item, const Path &path );
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
      const GroupItemBase *pRegistry = nullptr );

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

#ifndef _WIN32
// extern explicit instantiation to avoid link time de-duplication
extern
#endif
       template struct
#ifdef _WIN32
// Can't do that on Windows, but need this to link:
                       __declspec(dllexport)
#endif
                                             GroupItem<DefaultTraits>;
}

#endif
