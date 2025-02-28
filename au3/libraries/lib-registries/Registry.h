/**********************************************************************

Audacity: A Digital Audio Editor

Registry.h

Paul Licameli split from CommandManager.h

**********************************************************************/

#ifndef __AUDACITY_REGISTRY__
#define __AUDACITY_REGISTRY__

#include "Prefs.h"
#include "Composite.h"
#include "TypeSwitch.h"
#include "Variant.h"
#include <functional>
#include <type_traits>
#include <utility>

// Define classes and functions that associate parts of the user interface
// with path names
namespace Registry {
struct REGISTRIES_API EmptyContext {
    static EmptyContext Instance;
};
struct DefaultTraits {
    template<typename ... T> using List = TypeList::List<T...>;
    using ComputedItemContextType = EmptyContext;
    using LeafTypes = TypeList::Nil;
    using NodeTypes = TypeList::Nil;
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
        Unspecified  // keep this last
    } type;

    // name of some other BaseItem; significant only when type is Before or
    // After:
    Identifier name;

    OrderingHint(Type type = Unspecified, const wxString& name = {})
        : type{type}, name{name} {}

    bool operator ==(const OrderingHint& other) const
    { return name == other.name && type == other.type; }

    bool operator <(const OrderingHint& other) const
    {
        // This sorts unspecified placements later
        return std::make_pair(type, name)
               < std::make_pair(other.type, other.name);
    }
};

template<typename RegistryTraits> struct GroupItem;
using Path = std::vector<Identifier>;
struct Placement;
struct SingleItem;

namespace detail {
using namespace TypeList;
struct GroupItemBase;

struct REGISTRIES_API BaseItem {
    // declare at least one virtual function so dynamic_cast will work
    explicit
    BaseItem(const Identifier& internalName)
        : name{internalName}
    {}
    virtual ~BaseItem();

    const Identifier name;

    OrderingHint orderingHint;
};

using BaseItemPtr = std::unique_ptr<BaseItem>;

using BaseItemSharedPtr = std::shared_ptr<BaseItem>;

struct REGISTRIES_API IndirectItemBase : BaseItem {
    explicit IndirectItemBase(const BaseItemSharedPtr& ptr)
        : BaseItem{wxEmptyString}
        , ptr{ptr}
    {}
    ~IndirectItemBase() override;

    BaseItemSharedPtr ptr;
};

struct ComputedItemBase;

template<typename Type, typename List> struct ConsUnique {
    template<typename L> struct NotAlready {
        static constexpr bool value = !std::is_same_v<Type, Head_t<L> >;
    };
    using type = std::conditional_t<
        std::disjunction<Null<List>, NotAlready<List> >::value,
        Cons_t<Type, List>, List
        >;
};
template<typename Type, typename List> using ConsUnique_t
    = typename ConsUnique<Type, List>::type;

template<typename RegistryTraits> using VisitedNodeTypes
    =ConsUnique_t<const GroupItem<RegistryTraits>, Map_t<Fn<std::add_const_t>,
                                                         typename RegistryTraits::NodeTypes> >;
template<typename RegistryTraits> using VisitedLeafTypes
    =ConsUnique_t<const SingleItem, Map_t<Fn<std::add_const_t>,
                                          typename RegistryTraits::LeafTypes> >;
template<typename Types> using VisitorFunction
    =std::function<void (Head_t<Types>&, const Path&)>;
}

//! The underlying registry merging procedure assumes the listed types
//! are an exhaustive partition
using BaseItemTypes = TypeList::List<
    detail::IndirectItemBase, detail::ComputedItemBase,
    // see below
    struct SingleItem, struct detail::GroupItemBase
    >;
template<typename Item> using AcceptableBaseItem
    =TypeList::HasBaseIn<Item, BaseItemTypes>;
template<typename Item> constexpr auto AcceptableBaseItem_v
    =AcceptableBaseItem<Item>::value;

template<typename RegistryTraits> struct AllTypes {
    using type = TypeList::Append_t<
        typename RegistryTraits::LeafTypes,
        typename RegistryTraits::NodeTypes
        >;
};
template<typename RegistryTraits> using AllTypes_t
    =typename AllTypes<RegistryTraits>::type;

template<typename RegistryTraits> constexpr auto AcceptableTraits_v
    =TypeList::Every_v<TypeList::Fn<AcceptableBaseItem>,
                       AllTypes_t<RegistryTraits> >
      && TypeSwitch::TypeListCheck_v<detail::VisitedNodeTypes<RegistryTraits> >
      && TypeSwitch::TypeListCheck_v<detail::VisitedLeafTypes<RegistryTraits> >;

namespace detail {
//! An item that delegates to another held in a shared pointer
/*!
    This allows static tables of items to be computed once and reused.
    The name of the delegate is significant for path calculations, but the
    IndirectItem's ordering hint is used if the delegate has none
    */
template<typename Item>
struct IndirectItem final : IndirectItemBase {
    using ItemType = Item;
    static_assert(AcceptableBaseItem_v<ItemType>);
    explicit IndirectItem(const std::shared_ptr<Item>& ptr)
        : IndirectItemBase{ptr}
    {}
};
}

//! A convenience function
template<typename Item>
inline std::unique_ptr<detail::IndirectItem<Item> > Indirect(
    const std::shared_ptr<Item>& ptr)
{
    return std::make_unique<detail::IndirectItem<Item> >(ptr);
}

namespace detail {
struct REGISTRIES_API ComputedItemBase : BaseItem {
    using TypeErasedFactory = std::function<BaseItemSharedPtr (void*)>;

    explicit ComputedItemBase(const TypeErasedFactory& factory)
        : BaseItem(wxEmptyString)
        , factory{move(factory)}
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
struct ComputedItem final : ComputedItemBase {
    using ItemType = Item;
    static_assert(AcceptableBaseItem_v<ItemType>);

    //! Type-erasing constructor template
    /*!
     Return type of the factory may be shared_ptr or unique_ptr.
     The result is converted to shared_ptr. This lets the function decide
     whether to recycle the object or rebuild it on demand each time.
     Return value from the factory may be null.
     */
    template<typename Factory> ComputedItem(const Factory& factory)
        : ComputedItemBase{[factory](void* p) -> BaseItemSharedPtr {
            // p comes from the compute item context argument of
            // Registry::Visit, passed by reference
            assert(p);
            return factory(*static_cast<Context*>(p));
        }}
    {}
};
}

//! Common abstract base class for items that are not groups
struct REGISTRIES_API SingleItem : detail::BaseItem {
    using BaseItem::BaseItem;
    ~SingleItem() override = 0;
};

namespace detail {
//! Type-erased implementation details, don't call directly
REGISTRIES_API void RegisterItem(GroupItemBase& registry, const Placement& placement, BaseItemPtr pItem);

//! Common abstract base class for items that group other items
struct REGISTRIES_API GroupItemBase : Composite::Base<
        BaseItem, std::unique_ptr<BaseItem>, const Identifier&
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
    friend REGISTRIES_API void RegisterItem(GroupItemBase& registry, const Placement& placement, BaseItemPtr pItem);
};
}

// Forward declarations necessary before customizing Composite::Traits
template<typename RegistryTraits> struct GroupItem;
namespace detail {
template<typename RegistryTraits> struct Builder;
}
}

template<typename RegistryTraits> struct Composite::Traits<
    Registry::detail::GroupItemBase, Registry::GroupItem<RegistryTraits>
    > {
    static constexpr auto ItemBuilder
        =Registry::detail::Builder<RegistryTraits> {};
    //! Prohibit pushing-back of type-unchecked BaseItemPtr directly
    template<typename T> static constexpr auto enables_item_type_v
        =!std::is_same_v<T, Registry::detail::BaseItemPtr>;
};

namespace Registry {
//! Has variadic and range constructors that check types
/*!
    @tparam RegistryTraits defines associated types
    */
template<typename RegistryTraits>
struct GroupItem : Composite::Builder<
        detail::GroupItemBase, GroupItem<RegistryTraits>, const Identifier&
        > {
    ~GroupItem() override = default;
    using Composite::Builder<
        detail::GroupItemBase, GroupItem<RegistryTraits>, const Identifier&
        >::Builder;
};

// The /-separated path is relative to the GroupItem supplied to
// RegisterItem.
// For instance, wxT("Transport/Cursor") to locate an item under a sub-menu
// of a main menu
struct Placement {
    wxString path;
    OrderingHint hint;

    Placement(const wxString& path = {}, const OrderingHint& hint = {})
        : path{path}, hint{hint}
    {}
    Placement(const wxChar* path, const OrderingHint& hint = {})
        : path{path}, hint{hint}
    {}
};

//! Find the real item type (following chains of indirections)
template<typename T> struct ActualItem {
    using type = T;
};
template<typename T> using ActualItem_t = typename ActualItem<T>::type;
template<typename T> struct ActualItem<detail::IndirectItem<T> >
{
    using type = ActualItem_t<T>;
};
template<typename C, typename T>
struct ActualItem<detail::ComputedItem<C, T> >
{
    using type = ActualItem_t<T>;
};

template<typename Item, typename TypeList> struct AcceptableItemType {
    static constexpr auto value
        =::TypeList::HasBaseIn_v<ActualItem_t<Item>, TypeList>;
};

template<typename RegistryTraits, typename Item> using AcceptableType
    =AcceptableItemType<Item, AllTypes_t<RegistryTraits> >;
template<typename RegistryTraits, typename Item>
constexpr auto AcceptableType_v
    =AcceptableType<RegistryTraits, Item>::value;

namespace detail {
template<typename RegistryTraits>
struct Builder {
    using Context = typename RegistryTraits::ComputedItemContextType;

    template<typename ItemType>
    auto operator ()(std::unique_ptr<ItemType> ptr) const
    {
        static_assert(AcceptableType_v<RegistryTraits, ItemType>);
        return move(ptr);
    }

    //! This overload allows a lambda or function pointer in the variadic
    //! argument lists without any other syntactic wrapping.
    template<typename Factory,
             typename ItemType
                 =typename std::invoke_result_t<Factory, Context&>::element_type
                 >
    auto operator ()(const Factory& factory) const
    {
        static_assert(AcceptableType_v<RegistryTraits, ItemType>);
        return std::make_unique<ComputedItem<Context, ItemType> >(factory);
    }

    //! This overload lets you supply a shared pointer to an item, directly
    template<typename ItemType>
    auto operator ()(const std::shared_ptr<ItemType>& ptr) const
    {
        static_assert(AcceptableType_v<RegistryTraits, ItemType>);
        return std::make_unique<IndirectItem<ItemType> >(ptr);
    }
};
}

// registry collects items, before consulting preferences and ordering
// hints, and applying the merge procedure to them.
// This function puts one more item into the registry.
// The sequence of calls to RegisterItem has no significance for
// determining the visitation ordering.  When sequence is important, register
// a GroupItem.
template<typename RegistryTraits, typename Item>
void RegisterItem(GroupItem<RegistryTraits>& registry,
                  const Placement& placement, std::unique_ptr<Item> pItem)
{
    static_assert(AcceptableTraits_v<RegistryTraits>);
    static_assert(AcceptableType_v<RegistryTraits, Item>,
                  "Registered item must be of one of the types listed in the registry's "
                  "traits");
    detail::RegisterItem(registry, placement, move(pItem));
}

//! Generates classes whose instances register items at construction
/*!
       Usually constructed statically
       @tparam RegistryClass defines static member `Registry()` returning
          `GroupItem<RegistryTraits>&`, for some traits class
    */
template<typename RegistryClass>
class RegisteredItem
{
public:
    template<typename Ptr>
    RegisteredItem(Ptr pItem, const Placement& placement = {})
    {
        if (pItem) {
            RegisterItem(RegistryClass::Registry(), placement, move(pItem));
        }
    }
};

namespace detail {
// Helpers allowing a single callable instead of a tuple
template<typename V> auto ForwardTuple_(const V& visitor, std::false_type)
{
    // Visitor is not a tuple
    return std::tuple<const V&> { visitor };
}

template<typename V> auto ForwardTuple_(const V& visitor, std::true_type)
{
    // Visitor is a tuple
    return Tuple::ForwardAll(visitor);
}

template<typename V> auto ForwardTuple(const V& visitor)
{
    return ForwardTuple_(visitor,
                         std::bool_constant<Tuple::is_tuple_like_v<V> > {});
}

template<typename V> auto MakeTuple_(const V& visitor, std::false_type)
{
    // Visitor is not a tuple
    return std::tuple<V> { visitor };
}

template<typename V> auto MakeTuple_(const V& visitor, std::true_type)
{
    // Visitor is a tuple
    return Tuple::All(visitor);
}

template<typename V> auto MakeTuple(const V& visitor)
{
    return MakeTuple_(visitor,
                      std::bool_constant<Tuple::is_tuple_like_v<V> > {});
}

template<typename V> auto CaptureTuple_(const V& visitor, std::true_type)
{
    // Capture by reference
    return ForwardTuple(visitor);
}

template<typename V> auto CaptureTuple_(const V& visitor, std::false_type)
{
    // Capture by value
    return MakeTuple(visitor);
}

template<bool Reference, typename V> auto CaptureTuple(const V& visitor)
{
    return CaptureTuple_(visitor, std::bool_constant<Reference> {});
}

//! Capture a callable for Visit() in a std::function wrapper
template<typename Types, bool Reference, typename Visitor>
VisitorFunction<Types> MakeVisitorFunction(const Visitor& visitor)
{
    return [visitor = CaptureTuple<Reference>(visitor)](
        Head_t<Types>& object, const Path& path
    ){
        // The compile time checking of reachability of cases still applies
        // before the visitor is wrapped
        TypeSwitch::Dispatch<void, Types>(object, visitor, path);
    };
}

//! How large a tuple to make from Visitors
template<typename Visitors> static constexpr auto TupleSize
    =std::conditional_t<
          Tuple::is_tuple_like_v<Visitors>,
          std::tuple_size<Visitors>,
          std::integral_constant<size_t, 1>
          >::value;

template<typename RegistryTraits> using NodeVisitorFunction
    =VisitorFunction<VisitedNodeTypes<RegistryTraits> >;
template<typename RegistryTraits> using LeafVisitorFunction
    =VisitorFunction<VisitedLeafTypes<RegistryTraits> >;
template<typename RegistryTraits> using VisitorFunctionTriple = std::tuple<
    NodeVisitorFunction<RegistryTraits>,
    LeafVisitorFunction<RegistryTraits>,
    NodeVisitorFunction<RegistryTraits>
    >;
}

//! Adapt visitors, suitable to Visit(), as a std::function or a tuple
//! of three std::functions, which other visitors may decorate with other
//! steps
/*!
    @tparam Reference if true, capture visitors by reference; else, move them
    */
template<typename RegistryTraits, bool Reference = false>
struct VisitorFunctions : std::variant<
        detail::LeafVisitorFunction<RegistryTraits>,
        detail::VisitorFunctionTriple<RegistryTraits>
        > {
    using NodeTypes = detail::VisitedNodeTypes<RegistryTraits>;
    using LeafTypes = detail::VisitedLeafTypes<RegistryTraits>;

    //! Type-erasing constructor
    /*!
     @param visitors one callable, for leaves only; or else a tuple(-like) of
     three callables (or type-switching tuples of callables), for group
     pre-visit, leaf visit, and group post-visit
     */
    template<typename Visitors> VisitorFunctions(Visitors&& visitors)
    {
        using namespace detail;
        using namespace std;
        decltype(auto) forwarded = forward<Visitors>(visitors);
        static constexpr auto size = TupleSize<std::decay_t<Visitors> >;
        static_assert(size == 1 || size == 3);
        if constexpr (size == 1) {
            this->template emplace<0>(
                MakeVisitorFunction<LeafTypes, Reference>(forwarded));
        } else if constexpr (size == 3) {
            this->template emplace<1>(
                MakeVisitorFunction<NodeTypes, Reference>(get<0>(forwarded)),
                MakeVisitorFunction<LeafTypes, Reference>(get<1>(forwarded)),
                MakeVisitorFunction<NodeTypes, Reference>(get<2>(forwarded)));
        }
    }

    //! Call-through for a decorating pre-visitor
    void BeginGroup(const GroupItem<RegistryTraits>& item, const Path& path)
    const
    {
        if (const auto* pTriple = std::get_if<1>(this)) {
            (std::get<0>(*pTriple))(item, path);
        }
    }

    //! Call-through for a decorating leaf-visitor
    void Visit(const SingleItem& item, const Path& path) const
    {
        using Function = detail::LeafVisitorFunction<RegistryTraits>;
        static const auto selector = Callable::OverloadSet{
            [](const Function& fn) -> const Function& { return fn; },
            [](auto&& self) -> const Function& {
                return std::get<1>(self);
            }
        };
        const auto& function = Variant::Visit(selector, *this);
        function(item, path);
    }

    //! Call-through for a decorating post-visitor
    void EndGroup(const GroupItem<RegistryTraits>& item, const Path& path)
    const
    {
        if (const auto* pTriple = std::get_if<1>(this)) {
            (std::get<2>(*pTriple))(item, path);
        }
    }
};

//! Supply this when one member of a visitor function triple isn't needed
constexpr auto NoOp = [](auto&, auto&){};

namespace detail {
class REGISTRIES_API VisitorBase
{
public:
    virtual ~VisitorBase();
    virtual void BeginGroup(const GroupItemBase& item, const Path& path)
    const = 0;
    virtual void Visit(const SingleItem& item, const Path& path)
    const = 0;
    virtual void EndGroup(const GroupItemBase& item, const Path& path)
    const = 0;
};

REGISTRIES_API void Visit(
    VisitorBase& visitor, const GroupItemBase* pTopItem, const GroupItemBase* pRegistry, void* pComputedItemContext);

//! Type-erasing adapter class (with no std::function overhead)
/*!
    See TypeSwitch for details of function signatures
    */
template<typename RegistryTraits, typename Visitors>
struct Visitor : VisitorBase {
    using NodeTypes = ConsUnique_t<const GroupItemBase,
                                   VisitedNodeTypes<RegistryTraits> >;
    using LeafTypes = VisitedLeafTypes<RegistryTraits>;
    static constexpr auto size = TupleSize<Visitors>;
    static_assert(size == 1 || size == 3);
    Visitor(const Visitors& visitors)
        : visitors{visitors} {}

    void BeginGroup(const GroupItemBase& item, const Path& path)
    const override
    {
        if constexpr (size == 3) {
            TypeSwitch::Dispatch<void, NodeTypes>(item,
                                                  ForwardTuple(std::get<0>(visitors)), path);
        }
    }

    void Visit(const SingleItem& item, const Path& path) const override
    {
        TypeSwitch::Dispatch<void, LeafTypes>(item,
                                              ForwardTuple(std::get < size == 1 ? 0 : 1 > (ForwardTuple(visitors))),
                                              path);
    }

    void EndGroup(const GroupItemBase& item, const Path& path)
    const override
    {
        if constexpr (size == 3) {
            TypeSwitch::Dispatch<void, NodeTypes>(item,
                                                  ForwardTuple(std::get<2>(visitors)), path);
        }
    }

    const Visitors& visitors;
};
}

//! Top-down visitation of all items and groups in a tree rooted in
//! pTopItem, as merged with pRegistry.
/*!
    The merger of the trees is recomputed in each call, not saved.
    So neither given tree is modified.
    But there may be a side effect on preferences to remember the ordering
    imposed on each node of the unordered tree of registered items; each item
    seen in the registry for the first time is placed somewhere, and that
    ordering should be kept the same thereafter in later runs (which may add
    yet other previously unknown items).

    @param visitors A tuple of size 1 or 3, or a callable.
      - If a triple, the first member is for pre-visit of group nodes, the last
         is for post-visit, and the middle for leaf visits.
      - If single, or a callable, then visit leaves only.
      - If a tuple, each member can be a tuple of callables (passed to
        TypeSwitch::Dispatch), or simply a single callable.
      - "tuple" means std::tuple or any other tuple-like type.
    The callables take a reference to an object of a const
    GroupItem<RegistryTraits> or a const SingleItem subtype, and the path up to
    its parent node.
    @param computedItemContext is passed to factory functions of computed items
    */
template<typename RegistryTraits, typename Visitors>
void Visit(const Visitors& visitors,
           const GroupItem<RegistryTraits>* pTopItem,
           const GroupItem<RegistryTraits>* pRegistry = {},
           typename RegistryTraits::ComputedItemContextType& computedItemContext =
           RegistryTraits::ComputedItemContextType::Instance)
{
    static_assert(AcceptableTraits_v<RegistryTraits>);
    detail::Visitor<RegistryTraits, Visitors> visitor{ visitors };
    detail::Visit(visitor, pTopItem, pRegistry, &computedItemContext);
}

//! @copydoc Visit
//! Like Visit but passing function(s) wrapped in std::function
template<typename RegistryTraits>
void VisitWithFunctions(const VisitorFunctions<RegistryTraits>& visitors,
                        const GroupItem<RegistryTraits>* pTopItem,
                        const GroupItem<RegistryTraits>* pRegistry = {},
                        typename RegistryTraits::ComputedItemContextType& computedItemContext =
                        RegistryTraits::ComputedItemContextType::Instance)
{
    static_assert(AcceptableTraits_v<RegistryTraits>);
    Variant::Visit([&](auto&& visitor){
        Visit(visitor, pTopItem, pRegistry, computedItemContext);
    }, visitors);
}

// Typically a static object.  Constructor initializes certain preferences
// if they are not present.  These preferences determine an extrinsic
// visitation ordering for registered items.  This is needed in some
// places that have migrated from a system of exhaustive listings, to a
// registry of plug-ins, and something must be done to preserve old
// behavior.  It can be done in the central place using string literal
// identifiers only, not requiring static compilation or linkage dependency.
struct REGISTRIES_API
OrderingPreferenceInitializer : PreferenceInitializer {
    using Literal = const wxChar*;
    using Pair = std::pair< Literal, Literal >;
    using Pairs = std::vector< Pair >;
    OrderingPreferenceInitializer(
        // Specifies the topmost preference section:
        Literal root,
        // Specifies /-separated Registry paths relative to root
        // (these should be blank or start with / and not end with /),
        // each with a ,-separated sequence of identifiers, which specify a
        // desired ordering at one node of the tree:
        Pairs pairs);

    void operator ()() override;

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
