/**********************************************************************

  Audacity: A Digital Audio Editor

  @file Observer.h
  @brief Publisher-subscriber pattern, also known as Observer

  Paul Licameli

**********************************************************************/
#ifndef __AUDACITY_OBSERVER__
#define __AUDACITY_OBSERVER__

#include <cassert>
#include <functional>
#include <memory>
#include <type_traits>

namespace Observer {

// See below
template<typename Message, bool NotifyAll>
class Publisher;

//! Default message type for Publisher
struct Message {};

//! May be supplied to constructor of Publisher to customize exception handling
struct UTILITY_API ExceptionPolicy {
   virtual ~ExceptionPolicy() noexcept;
   //! Called at the start of each publication
   virtual void OnBeginPublish() = 0;
   // In a catch block, can rethrow or store std::current_exception()
   /*! @return if true, then skip remaining callbacks */
   virtual bool OnEachFailedCallback() noexcept(false) = 0;
   //! Called at the end of each publication, if exiting normally; may throw
   virtual void OnEndPublish() noexcept(false) = 0;
};

class Subscription;

//! Type-erased implementation helpers for Publisher
namespace detail {
struct RecordBase;
struct RecordLink{
   std::shared_ptr<RecordBase> next;
};
//! doubly-linked list cell using shared and weak pointers
struct UTILITY_API RecordBase : RecordLink {
   std::weak_ptr<RecordLink> prev;
   void Unlink() noexcept;
};
struct UTILITY_API RecordList
   : RecordLink, std::enable_shared_from_this<RecordLink> {
   //! Type of function visiting the list; stop visit on true return
   using Visitor = bool(*)(const RecordBase &record, const void *arg);
   //! @pre `visitor != nullptr`
   explicit RecordList(ExceptionPolicy *pPolicy, Visitor visitor);
   ~RecordList() noexcept;
   //! @pre `pRecord != nullptr`
   Subscription Subscribe(std::shared_ptr<RecordBase> pRecord);
   bool Visit(const void *arg);
private:
   ExceptionPolicy *const m_pPolicy;
   const Visitor m_visitor;
};
}

//! A move-only handle representing a connection to a Publisher
class UTILITY_API Subscription {
public:
   Subscription();
   Subscription(Subscription &&);
   Subscription &operator=(Subscription &&);

   //! Calls Reset
   ~Subscription() noexcept { Reset(); }

   //! Breaks the connection (constant time)
   void Reset() noexcept;

   /*!
      @return true iff there is no connection
      (Publisher was destroyed, or this was not reassigned since it was last
      Reset(), default-constructed, or moved from)
    */
   bool Expired() const { return m_wRecord.expired(); }

   //! @return not expired
   explicit operator bool() const { return !Expired(); }

private:
   friend detail::RecordList;
   explicit Subscription(std::weak_ptr<detail::RecordBase> pRecord);
   std::weak_ptr<detail::RecordBase> m_wRecord;
};

//! An object that sends messages to an open-ended list of subscribed callbacks
/*!
 Intended for single-threaded use only.
 Move-only; allows type-erased custom allocation.

 @tparam Message the type of message
 @tparam NotifyAll if true, callback return values are ignored; else, a callback
   returning true causes earlier subscribed callbacks to be skipped
 */
template<typename Message = Message, bool NotifyAll = true>
class Publisher {
public:
   //! An implementation class, public so it can be your custom allocator's template parameter
   struct Record;

   static constexpr bool notifies_all = NotifyAll;
   using message_type = Message;

   //! Constructor supporting type-erased custom allocation/deletion
   /*!
    @param pPolicy if null, exceptions from callbacks are caught and ignored;
    else, *pPolicy must have a lifetime encompassing the Publisher's
    */
   template<typename Alloc = std::allocator<Record>>
   explicit Publisher(ExceptionPolicy *pPolicy = nullptr, Alloc a = {});

   Publisher(Publisher&&) = default;
   Publisher& operator=(Publisher&&) = default;

   using CallbackReturn = std::conditional_t<NotifyAll, void, bool>;

   //! Type of functions that can be connected to the Publisher
   using Callback = std::function< CallbackReturn(const Message&) >;

   //! Connect a callback to the Publisher; later-connected are called earlier
   /*!
    During Publish(), the callback may have the side-effect of adding or
    removing other Subscriptions.  Added subscriptions will not be called,
    and removed ones, if not called already, will not be called after.

    @pre `callback != nullptr`
    */
   Subscription Subscribe(Callback callback);

   //! Overload of Subscribe takes an object and pointer-to-member-function
   template<typename Object, typename Return, typename... Args>
   Subscription Subscribe(
      Object &obj, Return (Object::*callback)(Args...))
   {
      return Subscribe( [&obj, callback](const Message &message){
         return (obj.*callback)(message);
      } );
   }

   struct Record : detail::RecordBase {
      explicit Record(Callback callback) : callback{ move(callback) } {}
      Callback callback;
   };

protected:
   //! Send a message to connected callbacks
   /*! Later-connected are called earlier; if !NotifyAll, any callback may
     return true, to stop the notification; see also class ExceptionPolicy

    @return if `NotifyAll`, then void; else, whether the visit was stopped
    (either because a callback returned true, or the exception policy consumed
    an exception, and ordered a stop)
    */
   CallbackReturn Publish(const Message &message);

private:
   // RecordList needs to be non-relocating but the Publisher object is movable
   std::shared_ptr<detail::RecordList> m_list;
   std::function<std::shared_ptr<detail::RecordBase>(Callback)> m_factory;
};

template<typename Message, bool NotifyAll>
template<typename Alloc> inline
Publisher<Message, NotifyAll>::Publisher(ExceptionPolicy *pPolicy, Alloc a)
: m_list{ std::allocate_shared<detail::RecordList>( a, pPolicy,
    []( // The visitor.  Lambda with no capture converts to pointer-to-function
      const detail::RecordBase &recordBase, const void *arg){
      auto &record = static_cast<const Record&>(recordBase);
      assert(arg);
      auto &message = *static_cast<const Message*>(arg);
      assert(record.callback);
      // Calling foreign code!  Which is why we have an exception policy.
      if constexpr (NotifyAll)
         return (record.callback(message), false);
      else
         return record.callback(message);
   }
) }
, m_factory( [a = move(a)](Callback callback) {
   return std::allocate_shared<Record>(a, move(callback));
} )
{}

// Thin wrappers of type-erased worker functions:

template<typename Message, bool NotifyAll>
auto Publisher<Message, NotifyAll>::Subscribe(Callback callback)
   -> Subscription
{
   assert(callback); // precondition
   return m_list->Subscribe(m_factory(move(callback)));
}

template<typename Message, bool NotifyAll> inline
auto Publisher<Message, NotifyAll>::Publish(const Message &message)
   -> CallbackReturn
{
   bool result = m_list->Visit(&message);
   if constexpr (!NotifyAll)
      return result;
}

}

#endif
