/**********************************************************************

  Audacity: A Digital Audio Editor

  @file Observer.cpp
  @brief Publisher-subscriber pattern, also known as Observer

  Paul Licameli

**********************************************************************/
#include "Observer.h"

namespace Observer {

namespace detail {

void RecordBase::Unlink() noexcept
{
   auto pPrev = prev.lock();
   assert(pPrev); // See RecordList constructor and PushFront
   // Do not move from next, see Visit
   if (auto &pNext = (pPrev->next = next))
      pNext->prev = move(prev);
}

RecordList::RecordList( ExceptionPolicy *pPolicy, Visitor visitor )
   : m_pPolicy{ pPolicy }
   , m_visitor{ visitor }
{
   assert(m_visitor); // precondition
}

RecordList::~RecordList() noexcept
{
   //! Non-defaulted destructor.  Beware stack growth
   auto pRecord = move(next);
   while (pRecord)
      pRecord = move(pRecord->next);
}

Subscription RecordList::Subscribe(std::shared_ptr<RecordBase> pRecord)
{
   assert(pRecord); // precondition
   auto result = Subscription{ pRecord };
   if (auto &pNext = (pRecord->next = move(next)))
      pNext->prev = pRecord;
   pRecord->prev = weak_from_this();
   next = move(pRecord);
   return result;
}

bool RecordList::Visit(const void *arg)
{
   assert(m_visitor); // See constructor
   if (m_pPolicy)
      m_pPolicy->OnBeginPublish();
   bool result = false;
   for (auto pRecord = next; pRecord; pRecord = pRecord->next) {
      try {
         if (m_visitor(*pRecord, arg)) {
            result = true;
            break;
         }
         // pRecord might have been removed from the list by the callback,
         // but pRecord->next is unchanged.  We won't see callbacks added by
         // the callback, because they are earlier in the list.
      }
      catch (...) {
         if (m_pPolicy && m_pPolicy->OnEachFailedCallback()) {
            result = true;
            break;
         }
      }
   }
   // Intentionally not in a finally():
   if (m_pPolicy)
      m_pPolicy->OnEndPublish();
   return result;
}

}

ExceptionPolicy::~ExceptionPolicy() noexcept = default;

Subscription::Subscription() = default;
Subscription::Subscription(std::weak_ptr<detail::RecordBase> pRecord)
   : m_wRecord{ move(pRecord) } {}
Subscription::Subscription(Subscription &&) = default;
Subscription &Subscription::operator=(Subscription &&other)
{
   const bool inequivalent =
      m_wRecord.owner_before(other.m_wRecord) ||
      other.m_wRecord.owner_before(m_wRecord);
   if (inequivalent) {
      Reset();
      m_wRecord = move(other.m_wRecord);
   }
   return *this;
}

void Subscription::Reset() noexcept
{
   if (auto pRecord = m_wRecord.lock())
      pRecord->Unlink();
   m_wRecord.reset();
}

}
