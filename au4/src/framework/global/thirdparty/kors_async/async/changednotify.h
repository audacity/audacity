/*
MIT License

Copyright (c) 2020 Igor Korsukov

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/
#ifndef KORS_ASYNC_CHANGEDNOTIFIER_H
#define KORS_ASYNC_CHANGEDNOTIFIER_H

#include "internal/abstractinvoker.h"

namespace kors::async {
template<typename T>
class ChangedNotifier;

template<typename T>
class ChangedNotify
{
public:
    ChangedNotify() = default;
    ChangedNotify(const ChangedNotify& notify)
        : m_ptr(notify.ptr()) {}
    ~ChangedNotify() {}

    template<typename Call>
    void onChanged(Asyncable* caller, Call f, Asyncable::AsyncMode mode = Asyncable::AsyncMode::AsyncSetOnce)
    {
        ptr()->addCallBack(Changed, caller, new ChangedCall<Call>(f), mode);
    }

    void resetOnChanged(Asyncable* caller)
    {
        ptr()->removeCallBack(Changed, caller);
    }

    template<typename Call>
    void onItemChanged(Asyncable* caller, Call f, Asyncable::AsyncMode mode = Asyncable::AsyncMode::AsyncSetOnce)
    {
        ptr()->addCallBack(ItemChanged, caller, new ItemChangedCallT<Call, T>(f), mode);
    }

    void resetOnItemChanged(Asyncable* caller)
    {
        ptr()->removeCallBack(ItemChanged, caller);
    }

    template<typename Call>
    void onItemAdded(Asyncable* caller, Call f, Asyncable::AsyncMode mode = Asyncable::AsyncMode::AsyncSetOnce)
    {
        ptr()->addCallBack(ItemAdded, caller, new ItemAddedCallT<Call, T>(f), mode);
    }

    void resetOnItemAdded(Asyncable* caller)
    {
        ptr()->removeCallBack(ItemAdded, caller);
    }

    template<typename Call>
    void onItemRemoved(Asyncable* caller, Call f, Asyncable::AsyncMode mode = Asyncable::AsyncMode::AsyncSetOnce)
    {
        ptr()->addCallBack(ItemRemoved, caller, new ItemRemovedCallT<Call, T>(f), mode);
    }

    void resetOnItemRemoved(Asyncable* caller)
    {
        ptr()->removeCallBack(ItemRemoved, caller);
    }

    template<typename Call>
    void onItemReplaced(Asyncable* caller, Call f, Asyncable::AsyncMode mode = Asyncable::AsyncMode::AsyncSetOnce)
    {
        ptr()->addCallBack(ItemReplaced, caller, new ItemReplacedCallT<Call, T>(f), mode);
    }

    void resetOnItemReplaced(Asyncable* caller)
    {
        ptr()->removeCallBack(ItemReplaced, caller);
    }

    enum CallType {
        Undefined = 0,
        Changed,
        ItemChanged,
        ItemAdded,
        ItemRemoved,
        ItemReplaced
    };

private:
    friend class ChangedNotifier<T>;

    struct IChanged {
        virtual ~IChanged() = default;
        virtual void changed() = 0;
    };

    template<typename Call>
    struct ChangedCall : public IChanged {
        Call f;
        ChangedCall(Call _f)
            : f(_f) {}
        void changed() { f(); }
    };

    struct IItemChanged {
        virtual ~IItemChanged() = default;
        virtual void itemChanged(const NotifyData& item) = 0;
    };

    template<typename Call, typename Arg>
    struct ItemChangedCallT : public IItemChanged {
        Call f;
        ItemChangedCallT(Call _f)
            : f(_f) {}
        void itemChanged(const NotifyData& item) { f(item.arg<Arg>()); }
    };

    struct IItemAdded {
        virtual ~IItemAdded() = default;
        virtual void itemAdded(const NotifyData& item) = 0;
    };

    template<typename Call, typename Arg>
    struct ItemAddedCallT : public IItemAdded {
        Call f;
        ItemAddedCallT(Call _f)
            : f(_f) {}
        void itemAdded(const NotifyData& item) { f(item.arg<Arg>()); }
    };

    struct IItemRemoved {
        virtual ~IItemRemoved() = default;
        virtual void itemRemoved(const NotifyData& item) = 0;
    };

    template<typename Call, typename Arg>
    struct ItemRemovedCallT : public IItemRemoved {
        Call f;
        ItemRemovedCallT(Call _f)
            : f(_f) {}
        void itemRemoved(const NotifyData& item) { f(item.arg<Arg>()); }
    };

    struct IItemReplaced {
        virtual ~IItemReplaced() = default;
        virtual void itemReplaced(const NotifyData& item) = 0;
    };

    template<typename Call, typename Arg>
    struct ItemReplacedCallT : public IItemReplaced {
        Call f;
        ItemReplacedCallT(Call _f)
            : f(_f) {}
        void itemReplaced(const NotifyData& item) { f(item.arg<Arg>(0), item.arg<Arg>(1)); }
    };

    struct ChangedInvoker : public AbstractInvoker
    {
        friend class ChangedNotify<T>;

        ChangedInvoker() = default;
        ~ChangedInvoker()
        {
            removeAllCallBacks();
        }

        void deleteCall(int _type, void* call) override
        {
            CallType type = static_cast<CallType>(_type);
            switch (type) {
            case Undefined: {} break;
            case Changed:     {
                delete static_cast<IChanged*>(call);
            } break;
            case ItemChanged:   {
                delete static_cast<IItemChanged*>(call);
            } break;
            case ItemAdded:   {
                delete static_cast<IItemAdded*>(call);
            } break;
            case ItemRemoved: {
                delete static_cast<IItemRemoved*>(call);
            } break;
            case ItemReplaced: {
                delete static_cast<IItemReplaced*>(call);
            } break;
            }
        }

        void doInvoke(int callKey, void* call, const NotifyData& d) override
        {
            switch (callKey) {
            case Undefined: return;
            case Changed: {
                static_cast<IChanged*>(call)->changed();
            } break;
            case ItemChanged: {
                static_cast<IItemChanged*>(call)->itemChanged(d);
            } break;
            case ItemAdded: {
                static_cast<IItemAdded*>(call)->itemAdded(d);
            } break;
            case ItemRemoved: {
                static_cast<IItemRemoved*>(call)->itemRemoved(d);
            } break;
            case ItemReplaced: {
                static_cast<IItemReplaced*>(call)->itemReplaced(d);
            } break;
            }
        }
    };

    std::shared_ptr<ChangedInvoker> ptr() const
    {
        if (!m_ptr) {
            m_ptr = std::make_shared<ChangedInvoker>();
        }
        return m_ptr;
    }

    mutable std::shared_ptr<ChangedInvoker> m_ptr = nullptr;
};

template<typename T>
class ChangedNotifier
{
public:
    ChangedNotifier()
        : m_notify(std::make_shared<ChangedNotify<T> >()) {}
    ~ChangedNotifier() {}

    std::shared_ptr<ChangedNotify<T> > notify() const
    {
        return m_notify;
    }

    void changed()
    {
        m_notify->ptr()->invoke(ChangedNotify<T>::Changed);
    }

    void itemChanged(const T& item)
    {
        NotifyData d;
        d.setArg<T>(0, item);

        m_notify->ptr()->invoke(ChangedNotify<T>::ItemChanged, d);
    }

    void itemAdded(const T& item)
    {
        NotifyData d;
        d.setArg<T>(0, item);

        m_notify->ptr()->invoke(ChangedNotify<T>::ItemAdded, d);
    }

    void itemRemoved(const T& item)
    {
        NotifyData d;
        d.setArg<T>(0, item);

        m_notify->ptr()->invoke(ChangedNotify<T>::ItemRemoved, d);
    }

    void itemReplaced(const T& oldItem, const T& newItem)
    {
        NotifyData d;
        d.setArg<T>(0, oldItem);
        d.setArg<T>(1, newItem);

        m_notify->ptr()->invoke(ChangedNotify<T>::ItemReplaced, d);
    }

private:
    std::shared_ptr<ChangedNotify<T> > m_notify;
};
}

#endif // KORS_ASYNC_CHANGEDNOTIFIER_H
