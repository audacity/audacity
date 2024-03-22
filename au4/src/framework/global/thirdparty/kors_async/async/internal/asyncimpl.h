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
#ifndef KORS_ASYNC_ASYNCIMPL_H
#define KORS_ASYNC_ASYNCIMPL_H

#include <mutex>
#include <map>
#include <thread>
#include "../asyncable.h"

namespace kors::async {
class AsyncImpl : public Asyncable::IConnectable
{
public:

    static AsyncImpl* instance();

    struct IFunction {
        virtual ~IFunction() {}
        virtual void call() = 0;
    };

    template<typename F>
    struct Functor : public IFunction {
        F functor;
        Functor(const F fn)
            : functor(fn) {}
        void call() { functor(); }
    };

    template<typename F, typename Arg1>
    struct FunctorArg1 : public IFunction {
        F functor;
        Arg1 arg1;
        FunctorArg1(const F fn, Arg1 a1)
            : functor(fn), arg1(a1) {}
        void call() { functor(arg1); }
    };

    void call(Asyncable* caller, IFunction* f, const std::thread::id& th = std::this_thread::get_id());
    void disconnectAsync(Asyncable* caller);

private:
    AsyncImpl() = default;

    void onCall(uint64_t key);

    struct Call {
        Asyncable* caller = nullptr;
        IFunction* f = nullptr;
        Call() = default;
        Call(Asyncable* c, IFunction* _f)
            : caller(c), f(_f) {}
    };

    std::mutex m_mutex;
    std::map<uint64_t, Call> m_calls;
};
}

#endif // KORS_ASYNC_ASYNCIMPL_H
