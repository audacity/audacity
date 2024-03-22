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
#ifndef KORS_ASYNC_ASYNC_H
#define KORS_ASYNC_ASYNC_H

#include "asyncable.h"
#include "internal/asyncimpl.h"

namespace kors::async {
class Async
{
public:

    template<typename F>
    static void call(const Asyncable* caller, F f, const std::thread::id& th = std::this_thread::get_id())
    {
        AsyncImpl::instance()->call(const_cast<Asyncable*>(caller), new AsyncImpl::Functor<F>(f), th);
    }

    template<typename F, typename Arg1>
    static void call(const Asyncable* caller, F f, Arg1 a1, const std::thread::id& th = std::this_thread::get_id())
    {
        AsyncImpl::instance()->call(const_cast<Asyncable*>(caller), new AsyncImpl::FunctorArg1<F, Arg1>(f, a1), th);
    }

    static void disconnectAsync(Asyncable* a)
    {
        AsyncImpl::instance()->disconnectAsync(a);
    }
};
}

#endif // KORS_ASYNC_ASYNC_H
