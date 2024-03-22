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
#ifndef KORS_PROFILER_H
#define KORS_PROFILER_H

#include <string>
#include <string_view>
#include <list>
#include <vector>
#include <set>
#include <unordered_map>
#include <thread>
#include <mutex>
#include <chrono>
#include <sstream>
#include <atomic>

#include "funcinfo.h"

// #define KORS_PROFILER_ENABLED

#ifdef KORS_PROFILER_ENABLED

#ifndef TRACEFUNC
#define TRACEFUNC \
    static std::string __func_info(CLASSFUNC); \
    kors::profiler::FuncMarker __funcMarker(__func_info);
#endif

#ifndef TRACEFUNC_C
#define TRACEFUNC_C(info) \
    static std::string __func_info(info); \
    kors::profiler::FuncMarker __funcMarkerInfo(__func_info);
#endif

#ifndef BEGIN_STEP_TIME
#define BEGIN_STEP_TIME(tag) \
    if (kors::profiler::Profiler::options().stepTimeEnabled) \
    { kors::profiler::Profiler::instance()->stepTime(tag, std::string("Begin"), true); }
#endif

#ifndef STEP_TIME
#define STEP_TIME(tag, info) \
    if (kors::profiler::Profiler::options().stepTimeEnabled) \
    { kors::profiler::Profiler::instance()->stepTime(tag, info); }
#endif

#ifndef PROFILER_CLEAR
#define PROFILER_CLEAR kors::profiler::Profiler::instance()->clear();
#endif

#ifndef PROFILER_PRINT
#define PROFILER_PRINT kors::profiler::Profiler::instance()->printThreadsData();
#endif

#else

#define TRACEFUNC
#define TRACEFUNC_C(info)
#define BEGIN_STEP_TIME
#define STEP_TIME
#define PROFILER_CLEAR
#define PROFILER_PRINT

#endif

namespace kors::profiler {
class Profiler
{
public:

    static Profiler* instance();

    struct Options {
        Options() {}
        bool stepTimeEnabled = true;
        std::atomic<bool> funcsTimeEnabled = true;
        bool funcsTraceEnabled = false;
        size_t funcsMaxThreadCount = 100;
        int statTopCount = 150;

        void assign(const Options& o) {
            stepTimeEnabled = o.stepTimeEnabled;
            funcsTimeEnabled = o.funcsTimeEnabled.load();
            funcsTraceEnabled = o.funcsTraceEnabled;
            funcsMaxThreadCount = o.funcsMaxThreadCount;
            statTopCount = o.statTopCount;
        }
    };

    struct Data {
        enum Mode {
            All,
            OnlyMain,
            OnlyOther
        };

        struct Func {
            std::string func;
            long callcount = 0;
            double sumtimeMs = 0.0;
            Func() {}
            Func(const std::string& f, long cc, double st)
                : func(f), callcount(cc), sumtimeMs(st) {}
        };

        struct Thread {
            std::thread::id thread;
            std::unordered_map<std::string, Func> funcs;
        };

        std::thread::id mainThread;
        std::unordered_map<std::thread::id, Thread> threads;
    };

    struct Printer {
        virtual ~Printer() = default;
        virtual void printDebug(const std::string& str);
        virtual void printInfo(const std::string& str);
        virtual void printStep(const std::string& tag, double beginMs, double stepMs, const std::string& info);
        virtual void printTraceBegin(const std::string& func, size_t stackCounter);
        virtual void printTraceEnd(const std::string& func, double calltimeMs, long callcount, double sumtimeMs, size_t stackCounter);
        virtual void printData(const Data& data, Data::Mode mode, int maxcount);
        virtual std::string formatData(const Data& data, Data::Mode mode, int maxcount) const;
        virtual void funcsToStream(std::stringstream& stream, const std::string& title, const std::list<Data::Func>& funcs,
                                   int count) const;

        static std::string formatDouble(double val, size_t prec);
        static std::string leftJustified(const std::string& in, size_t width);
    };

    struct ElapsedTimer {
        void start();
        void restart();
        double mlsecsElapsed() const; //NOTE fractional milliseconds
        void invalidate();
        bool isValid() const;

    private:
        std::chrono::high_resolution_clock::time_point m_start;
    };

    struct FuncTimer {
        const std::string& func;
        ElapsedTimer timer;
        long callcount = 0;
        double sumtimeMs = 0.0;
        explicit FuncTimer(const std::string& f)
            : func(f), callcount(0), sumtimeMs(0) {}
    };

    void setupDefault();
    void setup(const Options& opt = Options(), Printer* printer = nullptr);

    static const Options& options();
    Printer* printer() const;

    void stepTime(const std::string& tag, const std::string& info, bool isRestart = false);

    FuncTimer* beginFunc(const std::string& func);
    void endFunc(FuncTimer* timer, const std::string& func);

    const std::string& staticInfo(const std::string& info); //! NOTE Saving string

    void clear();

    Data threadsData(Data::Mode mode = Data::All) const;

    std::string threadsDataString(Data::Mode mode = Data::All) const;
    void printThreadsData(Data::Mode mode = Data::All) const;

    static void print(const std::string& str);

    bool save(const std::string& filePath);

private:
    Profiler();
    ~Profiler();

    friend struct FuncMarker;

    static Options m_options;

    struct StepTimer {
        ElapsedTimer beginTime;
        ElapsedTimer stepTime;

        double beginMs() const;
        double stepMs() const;
        void start();
        void restart();
        void nextStep();
    };

    typedef std::unordered_map<std::string, StepTimer* > StepTimers;
    struct StepsData {
        std::mutex mutex;
        StepTimers timers;
    };

    typedef std::unordered_map<const std::string*, FuncTimer* > FuncTimers;
    struct FuncsData {
        std::mutex mutex;
        std::atomic<size_t> lastIndex = 0;
        std::vector<std::thread::id> threads;
        std::vector<FuncTimers> timers;
        std::set<std::string> staticInfo;

        int threadIndex(std::thread::id th);
    };

    bool save_file(const std::string& path, const std::string& content);

    Printer* m_printer = nullptr;

    StepsData m_steps;
    mutable FuncsData m_funcs;

    size_t m_stackCounter = 0;
};

struct FuncMarker
{
    explicit FuncMarker(const std::string& fn)
        : func(fn)
    {
        if (Profiler::m_options.funcsTimeEnabled) {
            timer = Profiler::instance()->beginFunc(fn);
        }
    }

    ~FuncMarker()
    {
        if (Profiler::m_options.funcsTimeEnabled) {
            Profiler::instance()->endFunc(timer, func);
        }
    }

    Profiler::FuncTimer* timer = nullptr;
    const std::string& func;
};
}

#endif // KORS_PROFILER_H
