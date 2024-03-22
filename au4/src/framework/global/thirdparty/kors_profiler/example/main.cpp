#include <iostream>
#include <thread>
#include <chrono>

#include "profiler.h"

class Example
{
public:
    Example() = default;

    static void th_func_1()
    {
        TRACEFUNC;
        std::this_thread::sleep_for(std::chrono::milliseconds(2));
    }

    static void th_func_2()
    {
        TRACEFUNC;
        std::this_thread::sleep_for(std::chrono::milliseconds(2));
    }

    static void th_func_3()
    {
        TRACEFUNC;
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }

    static void th_func_4()
    {
        TRACEFUNC;
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }

    void func1()
    {
        TRACEFUNC;
        std::this_thread::sleep_for(std::chrono::milliseconds(2));
    }

    void func2()
    {
        TRACEFUNC;
        std::this_thread::sleep_for(std::chrono::milliseconds(2));
    }

    void func3()
    {
        TRACEFUNC;
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }

    void func4()
    {
        TRACEFUNC;
        std::this_thread::sleep_for(std::chrono::milliseconds(10));

        STEP_TIME("mark1", "end body func4");

        func3();
    }

    void example()
    {
        std::thread th1(th_func_1);
        std::thread th2(th_func_2);
        std::thread th3(th_func_3);
        std::thread th4(th_func_4);

        TRACEFUNC;
        func1();

        BEGIN_STEP_TIME("mark1");

        {
            TRACEFUNC_C("call func2 10 times")
            for (int i = 0; i < 10; ++i) {
                func2();
            }
        }

        STEP_TIME("mark1", "end call func2 10 times");

        func3();
        STEP_TIME("mark1", "end func3");

        func4();

        th1.join();
        th2.join();
        th3.join();
        th4.join();
    }
};

int main(int argc, char* argv[])
{
    std::cout << "Hello World, I am Profiler" << std::endl;

    using namespace app::profiler;

    //! NOTE Setup

    // custom printer
    struct MyPrinter : public Profiler::Printer
    {
        void printDebug(const std::string& str) override { std::clog << str << std::endl; }
        void printInfo(const std::string& str) override { std::clog << str << std::endl; }
    };

    // options
    Profiler::Options profOpt;
    profOpt.stepTimeEnabled = true;         // enable measure of steps, macros: BEGIN_STEP_TIME, STEP_TIME
    profOpt.funcsTimeEnabled = true;        // enable measure of functions, macros: TRACEFUNC, TRACEFUNC_C
    profOpt.funcsTraceEnabled = false;      // enable trace (output by func call), macros: TRACEFUNC, TRACEFUNC_C
    profOpt.funcsMaxThreadCount = 100;      // max treads count
    profOpt.statTopCount = 150;             // statistic top count

    Profiler* profiler = Profiler::instance();
    profiler->setup(profOpt, new MyPrinter());

    Example t;
    t.example();

    PROFILER_PRINT;

/* Output:
mark1 : 0.000/0.001 ms: Begin
mark1 : 25.427/25.326 ms: end call func2 10 times
mark1 : 36.794/11.139 ms: end func3
mark1 : 48.250/11.385 ms: end body func4


Main thread. Top 150 by sum time (total count: 6)
Function                                                    Call time         Call count        Sum time
Example::example                                            63.571 ms         1                 63.571 ms
call func2 10 times                                         25.168 ms         1                 25.168 ms
Example::func2                                              2.505 ms          10                25.052 ms
Example::func4                                              23.687 ms         1                 23.687 ms
Example::func3                                              11.394 ms         2                 22.788 ms
Example::func1                                              2.781 ms          1                 2.781 ms


Other threads. Top 150 by sum time (total count: 4)
Function                                                    Call time         Call count        Sum time
Example::th_func_3                                          11.056 ms         1                 11.056 ms
Example::th_func_4                                          10.433 ms         1                 10.433 ms
Example::th_func_2                                          3.297 ms          1                 3.297 ms
Example::th_func_1                                          2.435 ms          1                 2.435 ms

*/

}
