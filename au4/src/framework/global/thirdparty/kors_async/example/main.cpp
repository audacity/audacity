#include <functional>
#include <iostream>
#include <thread>
#include <atomic>

// example aliases
#include "async/asyncable.h"
#include "async/processevents.h"
#include "async/channel.h"
#include "async/notification.h"

using namespace app::async;

namespace app::example {
class Counter
{
public:

    ~Counter()
    {
        close();
    }

    void increment()
    {
        ++m_val;
        m_ch.send(m_val);
    }

    int val() const { return m_val; }
    Channel<int> channel() const { return m_ch; }

    void close() { m_ch.close(); }

private:
    int m_val = 0;
    Channel<int> m_ch;
};

class ChannelExample : public kors::async::Asyncable
{
public:

    void justChannel()
    {
        std::cout << "-- Just channel: subscribed to receive values, then send them" << std::endl;

        Channel<int> ch;
        ch.onReceive(nullptr, [](int v) {
            std::cout << "[justChannel] receive val: " << v << std::endl;
        });

        for (int i = 0; i < 5; ++i) {
            std::cout << "[justChannel] send val: " << i << std::endl;
            ch.send(i);
        }
    }

    void channelFromObj()
    {
        std::cout << "-- Return channel from obj and subscribed to receive values" << std::endl;

        Counter counter;

        Channel<int> ch = counter.channel();
        ch.onReceive(this, [](int v) {
            std::cout << "[channelFromObj] receive val: " << v << std::endl;
        });

        for (int i = 0; i < 5; ++i) {
            counter.increment();
        }
    }

    void communication()
    {
        std::cout << "-- Communication:  main <-> thread" << std::endl;
        {
            std::thread::id mainThreadId = std::this_thread::get_id();
            struct Channels {
                Channel<std::string> cmd;
                Channel<int, int> in;
                Channel<int> out;
            };

            Channels channels;

            bool running = true;

            //! NOTE Subscribe to commands, this is necessary for synchronization,
            //! to be sure that the worker has subscribe to receive data before sending it.
            channels.cmd.onReceive(this, [&channels, &running, mainThreadId](const std::string& cmd) {
                //! NOTE Will be called on the subscribe thread (not the send thread),
                //! i.e. there is no need to worry about thread safety in the callback
                std::thread::id thId = std::this_thread::get_id();
                std::cout << "[communication] receive thread: " << (mainThreadId == thId ? "'main'" : "'not main'") // will be main
                          << ", cmd: " << cmd << std::endl;

                //! NOTE On worker ready send in data
                if (cmd == "worker_ready") {
                    for (int i = 0; i < 5; ++i) {
                        std::cout << "[communication] send thread: " << (mainThreadId == thId ? "'main'" : "'not main'") // will be main
                                  << ", val: " << i << std::endl;
                        channels.in.send(i, i + 10);
                    }

                    //! NOTE Close the channel to report no data.
                    channels.in.close();

                    //! NOTE On worker finished, end running main event loop imitation
                } else if (cmd == "worker_finished") {
                    running = false;
                }
            });

            //! NOTE Subscribe to out data from thread
            channels.out.onReceive(this, [mainThreadId](int val) {
                //! NOTE Will be called on the subscribe thread (not the send thread),
                //! i.e. there is no need to worry about thread safety in the callback
                std::thread::id thId = std::this_thread::get_id();
                std::cout << "[communication] receive thread: " << (mainThreadId == thId ? "'main'" : "'not main'") // will be main
                          << ", val: " << val << std::endl;
            });

            std::thread thread([this, &channels, mainThreadId]() {
                //! NOTE Subscribe to in data from main
                channels.in.onReceive(this, [mainThreadId, &channels](int val1, int val2) {
                    //! NOTE Will be called on the subscribe thread (not the send thread),
                    //! i.e. there is no need to worry about thread safety in the callback
                    std::thread::id thId = std::this_thread::get_id();
                    std::cout << "[communication] receive thread: " << (mainThreadId == thId ? "'main'" : "'not main'") // will be not main
                              << ", val1: " << val1 << ", val2: " << val2 << std::endl;

                    //! NOTE Calc and send out data
                    channels.out.send(val1 + val2);
                });

                //! NOTE On close in channel, end running worker event loop imitation
                bool th_running = true;
                channels.in.onClose(this, [&th_running]() {
                    th_running = false;
                });

                //! NOTE Send worker ready
                channels.cmd.send("worker_ready");

                //! NOTE Worker event loop imitation
                while (th_running) {
                    app::async::processEvents();
                    std::this_thread::yield();
                    std::this_thread::sleep_for(std::chrono::milliseconds(2));
                }

                channels.cmd.send("worker_finished");
            });

            //! NOTE Main event loop imitation
            while (running) {
                app::async::processEvents();
                std::this_thread::yield();
                std::this_thread::sleep_for(std::chrono::milliseconds(2));
            }

            thread.join();
        }
    }
};
}

int main(int argc, char* argv[])
{
    std::cout << "Hello World, I am async" << std::endl;

    using namespace app::example;

    ChannelExample chExample;
    chExample.justChannel();
    chExample.channelFromObj();
    chExample.communication();
}
