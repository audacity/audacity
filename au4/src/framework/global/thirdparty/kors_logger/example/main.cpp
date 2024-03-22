#include <iostream>
#include <thread>
#include <experimental/filesystem>
namespace fs = std::experimental::filesystem;

#ifdef KORS_LOGGER_QT_SUPPORT
#include <QDebug>
#include <QString>
#include <QPoint>
#endif

#include "logger.h"
#include "logstream.h"
#include "log.h"

namespace app {
struct MyType
{
    MyType (int v)
        : val(v) {}

    int val = 0;
};

inline app::logger::Stream& operator<<(app::logger::Stream& s, const MyType& t)
{
    s << t.val;
    return s;
}

class Example
{
public:
    Example() = default;

    void example()
    {
        logger::Logger* logger = logger::Logger::instance();
        logger->setupDefault();

        //! Default output to console, catch Qt messages (if supported)

        LOGE() << "This is error";
        LOGW() << "This is warning";
        LOGI() << "This is info";
        LOGD() << "This is debug"; //! NOTE Default not output

        std::thread t1([]() { LOGI() << "Info from thread"; });
        t1.join();

#ifdef KORS_LOGGER_QT_SUPPORT
        qCritical() << "This is qCritical";
        qWarning() << "This is qWarning";
        qDebug() << "This is qDebug"; //! NOTE Default not output

        LOGI() << QString("This is QString");
#endif

        LOGI() << "This is custom type: " << MyType(42);

        /*
        23:07:43.602 | ERROR | main_thread     | Example::example | This is error
        23:07:43.602 | WARN  | main_thread     | Example::example | This is warning
        23:07:43.602 | INFO  | main_thread     | Example::example | This is info
        23:07:43.602 | INFO  | 140147421083200 | Example::example | Info from thread
        23:07:43.602 | ERROR | main_thread     | Example::example | This is qCritical
        23:07:43.603 | WARN  | main_thread     | Example::example | This is qWarning
        23:07:43.603 | INFO  | main_thread     | Example::example | "This is QString"
        23:07:43.603 | INFO  | main_thread     | Example::example | This is custom type: 42
        */

        //! Using message formatting
        LOGD("This is formatted message, arg1: %d, arg2: %d, sum: %d", 40, 2, 42);
        /*
        23:07:43.603 | INFO  | main_thread     | Example::example | This is formatted message, arg1: 40, arg2: 2, sum: 42
        */

        //! Set tag (default class::func)

        #undef LOG_TAG
        #define LOG_TAG "MYTAG"

        LOGI() << "This is info with custorm tag";
        /*
        23:07:43.603 | INFO  | main_thread     | MYTAG           | This is info with custorm tag
        */

        //! Set log level
        logger->setLevel(logger::Level::Debug);

        LOGD() << "This is debug";
#ifdef KORS_LOGGER_QT_SUPPORT
        qDebug() << "This is qDebug (for Qt always tag is class::func)";
#endif

        /*
        23:07:43.603 | DEBUG | main_thread     | MYTAG           | This is debug
        23:07:43.603 | DEBUG | main_thread     | Example::example | This is qDebug (for Qt always tag is class::func)
        */

        //! --- Setup logger ---
        LOGI() << "Custom setup... ";
        //! Destination and format
        logger->clearDests();

        //! Console
        logger->addDest(new logger::ConsoleLogDest(logger::LogLayout("${time} | ${type|7} | ${thread} | ${tag|20} | ${message}")));

        //! File
        std::string pwd = fs::current_path();
        std::string logsDir = pwd + "/logs";
        if (!fs::exists(logsDir)) {
            fs::create_directories(logsDir);
        }
        std::string logPath = logsDir + "/myapp.log";
        logger->addDest(new logger::FileLogDest(logPath,
                                                logger::LogLayout("${datetime} | ${type|7} | ${thread} | ${tag|20} | ${message}")));

        /** NOTE Layout have a tags
        "${datetime}"   - yyyy-MM-ddThh:mm:ss.zzz
        "${time}"       - hh:mm:ss.zzz
        "${type}"       - type
        "${tag}"        - tag
        "${thread}"     - thread, the main thread output as "main_thread" otherwise thread id
        "${message}"    - message
        |N - min field width
         */

        LOGI() << "now log fields width is changed";
        /*
        23:07:43.603 | INFO  | main_thread     | MYTAG           | Custom setup...
        23:07:43.603 | INFO    | main_thread | MYTAG                | now log fields width is changed
        */

        //! NOTE Custom log layout can be used - inherits of the LogLayout with overridden method "output"
        //! NOTE Any custom destinations can be added - inherits of the LogDest with overridden method "write"

        //! Level
        logger->setLevel(logger::Level::Debug);

        //! Catch Qt message (if supported)
#ifdef KORS_LOGGER_QT_SUPPORT
        logger->setIsCatchQtMsg(true);
#endif

        //! Custom types
        logger->setType("MYTRACE", true);

        //! See custom macro in example log.h

        MYTRACE() << "This my trace";

        /*
        23:07:43.603 | MYTRACE | main_thread | MYTAG                | This my trace
        */

        //! Disable output for type
        logger->setType("MYTRACE", false); //! NOTE Type should be a debug level

        MYTRACE() << "This my trace"; //! NOTE Not output
    }
};
}

int main(int argc, char* argv[])
{
    std::cout << "Hello World, I am Logger\n";

    app::Example t;
    t.example();

#undef LOG_TAG
#define LOG_TAG CLASSFUNC

    LOGI() << "Goodbye!";
}
