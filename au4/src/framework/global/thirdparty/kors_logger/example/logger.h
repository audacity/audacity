#ifndef EXAMPLE_LOGGER_H
#define EXAMPLE_LOGGER_H

#include "../src/logger.h" // kors
#include "../src/logdefdest.h" // kors

namespace app::logger {
using Logger = kors::logger::Logger;
using Type = kors::logger::Type;
using Level = kors::logger::Level;
using Color = kors::logger::Color;
using LogLayout = kors::logger::LogLayout;
using ConsoleLogDest = kors::logger::ConsoleLogDest;
using FileLogDest = kors::logger::FileLogDest;
}

#endif // EXAMPLE_LOGGER_H
