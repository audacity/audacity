#ifndef EXAMPLE_LOG_H
#define EXAMPLE_LOG_H

#include "../src/log_base.h" // kors
#include "logger.h" // example

// custom macros

#define MYTRACE IF_LOGLEVEL(app::logger::Level::Debug) LOG_STREAM("MYTRACE", LOG_TAG, app::logger::Color::Magenta)

#endif // EXAMPLE_LOG_H
