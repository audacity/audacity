/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#if defined(_WIN32) && defined(_DEBUG)

namespace au::app::winleaktracker {
// Enables the CRT leak check, redirects the CRT report sink to a file,
// installs an allocation hook that captures a call stack per allocation,
// and registers an at-exit dump of leaks aggregated by top call-stack
// frames. Writes two files to the current working directory:
//   - audacity_crt_leaks.log     (raw CRT dump, one line per unfreed block)
//   - audacity_leak_stacks.log   (our aggregated report with symbolicated
//                                  stacks, grouped by call site)
void install();
}

#else

namespace au::app::winleaktracker {
inline void install() {}
}

#endif
