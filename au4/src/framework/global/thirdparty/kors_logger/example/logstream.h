#ifndef EXAMPLE_LOGSTREAM_H
#define EXAMPLE_LOGSTREAM_H

#include "../src/logstream.h" // kors

namespace app::logger {
using Stream = kors::logger::Stream;
}

/*
Add stream operator for your type
inline app::logger::Stream& operator<<(app::logger::Stream& s, const SomeType& v)
{
    s << v.toStdString();
    return s;
}
*/

#endif // EXAMPLE_LOGSTREAM_H
