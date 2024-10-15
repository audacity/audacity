/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    Vamp

    An API for audio analysis and feature extraction plugins.

    Centre for Digital Music, Queen Mary, University of London.
    Copyright 2006 Chris Cannam.
  
    Permission is hereby granted, free of charge, to any person
    obtaining a copy of this software and associated documentation
    files (the "Software"), to deal in the Software without
    restriction, including without limitation the rights to use, copy,
    modify, merge, publish, distribute, sublicense, and/or sell copies
    of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR
    ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
    CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
    WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

    Except as contained in this notice, the names of the Centre for
    Digital Music; Queen Mary, University of London; and Chris Cannam
    shall not be used in advertising or otherwise to promote the sale,
    use or other dealings in this Software without prior written
    authorization.
*/

/*
   This is a modified version of a source file from the 
   Rosegarden MIDI and audio sequencer and notation editor.
   This file copyright 2000-2006 Chris Cannam.
   Relicensed by the author as detailed above.
*/

#ifndef _VAMP_REAL_TIME_H_
#define _VAMP_REAL_TIME_H_

#include <iostream>
#include <string>

#ifndef _WIN32
struct timeval;
#endif

#include "plugguard.h"
_VAMP_SDK_PLUGSPACE_BEGIN(RealTime.h)

namespace Vamp {

/**
 * \class RealTime RealTime.h <vamp-sdk/RealTime.h>
 * 
 * RealTime represents time values to nanosecond precision
 * with accurate arithmetic and frame-rate conversion functions.
 */

struct RealTime
{
    int sec;
    int nsec;

    int usec() const { return nsec / 1000; }
    int msec() const { return nsec / 1000000; }

    RealTime(): sec(0), nsec(0) {}
    RealTime(int s, int n);

    RealTime(const RealTime &r) :
	sec(r.sec), nsec(r.nsec) { }

    static RealTime fromSeconds(double sec);
    static RealTime fromMilliseconds(int msec);

#ifndef _WIN32
    static RealTime fromTimeval(const struct timeval &);
#endif

    RealTime &operator=(const RealTime &r) {
	sec = r.sec; nsec = r.nsec; return *this;
    }

    RealTime operator+(const RealTime &r) const {
	return RealTime(sec + r.sec, nsec + r.nsec);
    }
    RealTime operator-(const RealTime &r) const {
	return RealTime(sec - r.sec, nsec - r.nsec);
    }
    RealTime operator-() const {
	return RealTime(-sec, -nsec);
    }

    bool operator <(const RealTime &r) const {
	if (sec == r.sec) return nsec < r.nsec;
	else return sec < r.sec;
    }

    bool operator >(const RealTime &r) const {
	if (sec == r.sec) return nsec > r.nsec;
	else return sec > r.sec;
    }

    bool operator==(const RealTime &r) const {
        return (sec == r.sec && nsec == r.nsec);
    }
 
    bool operator!=(const RealTime &r) const {
        return !(r == *this);
    }
 
    bool operator>=(const RealTime &r) const {
        if (sec == r.sec) return nsec >= r.nsec;
        else return sec >= r.sec;
    }

    bool operator<=(const RealTime &r) const {
        if (sec == r.sec) return nsec <= r.nsec;
        else return sec <= r.sec;
    }

    RealTime operator/(int d) const;

    /**
     * Return the ratio of two times.
     */
    double operator/(const RealTime &r) const;

    /**
     * Return a human-readable debug-type string to full precision
     * (probably not a format to show to a user directly)
     */ 
    std::string toString() const;

    /**
     * Return a user-readable string to the nearest millisecond
     * in a form like HH:MM:SS.mmm
     */
    std::string toText(bool fixedDp = false) const;

    /**
     * Convert a RealTime into a sample frame at the given sample rate.
     */
    static long realTime2Frame(const RealTime &r, unsigned int sampleRate);

    /**
     * Convert a sample frame at the given sample rate into a RealTime.
     */
    static RealTime frame2RealTime(long frame, unsigned int sampleRate);

    static const RealTime zeroTime;
};

std::ostream &operator<<(std::ostream &out, const RealTime &rt);

}

_VAMP_SDK_PLUGSPACE_END(RealTime.h)
    
#endif
