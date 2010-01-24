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

#include <iostream>

#if (__GNUC__ < 3)
#include <strstream>
#define stringstream strstream
#else
#include <sstream>
#endif

using std::cerr;
using std::endl;

#ifndef _WIN32
#include <sys/time.h>
#endif

#include <vamp-sdk/RealTime.h>

_VAMP_SDK_PLUGSPACE_BEGIN(RealTime.cpp)

namespace Vamp {

// A RealTime consists of two ints that must be at least 32 bits each.
// A signed 32-bit int can store values exceeding +/- 2 billion.  This
// means we can safely use our lower int for nanoseconds, as there are
// 1 billion nanoseconds in a second and we need to handle double that
// because of the implementations of addition etc that we use.
//
// The maximum valid RealTime on a 32-bit system is somewhere around
// 68 years: 999999999 nanoseconds longer than the classic Unix epoch.

#define ONE_BILLION 1000000000

RealTime::RealTime(int s, int n) :
    sec(s), nsec(n)
{
    if (sec == 0) {
	while (nsec <= -ONE_BILLION) { nsec += ONE_BILLION; --sec; }
	while (nsec >=  ONE_BILLION) { nsec -= ONE_BILLION; ++sec; }
    } else if (sec < 0) {
	while (nsec <= -ONE_BILLION) { nsec += ONE_BILLION; --sec; }
	while (nsec > 0)             { nsec -= ONE_BILLION; ++sec; }
    } else { 
	while (nsec >=  ONE_BILLION) { nsec -= ONE_BILLION; ++sec; }
	while (nsec < 0)             { nsec += ONE_BILLION; --sec; }
    }
}

RealTime
RealTime::fromSeconds(double sec)
{
    return RealTime(int(sec), int((sec - int(sec)) * ONE_BILLION + 0.5));
}

RealTime
RealTime::fromMilliseconds(int msec)
{
    return RealTime(msec / 1000, (msec % 1000) * 1000000);
}

#ifndef _WIN32
RealTime
RealTime::fromTimeval(const struct timeval &tv)
{
    return RealTime(tv.tv_sec, tv.tv_usec * 1000);
}
#endif

std::ostream &operator<<(std::ostream &out, const RealTime &rt)
{
    if (rt < RealTime::zeroTime) {
	out << "-";
    } else {
	out << " ";
    }

    int s = (rt.sec < 0 ? -rt.sec : rt.sec);
    int n = (rt.nsec < 0 ? -rt.nsec : rt.nsec);

    out << s << ".";

    int nn(n);
    if (nn == 0) out << "00000000";
    else while (nn < (ONE_BILLION / 10)) {
	out << "0";
	nn *= 10;
    }
    
    out << n << "R";
    return out;
}

std::string
RealTime::toString() const
{
    std::stringstream out;
    out << *this;
    
#if (__GNUC__ < 3)
    out << std::ends;
#endif

    std::string s = out.str();

    // remove trailing R
    return s.substr(0, s.length() - 1);
}

std::string
RealTime::toText(bool fixedDp) const
{
    if (*this < RealTime::zeroTime) return "-" + (-*this).toText();

    std::stringstream out;

    if (sec >= 3600) {
	out << (sec / 3600) << ":";
    }

    if (sec >= 60) {
	out << (sec % 3600) / 60 << ":";
    }

    if (sec >= 10) {
	out << ((sec % 60) / 10);
    }

    out << (sec % 10);
    
    int ms = msec();

    if (ms != 0) {
	out << ".";
	out << (ms / 100);
	ms = ms % 100;
	if (ms != 0) {
	    out << (ms / 10);
	    ms = ms % 10;
	} else if (fixedDp) {
	    out << "0";
	}
	if (ms != 0) {
	    out << ms;
	} else if (fixedDp) {
	    out << "0";
	}
    } else if (fixedDp) {
	out << ".000";
    }
	
#if (__GNUC__ < 3)
    out << std::ends;
#endif

    std::string s = out.str();

    return s;
}


RealTime
RealTime::operator/(int d) const
{
    int secdiv = sec / d;
    int secrem = sec % d;

    double nsecdiv = (double(nsec) + ONE_BILLION * double(secrem)) / d;
    
    return RealTime(secdiv, int(nsecdiv + 0.5));
}

double 
RealTime::operator/(const RealTime &r) const
{
    double lTotal = double(sec) * ONE_BILLION + double(nsec);
    double rTotal = double(r.sec) * ONE_BILLION + double(r.nsec);
    
    if (rTotal == 0) return 0.0;
    else return lTotal/rTotal;
}

long
RealTime::realTime2Frame(const RealTime &time, unsigned int sampleRate)
{
    if (time < zeroTime) return -realTime2Frame(-time, sampleRate);
    double s = time.sec + double(time.nsec + 1) / 1000000000.0;
    return long(s * sampleRate);
}

RealTime
RealTime::frame2RealTime(long frame, unsigned int sampleRate)
{
    if (frame < 0) return -frame2RealTime(-frame, sampleRate);

    RealTime rt;
    rt.sec = frame / long(sampleRate);
    frame -= rt.sec * long(sampleRate);
    rt.nsec = (int)(((double(frame) * 1000000.0) / sampleRate) * 1000.0);
    return rt;
}

const RealTime RealTime::zeroTime(0,0);

}

_VAMP_SDK_PLUGSPACE_END(RealTime.cpp)



