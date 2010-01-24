//////////////////////////////////////////////////////////////////////
//  iAVC -- integer Automatic Volume Control (on samples given it)
//
//	Copyright (C) 2002 Vincent A. Busam
//				  15754 Adams Ridge
//		  		  Los Gatos, CA 95033
//		  email:  vince@busam.com
//
//	This library is free software; you can redistribute it and/or
//	modify it under the terms of the GNU Lesser General Public
//	License as published by the Free Software Foundation; either
//	version 2.1 of the License, or (at your option) any later version.
//
//	This library is distributed in the hope that it will be useful,
//	but WITHOUT ANY WARRANTY; without even the implied warranty of
//	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//	Lesser General Public License for more details.
//
//	You should have received a copy of the GNU Lesser General Public
//	License along with this library; if not, write to the Free Software
//	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

//  If you change the algorithm or find other sets of values that improve the
//	output results, please send me a copy so that I can incorporate them into
//  iAVC.  Of course, you are not required to send me any updates under the
//	LGPL license, but please consider doing so under the spirit of open source
//	and in appreciation of being able to take advantage of my efforts expended
//  in developing iAVC.

//  This code implements a "poor man's" dynamic range compression algorithm
//	that was build on hueristics.  It's purpose is to perform dynamic range
//  compression in real time using only integer arithmetic.  Processing time
//  is more important than memory.

//  There are 3 window sizes that can be set:
//      Sample Window Size:  Total number of samples kept in a circular buffer.
//                              The caller can "delay" retrieving samples this
//                              long, although there is probably no reason to
//                              make this bigger than the Adjuster Window size
//                              plus the Lookahead Window size.
//      Adjuster Window Size: Total number of samples in the moving average
//                              that is used to determine the amplification
//                              multiplication factor.
//      Lookahead Window Size: A small window that is this many samples ahead
//                              of the Adjuster Window.  The
//                              moving average of this window is used to
//                              delay changing the multiplication factor for
//                              this many samples.  Helps avoid distorted
//                              transitions from loud to soft.  This is only
//                              useful if the caller delays retrieving samples
//                              by at least this many samples.  (The lookahead
//								window is currently NOT being used.)

//   |- oldest sample                                         newest sample -|
//   |                                                                       |
//   <----------------------- Sample Window --------------------------------->
//                                                       <-Lookahead  Window->  
//          <------------ Adjuster Window -------------->
//
//                                                                           ^
//                                         last sample put by PutNextSample -|
//
//                                                ^
//                                                |- last sample got by GetNextSample
//
//                                                <-- callers put/get delay ->

// The algorithms are safe for files with the number of samples <= 2,147,483,647
//      since 32 bit signed integer storage is used in some places.


// The define symbol "IAVC_INLINE" is used to make the attributes (data) outside
//      the class definition so the SetNextSample and PutNextSample methods can be
//      inline for faster processing in realtime environments.  (I don't trust
//      all C++ compilers to honor the inline directive so I'm forcing inline through
//      the use of define symbols.)  See DRCinlineSample.cpp for how to use define
//      symbols for inline code.

// The define symbol "IAVC_FLOAT" is used to perform calculations in floating point
//		instead of integer.  Floating point samples are expected to be in the range
//		or -1.0 to +1.0.  Note that the multiplier array size remains 
//		MULTIPLY_PCT_ARRAY_SIZE.

#ifndef _IAVC_H_
#define _IAVC_H_

#ifndef   IAVC_INLINE
    // prepare other defines for code inclusion, since not inline
    #define  IAVC_SETNEXTSAMPLE
    #define  IAVC_GETNEXTSAMPLE
    #define  IAVC_ADJUSTMULTIPLIER
#else
    #undef   IAVC_SETNEXTSAMPLE
    #undef   IAVC_GETNEXTSAMPLE
    #undef   IAVC_ADJUSTMULTIPLIER
#endif

#define  MAX_INTEGER_SAMPLE_VALUE ( 32767 )		// for 16 bit samples
#define  MULTIPLY_PCT_ARRAY_SIZE  ( MAX_INTEGER_SAMPLE_VALUE + 2 )

#define  DEFAULT_MINIMUM_SAMPLES_BEFORE_SWITCH  	1100
#define  MIN_MINIMUM_SAMPLES_BEFORE_SWITCH             1

#define  DEFAULT_ADJUSTER_WINDOW_SIZE				2200
#define  DEFAULT_LOOKAHEAD_WINDOW_SIZE                 0

#define  DEFAULT_SAMPLE_WINDOW_SIZE				( DEFAULT_ADJUSTER_WINDOW_SIZE + DEFAULT_LOOKAHEAD_WINDOW_SIZE )
#define  MAX_SAMPLE_WINDOW_SIZE                    32767


#define  DEFAULT_MAX_PCT_CHANGE_AT_ONCE               25    // 25%, not used if == 0

#define  DEFAULT_NUMBER_OF_TRACKS                      2
#define  MAX_NUMBER_OF_TRACKS                          2

#include <stdio.h>

#ifndef NULL
  #define NULL 0
#endif

#ifndef AfxMessageBox
  #define AfxMessageBox( pText ) { fprintf(stderr,"MESSAGE: %s\n",pText); };
#endif

#ifndef maxVal
  #define maxVal(a,b)  ( (a<b)?b:a )
#endif

#ifndef absVal
  #define absVal(a)    ( (a<0)?-a:a )
#endif


#ifdef IAVC_FLOAT
//#pragma message("iAVC using floating point samples")
	typedef float IAVCSAMPLETYPE;
	typedef float IAVCSUMTYPE;
	typedef float IAVCMULTIPLYPCT;
	#define APPLY_MULTIPLY_FACTOR(x)		(x)
	#define UNDO_MULTIPLY_FACTOR(x)			(x)
	#define AVG_TO_MULTIPLIER_SUBSCRIPT(x)	long(x*MAX_INTEGER_SAMPLE_VALUE)
	#define MULTIPLY_FACTOR_TO_INT_X256(x)	(int(x*256))
	#define DEFAULT_MAX_SAMPLE_VALUE		1  // values range from -1 to 1 for float
	#define DEFAULT_MIN_SAMPLE_VALUE		-1 // values range from -1 to 1 for float
	#define IF_CLIP(x)						( absVal(x) > m_nMaxSampleValue )
#else
//#pragma message("iAVC using short int samples")
	typedef short int IAVCSAMPLETYPE;
	typedef long IAVCSUMTYPE;
	typedef long IAVCMULTIPLYPCT;
	#define APPLY_MULTIPLY_FACTOR(x)		(long(x)<<8)
	#define UNDO_MULTIPLY_FACTOR(x)			(long(x)>>8)
	#define AVG_TO_MULTIPLIER_SUBSCRIPT(x)  (x)
	#define MULTIPLY_FACTOR_TO_INT_X256(x)	(x)
	#define DEFAULT_MAX_SAMPLE_VALUE		32767  // for 16 bit samples, -32767 to 32767
	#define DEFAULT_MIN_SAMPLE_VALUE		-32768 // for 16 bit samples, -32767 to 32767
	#define IF_CLIP(x)						( x != IAVCSAMPLETYPE ( x ) )
#endif

struct Sample;

class AutoVolCtrl
{
public:
	AutoVolCtrl();			 // standard constructor

	virtual ~AutoVolCtrl(); // destructor

	void Reset();			// reset to default values  (can call between tracks, all settings saved)

	// Initialization methods (Set Sample Window Size BEFORE setting Min Samples Before Switch)
	//							Min Samples Before Switch must be < Sample Window Size
	// window size <= MAX_SAMPLE_WINDOW_SIZE
	bool SetSampleWindowSize ( unsigned long nSampleWindowSize, 
                               unsigned long nAdjusterWindowSize,
                               unsigned long nLookAheadWindowSize );	
	bool SetMinSamplesBeforeSwitch ( unsigned long nMinSamplesBeforeSwitch );
	void SetMaxPctChangeAtOnce ( IAVCMULTIPLYPCT nPctChange );  // in %, e.g. 10 for 10%
	void SetMultipliers ( unsigned short int nValueWanted [ MULTIPLY_PCT_ARRAY_SIZE ] );
							// e.g. if a sample with value 10000 is to be changed to be 20000, then
							//		nValueWanted [ 10000 ] = 20000;
							// a nil transform is when every array element's value is its subscript
							// 
	bool SetNumberTracks ( unsigned int nNumTracks );	// currently only 1 or 2 tracks supported

	// Processing samples.  In version 1 you MUST do a SetNextSample followed by a GetNextSample
    //                      If only one track the right sample must = 0.
	bool SetNextSample ( IAVCSAMPLETYPE left, IAVCSAMPLETYPE right );		    // return true if AOK
	bool GetNextSample ( IAVCSAMPLETYPE & left, IAVCSAMPLETYPE & right );		// return true if AOK

protected:

    void	AdjustMultiplier();
    void    ZeroSampleWindow();

#ifdef IAVC_INLINE   
    
};          // end class definition here if not C++ (make data definitions below outside of class
#pragma message("iAVC using inline methods")

#endif

    struct Sample
    {
	    Sample*	        m_pNext;		// one entry points to the next, last entry to first entry
	    Sample*	        m_pAvgPartner;	// node "m_nSamplesInAvg" (i.e. adjuster size) before this node
        Sample*         m_pLookaheadPartner; // node "m_nLookAheadWindowSize" before this node

	    IAVCSAMPLETYPE	m_nLeft;
	    IAVCSAMPLETYPE	m_nRight;
	    long			m_nSampleValid;     // =1 if node contains a sample value, =0 if no value
        IAVCSUMTYPE     m_nSampleAbsAvg;    // ( abs(left) + abs(right) ) / num_tracks, zero if not valid
    };

	// Following are parameters whose values are provided by caller
	unsigned long	m_nSampleWindowSize;			// size of window of samples to keep
	unsigned long   m_nSamplesInAvg;				// <= m_nSampleWindowSize
    unsigned long   m_nLookAheadWindowSize;         // <= m_nMinSamplesBeforeSwitch & <= m_nSampleWindowSize
	unsigned long	m_nMinSamplesBeforeSwitch;		// minimum number of samples between multiplier changes
    IAVCMULTIPLYPCT m_nMaxChangePct;                // maximum % change in multiplier at a time
	IAVCMULTIPLYPCT	m_nMultiplyPct [ MULTIPLY_PCT_ARRAY_SIZE ];	// desired multiplier % for each sample value
	unsigned long	m_nNumTracks;					// = 2 for stereo, = 1 for mono
	IAVCSUMTYPE     m_nMaxSampleValue;				// e.g. 32767 for 16 bit

	// Following are internal attributes
	IAVCSUMTYPE  	m_nSampleAvgSum;			// sum of sound samples in current sample window
	unsigned long	m_nSamplesInSum;			// number of samples in m_nSampleAvgSum ( <= m_nSamplesInAvg )
	IAVCMULTIPLYPCT	m_nCurrentMultiplier;		// current % multiplier for sample
	Sample*     	m_pNextSet;					// next node for SetNextSample
	Sample*     	m_pNextGet;					// next node for GetNextSample
    IAVCSUMTYPE     m_nLookaheadSum;            // sum of lookahead samples
    unsigned int    m_nSamplesInLookahead;      // number of samples in m_nLookaheadSum
    signed long     m_nNumSamplesBeforeNextSwitch; // number of samples before next switch

    Sample *		m_pSampleList;				// array of samples

	// Following are internal attributes for diagnostics
	long			m_nTotalSamples;
	long			m_nNumMultiplerChanges;
	long			m_nClips;

#ifndef IAVC_INLINE

};

#endif      // end class definition here if C++


#endif  // _IAVC_H_
