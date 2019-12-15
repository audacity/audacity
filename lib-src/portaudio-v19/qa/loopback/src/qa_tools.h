
/*
 * PortAudio Portable Real-Time Audio Library
 * Latest Version at: http://www.portaudio.com
 *
 * Copyright (c) 1999-2010 Phil Burk and Ross Bencina
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files
 * (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge,
 * publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

/*
 * The text above constitutes the entire PortAudio license; however, 
 * the PortAudio community also makes the following non-binding requests:
 *
 * Any person wishing to distribute modifications to the Software is
 * requested to send the modifications to the original developer so that
 * they can be incorporated into the canonical version. It is also 
 * requested that these non-binding requests be included along with the 
 * license above.
 */

#ifndef _QA_TOOLS_H
#define _QA_TOOLS_H

extern int g_testsPassed;
extern int g_testsFailed;

#define QA_ASSERT_TRUE( message, flag ) \
	if( !(flag) ) \
	{ \
		printf( "%s:%d - ERROR - %s\n", __FILE__, __LINE__, message ); \
		g_testsFailed++; \
		goto error; \
	} \
	else g_testsPassed++;


#define QA_ASSERT_EQUALS( message, expected, actual ) \
	if( ((expected) != (actual)) ) \
	{ \
		printf( "%s:%d - ERROR - %s, expected %d, got %d\n", __FILE__, __LINE__, message, expected, actual ); \
		g_testsFailed++; \
		goto error; \
	} \
	else g_testsPassed++;

#define QA_ASSERT_CLOSE( message, expected, actual, tolerance ) \
	if (fabs((expected)-(actual))>(tolerance)) \
	{ \
		printf( "%s:%d - ERROR - %s, expected %f, got %f, tol=%f\n", __FILE__, __LINE__, message, ((double)(expected)), ((double)(actual)), ((double)(tolerance)) ); \
		g_testsFailed++; \
		goto error; \
	} \
	else g_testsPassed++;

#define QA_ASSERT_CLOSE_INT( message, expected, actual, tolerance ) \
    if (abs((expected)-(actual))>(tolerance)) \
    { \
        printf( "%s:%d - ERROR - %s, expected %d, got %d, tol=%d\n", __FILE__, __LINE__, message, ((int)(expected)), ((int)(actual)), ((int)(tolerance)) ); \
        g_testsFailed++; \
        goto error; \
    } \
    else g_testsPassed++;


#endif
