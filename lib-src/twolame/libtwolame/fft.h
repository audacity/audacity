/*
 *  TwoLAME: an optimized MPEG Audio Layer Two encoder
 *
 *  Copyright (C) 2001-2004 Michael Cheng
 *  Copyright (C) 2004-2006 The TwoLAME Project
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  $Id$
 *
 */


#ifndef	_FFT_H_
#define	_FFT_H_

//void fft (FLOAT[BLKSIZE], FLOAT[BLKSIZE], FLOAT[BLKSIZE], FLOAT[BLKSIZE], int);

void psycho_2_fft(FLOAT * x_real, FLOAT * energy, FLOAT * phi);
void psycho_1_fft(FLOAT * x_real, FLOAT * energy, int N);


#endif


// vim:ts=4:sw=4:nowrap: 
