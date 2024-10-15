/*
 *	TwoLAME: an optimized MPEG Audio Layer Two encoder
 *
 *	Copyright (C) 2001-2004 Michael Cheng
 *	Copyright (C) 2004-2006 The TwoLAME Project
 *
 *	This library is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU Lesser General Public
 *	License as published by the Free Software Foundation; either
 *	version 2.1 of the License, or (at your option) any later version.
 *
 *	This library is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *	Lesser General Public License for more details.
 *
 *	You should have received a copy of the GNU Lesser General Public
 *	License along with this library; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  $Id$
 *
 */


#ifndef _CRITBAND_H_
#define _CRITBAND_H_

static const int SecondCriticalBand[7][28] = {
    {
     /* 2cb0, 27 entries */
     27,
     1,
     2,
     3,
     5,
     7,
     10,
     13,
     16,
     19,
     22,
     26,
     30,
     35,
     40,
     46,
     54,
     64,
     76,
     90,
     104,
     124,
     148,
     176,
     216,
     264,
     360,
     464},
    {
     /* 2cb1, 27 entries */
     27,
     1,
     2,
     3,
     5,
     7,
     9,
     12,
     14,
     17,
     20,
     24,
     27,
     32,
     37,
     42,
     50,
     58,
     70,
     82,
     100,
     116,
     136,
     164,
     200,
     248,
     328,
     432},
    {
     /* 2cb2, 25 entries */
     25,
     1,
     3,
     6,
     10,
     13,
     17,
     21,
     25,
     30,
     35,
     41,
     47,
     54,
     64,
     74,
     88,
     104,
     124,
     148,
     176,
     208,
     248,
     296,
     368,
     480,
     0, 0},
    {
     /* 2cb3, 0 entries (all dummies) */
     0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0},
    {
     /* 2cb4, 24 entries */
     24,
     1,
     5,
     9,
     14,
     19,
     25,
     30,
     36,
     43,
     50,
     58,
     68,
     78,
     92,
     108,
     124,
     148,
     176,
     208,
     248,
     296,
     352,
     424,
     480,
     0, 0, 0},
    {
     /* 2cb5, 24 entries */
     24,
     1,
     4,
     9,
     13,
     18,
     23,
     28,
     33,
     39,
     46,
     54,
     62,
     72,
     84,
     100,
     116,
     136,
     164,
     192,
     232,
     272,
     328,
     392,
     480,
     0, 0, 0},
    {
     /* 2cb6, 22 entries */
     22,
     1,
     6,
     13,
     20,
     27,
     34,
     42,
     50,
     60,
     70,
     80,
     94,
     108,
     124,
     148,
     172,
     208,
     248,
     288,
     344,
     408,
     480,
     0, 0, 0, 0, 0}
};

#endif


// vim:ts=4:sw=4:nowrap: 
