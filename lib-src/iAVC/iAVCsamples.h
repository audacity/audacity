//////////////////////////////////////////////////////////////////////
//  iAVC -- integer Automatic Volume Control -- Sample transformations for use with iAVC
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


//-------------------------------
// nil transform.  Useful for testing.

static int iHoriz_1K_1K[] = { 0, 1000, 13000, 32768, 99999 };  // leave first and last two pairs
static int iVert_1K_1K [] = { 0, 1000, 13000, 32768, 99999 };  //		of values alone


//-------------------------------
// Heavy amplification in low volumes, linear when sound is louder.
// Doesn't turn linear until quite loud, which contributes to more clipping.

// 	-100 db -> -100 db	expand 2:1 below -60 db			0 -> 0
//	 -60 db ->  -20 db  compress 4.33:1 below -8 db		1,000 -> 3,500
//	  -8 db ->   -8 db	flat 1:1 above -8 db			13,000 -> 13,000
//	   0 db ->    0 db	
static int iHoriz_1K_3HK[] = { 0, 1000, 13000, 32768, 99999 };  // leave first and last two pairs
static int iVert_1K_3HK [] = { 0, 3500, 13000, 32768, 99999 };  //		of values alone


//-------------------------------
// Another version with heavy amplication in low volumes.  
// based on x^0.75 from 0 to 5,000 then flat
// More points make for a smoother curve but more multiplier changes.

static int iHoriz_E75_5K[] = { 
	0,  50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950,1000,1050,1100,1150,1200,1250,1300,1350,1400,1450,1500,1550,1600,1650,1700,1750,1800,1850,1900,1950,2000,2050,2100,2150,2200,2250,2300,2350,2400,2450,2500,2550,2600,2650,2700,2750,2800,2850,2900,2950,3000,3050,3100,3150,3200,3250,3300,3350,3400,3450,3500,3550,3600,3650,3700,3750,3800,3850,3900,3950,4000,4050,4100,4150,4200,4250,4300,4350,4400,4450,4500,4550,4600,4650,4700,4750,4800,4850,4900,4950,5000
	,32768, 99999 };
static int iVert_E75_5K [] = { 
	0,1581,1880,2081,2236,2364,2475,2572,2659,2739,2812,2880,2943,3002,3058,3112,3162,3211,3257,3301,3344,3385,3424,3463,3500,3536,3570,3604,3637,3669,3700,3731,3761,3790,3818,3846,3873,3900,3926,3951,3976,4001,4025,4049,4072,4095,4118,4140,4162,4183,4204,4225,4246,4266,4286,4306,4325,4344,4363,4382,4401,4419,4437,4455,4472,4490,4507,4524,4540,4557,4573,4590,4606,4622,4637,4653,4668,4684,4699,4714,4729,4743,4758,4772,4787,4801,4815,4829,4843,4856,4870,4883,4897,4910,4923,4936,4949,4962,4975,4987,5000
	,32768, 99999 };
//multipliers 
//  1,31.6,18.8,13.9,11.2, 9.5, 8.2 ,7.3, 6.6, 6.1, 5.6, 5.2, 4.9, 4.6, 4.4, 4.1, 4.0, 3.8, 3.6, 3.5, 3.3,3.2,3.1,3.0,2.9,2.8,2.7,2.7,2.6,2.5,2.5,2.4,2.4,2.3,2.2,2.2,2.2,2.1,2.1,2.0,2.0,2.0,1.9,1.9,1.9,1.8,1.8,1.8,1.7,1.7,1.7,1.7,1.6,1.6,1.6,1.6,1.5,1.5,1.5,1.5,1.5,1.4,1.4,1.4,1.4,1.4,1.4,1.4,1.3,1.3,1.3,1.3,1.3,1.3,1.3,1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.1,1.1,1.1,1.1,1.1,1.1,1.1,1.1,1.1,1.1,1.1,1.0,1.0,1.0,1.0,1.0,1.0,1.0


//-------------------------------
// Another version with heavy amplication in low volumes.  
// based on x^0.75 from 0 to 3,500 then flat
// More points make for a smoother curve but more multiplier changes.

static int iHoriz_75_3500[] = { 
	0,  50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950,1000,1050,1100,1150,1200,1250,1300,1350,1400,1450,1500,1550,1600,1650,1700,1750,1800,1850,1900,1950,2000,2050,2100,2150,2200,2250,2300,2350,2400,2450,2500,2550,2600,2650,2700,2750,2800,2850,2900,2950,3000,3050,3100,3150,3200,3250,3300,3350,3400,3450,3500
	,32768, 99999 };
static int iVert_75_3500 [] = { 
	0,1210,1439,1592,1711,1809,1894,1968,2035,2096,2152,2204,2252,2298,2341,2381,2420,2457,2492,2526,2559,2590,2621,2650,2678,2706,2732,2758,2783,2808,2832,2855,2878,2900,2922,2943,2964,2984,3004,3024,3043,3062,3080,3099,3116,3134,3151,3168,3185,3201,3218,3234,3249,3265,3280,3295,3310,3325,3339,3354,3368,3382,3395,3409,3422,3436,3449,3462,3475,3487,3500
	,32768, 99999 };
//multipliers 
//  1,24.2,14.4,10.6, 8.6, 7.2, 6.3, 5.6, 5.1, 4.7, 4.3, 4.0, 3.8, 3.5, 3.3, 3.2, 3.0, 2.9, 2.8, 2.7, 2.6, 2.5, 2.4, 2.3, 2.2, 2.2, 2.1, 2.0, 2.0, 1.9, 1.9, 1.8, 1.8, 1.8, 1.7, 1.7, 1.6, 1.6, 1.6, 1.6, 1.5, 1.5, 1.5, 1.4, 1.4, 1.4, 1.4, 1.3, 1.3, 1.3, 1.3, 1.3, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.0, 1.0, 1.0, 1.0, 1.0,


//-------------------------------
// Another version with heavy amplication in low volumes.  
// based on x^0.75 from 0 to 3,500 then flat
// Fewer point to reduce number of multiplier changes.

static int iHoriz_AE75_3HK[] = { 0,  150,  300,  450,  650, 2500, 32768, 99999 };  // leave first and last two pairs
static int iVert_AE75_3HK [] = { 0, 1592, 1894, 2096, 2298, 3218, 32768, 99999 };  //		of values alone

//static int iHoriz_AE75_3HK[] = { 0,  300,  600, 1000, 2500, 3500, 32768, 99999 };  // leave first and last two pairs
//static int iVert_AE75_3HK [] = { 0, 1900, 2250, 2500, 3100, 3500, 32768, 99999 };  //		of values alone
