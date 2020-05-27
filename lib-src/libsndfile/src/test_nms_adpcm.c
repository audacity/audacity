/*
** Copyright (C) 2007-2018 Erik de Castro Lopo <erikd@mega-nerd.com>
** Copyright (C) 2017-2018 Arthur Taylor <art@ified.ca>
**
** This library is free software; you can redistribute it and/or modify it
** under the terms of the GNU Lesser General Public License as published by
** the Free Software Foundation; either version 2 of the License, or (at
** your option) any later version.
**
** This library is distributed in the hope that it will be useful, but
** WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
** General Public License for more details.
**
** You should have received a copy of the GNU Lesser General Public License
** along with this library.  If not, write to the Free Software Foundation,
** Fifth Floor, 51 Franklin Street, Boston, MA 02111-1301, USA.
*/

#include "sfconfig.h"

#include <stdio.h>

#include "test_main.h"

#include "nms_adpcm.c"

static const short pcm_data_src [] =
{	505, 743, 805, 409, 101, -552, -709, -679,
	-624, -1174, -1050, 32, -401, -769, -679, 47,
	-3273, -4425, -2158, -176, 824, 1057, 1245, 805,
	2414, 3282, 1739, -624, -1197, -1663, -913, 603,
	549, -614, 707, 3314, 2864, 1127, -294, -919,
	-1251, -147, 30, -750, -131, 1394, 2197, 1829,
	1387, 417, 391, 533, 581, 179, -210, -210,
	-884, -937, -1373, -1338, -1811, -2727, -2536, -1552,
	-651, -3556, -7713, -9083, -6182, 1070, 4983, 5341,
	4596, 4682, 6488, 5197, 2401, -2702, -5261, -4036,
	-1995, 463, 1056, 2436, 3238, 4395, 4478, 1130,
	-2383, -4349, -4547, -3631, -1396, 1029, 2589, 3948,
	4966, 4312, 2362, 492, -1289, -2259, -1659, -597,
	239, 1433, 2353, 2512, 1763, 610, -291, -640,
	7, 132, 432, 438, -1070, -1202, -1071, -1841,
	-462, 225, -4465, -11313, -10492, -4522, -2096, -7013,
	-11770, -2546, 7687, 12765, 12014, 5324, 1645, 1191,
	3800, -187, -6689, -7778, -4631, 2487, 7352, 7928,
	4317, 2424, 3784, 2301, -1713, -6668, -8345, -6453,
	-2303, 2269, 3232, 4114, 5054, 5054, 3768, 1060,
	-1793, -3302, -2059, -86, 1153, 1690, 2869, 3841,
	3551, 1919, -197, -1391, -847, 128, 746, 1111,
	431, 559, 1086, 138, -1539, -2758, -1886, 1351,
	2407, -1883, -8356, -10999, -9917, -7329, -4295, -3209,
	-11616, -15352, 1032, 12603, 13233, 9059, 4019, 1858,
	3368, 7454, -56, -8600, -7278, -818, 5478, 7039,
	5630, 1186, 1634, 5422, 2518, -3182, -8271, -7889,
	-4399, -129, 3205, 2933, 3661, 5886, 6543, 3798,
	374, -2722, -3378, -1804, -24, 385, 1663, 3595,
	4749, 3865, 1402, -851, -1932, -1394, -725, -219,
	290, 658, 1074, 1638, 536, 204, -340, 408,
	1835, 1261, -2872, -4840, -5978, -8177, -7644, -6554,
	-8093, -6174, -7796, -17019, -12355, 1280, 12576, 11868,
	10710, 8578, 5605, 9675, 7123, -977, -8770, -6740,
	-1327, 2905, 6386, 5026, 3809, 5137, 6392, 2463,
	-4924, -8830, -9572, -6122, -1608, 1677, 3379, 5660,
	8236, 7225, 4470, 295, -2628, -3572, -2107, -666,
	951, 3101, 5049, 4759, 2367, -140, -2078, -2471,
	-2332, -1547, -798, 410, 1825, 3329, 3092, 352,
	-3310, -3307, -1229, -415, 532, 2091, 465, -1430
} ;

/* pcm_data encoded as 16kbs from a known reference */
static const unsigned short test_codes_16 [] =
{	0x5777, 0xfff0, 0xdcd0, 0x672d, 0x1826, 0xc11c, 0x0822, 0xffee,
	0x3ddc, 0x6372, 0x0116, 0xc8d8, 0x6780, 0x8624, 0x3323, 0x33ef,
	0xd865, 0x4cd8, 0x3372, 0x1096, 0x0049, 0xa911, 0x1288, 0xa74d,
	0x3fee, 0xcc45, 0x52de, 0x6a72, 0x9118, 0xe291, 0x60a2, 0x3164,
	0x73fe, 0xeddf, 0x57b5, 0x185a, 0xe889, 0x460e, 0x2646, 0x8d87,
	0xe5ba, 0x004c
} ;

/* pcm_data encoded as 24kbs from a known reference */
static const unsigned short test_codes_24 [] =
{	0x7776, 0x2fec, 0xceb0, 0xffd0, 0x3241, 0x650a, 0x0a26, 0x61ba,
	0xa10b, 0x3912, 0x39a8, 0xebfa, 0x1fff, 0x8552, 0x2342, 0x0204,
	0x454b, 0xccbb, 0x4318, 0xaa00, 0x1642, 0x3031, 0xfc8f, 0x38ff,
	0xf604, 0x4924, 0x2ddb, 0x0469, 0xbcaa, 0x83b6, 0x0049, 0x8828,
	0x2266, 0x3801, 0x873d, 0xcb86, 0x0eff, 0xef64, 0xd402, 0x44fa,
	0x2867, 0xd1d0, 0xa109, 0x2a11, 0x8a64, 0x4018, 0x1357, 0xd5a5,
	0x4bfc, 0xcbfe, 0x070a, 0x6307, 0x1858, 0x624b, 0xf9a9, 0x783b,
	0x0880, 0x1652, 0xc893, 0x641c, 0xf30d, 0x004c
} ;

/* pcm_data encoded as 32kbs from a known reference */
static const unsigned short test_codes_32 [] =
{	0x7772, 0x0cdc, 0xbec2, 0xacb2, 0xff90, 0x1220, 0x551c, 0xcc84,
	0x2c47, 0x30aa, 0xa10b, 0x0663, 0x2812, 0x28a9, 0xf9ba, 0xceb9,
	0x1fec, 0x9553, 0x2361, 0x9ed8, 0x8314, 0x564b, 0xddba, 0x1346,
	0x6308, 0xab00, 0x0721, 0x2908, 0x3820, 0xf89c, 0x38ff, 0xa2bf,
	0xc535, 0x2933, 0x5de9, 0x8633, 0x8569, 0xbeca, 0x1186, 0x5528,
	0xd000, 0xaa21, 0x0473, 0x2800, 0x1112, 0xa64d, 0xdc17, 0x8eeb,
	0xccac, 0xfe74, 0xc501, 0x63f9, 0x2040, 0x3a73, 0xc9b9, 0x9188,
	0x7318, 0x0a81, 0x9a65, 0x5188, 0x00ba, 0x2256, 0xd5b6, 0x4bfa,
	0xbeac, 0xe8fe, 0x343b, 0x7117, 0x9ca4, 0x915a, 0x563d, 0xcad0,
	0xa837, 0x302a, 0x1a2a, 0x3561, 0x98a9, 0xb9b5, 0x578a, 0xc48b,
	0x25f0, 0x1000
} ;


/* test_codes_16 decoded by a known reference */
const short pcm_data_out16 [] =
{	12, 16, 24, 36, 52, -68, -104, -156,
	-224, -309, -433, 12, -449, -618, -851, 32,
	-871, -1176, -1586, 60, 1172, 634, 1566, 983,
	1995, 2586, 1718, -1152, -815, -2313, -1610, 1261,
	1056, -253, 522, 1799, 2506, 1518, 72, -329,
	-1510, -76, 337, -1144, 68, 1369, 2200, 1337,
	1016, 60, 405, 461, 433, 389, -36, -164,
	-550, -871, -1212, -1626, -2136, -2791, -2064, -1642,
	-485, -1566, -2538, -3445, -4481, 650, 4381, 3799,
	5807, 4742, 6674, 5590, 2072, -2228, -5650, -4983,
	-1698, 441, 1333, 3064, 2855, 3815, 5108, 2156,
	-2228, -3321, -5028, -4405, -1550, 771, 3232, 3273,
	4008, 5008, 2024, 859, -654, -2746, -1694, -136,
	68, 1409, 1759, 2453, 2016, 522, -514, -445,
	0, 305, 493, 518, -232, -1076, -1116, -1321,
	-506, 365, -140, -1132, -2076, -2895, -2357, -2477,
	-3325, -2859, 666, 4449, 7164, 6244, 1847, 1365,
	3827, -779, -7682, -8951, -3811, 1718, 6566, 7120,
	4674, 1959, 1819, 2032, -1104, -5220, -8518, -7626,
	-2385, 2714, 3510, 3871, 4831, 4024, 4156, 1590,
	-1694, -3437, -2393, 96, 959, 1847, 2775, 3638,
	3072, 1734, -204, -1730, -718, -92, 453, 807,
	220, 514, 1349, -40, -1285, -2477, -1566, 1273,
	2586, 546, -2887, -5534, -6883, -7461, -5281, -2224,
	-2361, -5104, -48, 9228, 12140, 9048, 3614, 1927,
	4618, 6004, -148, -9871, -5582, -489, 6835, 6746,
	6839, 2851, 3028, 4566, 1461, -2028, -6883, -7642,
	-5321, -610, 3385, 3461, 3088, 3389, 4570, 4321,
	-389, -2630, -3369, -1706, -136, 220, 1594, 3024,
	4622, 4232, 1265, -943, -2273, -1638, -726, -232,
	365, 538, 995, 1530, 289, 453, -68, 12,
	1184, 1562, 92, -2558, -4859, -6277, -7096, -5461,
	-4811, -6020, -8851, -12594, -11501, -943, 12927, 10449,
	8935, 10389, 5662, 5755, 9108, 1827, -10224, -7807,
	-148, 3429, 7722, 5212, 4734, 3847, 5570, 3433,
	-3931, -8244, -8461, -5397, -1710, 1919, 3787, 4558,
	5040, 5722, 4811, -441, -3140, -4180, -2397, -493,
	1309, 3064, 4116, 5040, 2759, -730, -2445, -2847,
	-2080, -1682, -1124, 706, 2032, 3325, 3248, 425,
	-3586, -2987, -1397, -188, 144, 1506, 4, -2028
} ;

/* test_codes_24 decoded by a known reference */
static const short pcm_data_out24 [] =
{	16, 32, 68, 140, 116, -232, -510, -650,
	-771, -1329, -1052, -152, -317, -907, -710, -104,
	-1144, -2132, -2598, -301, 662, 827, 1469, 702,
	2401, 2987, 1574, -244, -1481, -1365, -903, 738,
	369, -469, 473, 1630, 3124, 1542, -582, -1172,
	-1381, -317, 4, -610, -40, 1236, 1843, 1493,
	1349, 417, 389, 630, 686, 188, -228, -168,
	-742, -795, -1530, -1473, -1903, -3008, -2907, -1317,
	-445, -2309, -4919, -8939, -5867, 1204, 5293, 5337,
	4871, 4562, 5602, 5104, 2485, -2337, -5594, -4240,
	-1694, 867, 1281, 2622, 3638, 4228, 4654, 1405,
	-1947, -4112, -4184, -3582, -1570, 1325, 2538, 4036,
	5144, 4630, 2718, 518, -1373, -2397, -1642, -453,
	349, 1566, 2558, 2493, 1927, 662, -365, -610,
	-136, 188, 453, 437, -385, -1281, -1196, -1534,
	-369, 265, -899, -3445, -7176, -4538, -2726, -5650,
	-13152, -1694, 7040, 11489, 12224, 5971, 1971, 1779,
	3457, -373, -6040, -7714, -5008, 2594, 7658, 8156,
	4461, 2333, 4369, 2867, -1919, -7180, -8465, -6409,
	-2618, 2152, 3120, 4208, 5570, 5558, 4120, 690,
	-2088, -3345, -1975, -208, 1180, 1738, 2144, 3289,
	3686, 1819, -417, -1534, -875, 88, 678, 967,
	437, 558, 951, 20, -1638, -2558, -1967, 558,
	2289, 465, -4449, -11080, -8931, -6248, -4208, -3337,
	-6493, -14550, -5068, 12305, 13261, 9742, 4261, 1851,
	3016, 6971, 441, -9554, -7096, -975, 5188, 6658,
	5409, 1341, 855, 6164, 1726, -2381, -7991, -7212,
	-4799, -433, 3236, 3273, 3253, 4445, 6706, 3329,
	582, -2602, -3028, -1614, -152, 196, 1598, 3638,
	5144, 4016, 1586, -1004, -2016, -1401, -682, -128,
	273, 614, 963, 1614, 425, 269, -449, 277,
	1746, 1240, -1510, -4598, -6397, -8008, -7602, -7152,
	-7393, -6738, -8606, -15385, -13385, 1192, 12212, 11152,
	9967, 8622, 5240, 6939, 7369, -2216, -9602, -7425,
	-999, 3228, 6329, 4702, 4305, 4550, 6216, 3072,
	-4983, -9313, -9437, -5586, -1987, 2088, 3184, 4662,
	8244, 6598, 4606, -277, -2718, -3188, -2321, -437,
	835, 2855, 4638, 4943, 2116, -393, -2269, -2502,
	-2445, -1630, -646, 469, 1927, 3188, 2943, 502,
	-3148, -3100, -1144, -642, 658, 1843, 449, -1445
} ;

/* test_codes_32 decoded by a known reference */
static const short pcm_data_out32 [] =
{	20, 96, 417, 433, 140, -506, -742, -714,
	-598, -1092, -1044, 56, -445, -702, -622, 76,
	-1116, -4293, -2429, -433, 606, 1196, 1357, 650,
	2465, 3040, 1730, -682, -1381, -1759, -867, 518,
	614, -698, 751, 2172, 3216, 1369, -562, -1076,
	-1293, -116, -12, -803, -176, 1297, 2228, 1759,
	1257, 425, 453, 614, 622, 188, -212, -220,
	-975, -951, -1441, -1309, -1698, -2578, -2405, -1650,
	-590, -2293, -7052, -8506, -5907, 1100, 5192, 5305,
	4244, 4425, 6779, 5313, 2152, -2654, -5598, -3803,
	-2176, 301, 1080, 2281, 3361, 4485, 4690, 1269,
	-2253, -4477, -4562, -3598, -1345, 1108, 2638, 3783,
	4819, 4401, 2357, 409, -1180, -2204, -1730, -662,
	168, 1566, 2550, 2333, 1879, 485, -293, -690,
	-28, 176, 445, 413, -767, -1088, -1204, -1847,
	-481, 261, -1321, -8714, -10646, -4265, -1979, -7100,
	-11678, -1911, 7449, 13333, 11991, 5244, 1935, 1072,
	3638, -4, -6377, -7650, -4819, 2674, 7148, 8036,
	4325, 2433, 3855, 2204, -1638, -6361, -8192, -6634,
	-2184, 2144, 3357, 4164, 4783, 5168, 3835, 1100,
	-1670, -3224, -2140, -144, 1120, 1755, 2530, 3626,
	3678, 1771, -281, -1289, -875, 48, 755, 1112,
	449, 546, 1140, 232, -1530, -2783, -1871, 1128,
	2216, -1899, -8606, -11333, -10140, -7546, -4357, -2979,
	-6044, -14851, -3726, 13136, 13477, 9534, 3871, 1489,
	3526, 7012, 80, -8188, -7140, -1120, 5783, 7060,
	5823, 1337, 1108, 5566, 2345, -3373, -8140, -7919,
	-4566, 76, 3060, 2795, 3385, 5907, 6558, 3638,
	257, -2630, -3401, -1807, -116, 349, 1610, 3417,
	4750, 3967, 1489, -907, -1923, -1385, -666, -265,
	253, 682, 1084, 1586, 538, 184, -381, 433,
	1875, 1289, -1574, -4538, -6168, -8196, -7887, -6750,
	-7526, -6060, -8148, -16036, -12546, 895, 12991, 12060,
	10827, 8931, 5321, 8646, 7654, -473, -8582, -6614,
	-1321, 2803, 6542, 5184, 3847, 4943, 6397, 2148,
	-4999, -8799, -9614, -5931, -1574, 1546, 3493, 5397,
	7879, 6919, 4610, 160, -2538, -3582, -2052, -578,
	1060, 2987, 4843, 4791, 2421, -116, -1987, -2518,
	-2333, -1534, -855, 365, 1779, 3389, 3080, 477,
	-3281, -3120, -1188, -265, 638, 2224, 333, -1377
} ;


static void
test_nms_adpcm_32 (void)
{
	struct nms_adpcm_state nms ;
	int16_t *buffer ;
	unsigned char code ;
	int i, j, sl ;

	buffer = (int16_t *) malloc (sizeof (int16_t) * NMS_SAMPLES_PER_BLOCK) ;

	print_test_name ("Testing nms adpcm 32kbs encoder") ;

	nms_adpcm_codec_init (&nms, NMS32) ;
	for (i = 0 ; i * NMS_BLOCK_SHORTS_32 < ARRAY_LEN (test_codes_32) ; i ++)
	{	/* Unpack the reference */
		nms_adpcm_block_unpack_32 (&(test_codes_32 [i * NMS_BLOCK_SHORTS_32]), buffer, NULL) ;
		for (j = 0 ; j < NMS_SAMPLES_PER_BLOCK ; j++)
		{	sl = pcm_data_src [i * NMS_SAMPLES_PER_BLOCK + j] ;
			code = nms_adpcm_encode_sample (&nms, sl) ;
			if (code != buffer [j])
			{	printf ("\n\nFail at sample %d (block %d, sample %d). Expected 0x%x got 0x%x\n\n",
					i * NMS_SAMPLES_PER_BLOCK + j, i, j, buffer [j], code) ;
				exit (1) ;
				}
			}
		}

	puts ("ok") ;

	print_test_name ("Testing nms adpcm 32kbs decoder") ;

	nms_adpcm_codec_init (&nms, NMS32) ;
	for (i = 0 ; i * NMS_BLOCK_SHORTS_32 < ARRAY_LEN (test_codes_32) ; i ++)
	{	/* Unpack the code */
		nms_adpcm_block_unpack_32 (&(test_codes_32 [i * NMS_BLOCK_SHORTS_32]), buffer, NULL) ;
		for (j = 0 ; j < NMS_SAMPLES_PER_BLOCK ; j++)
		{	sl = nms_adpcm_decode_sample (&nms, buffer [j]) ;
			if (sl != pcm_data_out32 [i * NMS_SAMPLES_PER_BLOCK + j])
			{	printf ("\n\nFail at sample %d (block %d, sample %d). Expected %d got %d\n\n",
					i * NMS_SAMPLES_PER_BLOCK + j, i, j, pcm_data_out32 [i * NMS_SAMPLES_PER_BLOCK + j], sl) ;
				exit (1) ;
				}
			}
		}

	puts ("ok") ;

	free (buffer) ;
}


static void
test_nms_adpcm_24 (void)
{
	struct nms_adpcm_state nms ;
	int16_t *buffer ;
	unsigned char code ;
	int i, j, sl ;

	buffer = (int16_t *) malloc (sizeof (int16_t) * NMS_SAMPLES_PER_BLOCK) ;


	print_test_name ("Testing nms adpcm 24kbs encoder") ;

	nms_adpcm_codec_init (&nms, NMS24) ;
	for (i = 0 ; i * NMS_BLOCK_SHORTS_24 < ARRAY_LEN (test_codes_24) ; i ++)
	{	/* Unpack the reference */
		nms_adpcm_block_unpack_24 (&test_codes_24 [i * NMS_BLOCK_SHORTS_24], buffer, NULL) ;
		for (j = 0 ; j < NMS_SAMPLES_PER_BLOCK ; j++)
		{	sl = pcm_data_src [i * NMS_SAMPLES_PER_BLOCK + j] ;
			code = nms_adpcm_encode_sample (&nms, sl) ;
			if (code != buffer [j])
			{	printf ("\n\nFail at sample %d (block %d, sample %d). Expected 0x%x got 0x%x\n\n",
					i * NMS_SAMPLES_PER_BLOCK + j, i, j, buffer [j], code) ;
				exit (1) ;
				}
			}
		}

	puts ("ok") ;


	print_test_name ("Testing nms adpcm 24kbs decoder") ;

	nms_adpcm_codec_init (&nms, NMS24) ;
	for (i = 0 ; i * NMS_BLOCK_SHORTS_24 < ARRAY_LEN (test_codes_24) ; i ++)
	{	/* Unpack the code */
		nms_adpcm_block_unpack_24 (&test_codes_24 [i * NMS_BLOCK_SHORTS_24], buffer, NULL) ;
		for (j = 0 ; j < NMS_SAMPLES_PER_BLOCK ; j++)
		{	sl = nms_adpcm_decode_sample (&nms, buffer [j]) ;
			if (sl != pcm_data_out24 [i * NMS_SAMPLES_PER_BLOCK + j])
			{	printf ("\n\nFail at sample %d (block %d, sample %d). Expected %d got %d\n\n",
					i * NMS_SAMPLES_PER_BLOCK + j, i, j, pcm_data_out24 [i * NMS_SAMPLES_PER_BLOCK + j], sl) ;
				exit (1) ;
				}
			}
		}

	puts ("ok") ;

	free (buffer) ;
} /* test_nms_adpcm_24 */

static void
test_nms_adpcm_16 (void)
{	struct nms_adpcm_state nms ;
	int16_t *buffer ;
	unsigned char code ;
	int i, j, sl ;

	buffer = (int16_t *) malloc (sizeof (int16_t) * NMS_SAMPLES_PER_BLOCK) ;

	print_test_name ("Testing nms adpcm 16kbs encoder") ;

	nms_adpcm_codec_init (&nms, NMS16) ;
	for (i = 0 ; i * NMS_BLOCK_SHORTS_16 < ARRAY_LEN (test_codes_16) ; i ++)
	{	/* Unpack the reference */
		nms_adpcm_block_unpack_16 (&test_codes_16 [i * NMS_BLOCK_SHORTS_16], buffer, NULL) ;
		for (j = 0 ; j < NMS_SAMPLES_PER_BLOCK ; j++)
		{	sl = pcm_data_src [i * NMS_SAMPLES_PER_BLOCK + j] ;
			code = nms_adpcm_encode_sample (&nms, sl) ;
			if (code != buffer [j])
			{	printf ("\n\nFail at sample %d (block %d, sample %d). Expected 0x%x got 0x%x\n\n",
					i * NMS_SAMPLES_PER_BLOCK + j, i, j, buffer [j], code) ;
				exit (1) ;
				}
			}
		}

	puts ("ok") ;

	print_test_name ("Testing nms adpcm 16kbs decoder") ;

	nms_adpcm_codec_init (&nms, NMS16) ;
	for (i = 0 ; i * NMS_BLOCK_SHORTS_16 < ARRAY_LEN (test_codes_16) ; i ++)
	{	/* Unpack the code */
		nms_adpcm_block_unpack_16 (&test_codes_16 [i * NMS_BLOCK_SHORTS_16], buffer, NULL) ;
		for (j = 0 ; j < NMS_SAMPLES_PER_BLOCK ; j++)
		{	sl = nms_adpcm_decode_sample (&nms, buffer [j]) ;
			if (sl != pcm_data_out16 [i * NMS_SAMPLES_PER_BLOCK + j])
			{	printf ("\n\nFail at sample %d (block %d, sample %d). Expected %d got %d\n\n",
					i * NMS_SAMPLES_PER_BLOCK + j, i, j, pcm_data_out16 [i * NMS_SAMPLES_PER_BLOCK + j], sl) ;
				exit (1) ;
				}
			}
		}

	puts ("ok") ;

	free (buffer) ;
} /* test_nms_adpcm_16 */

void
test_nms_adpcm (void)
{	test_nms_adpcm_32 () ;
	test_nms_adpcm_24 () ;
	test_nms_adpcm_16 () ;
} /* main */

