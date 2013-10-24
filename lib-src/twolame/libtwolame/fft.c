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



/*
** FFT and FHT routines
**  Copyright 1988, 1993; Ron Mayer
**  
**  fht(fz,n);
**    Does a hartley transform of "n" points in the array "fz".
**    
** NOTE: This routine uses at least 2 patented algorithms, and may be
**       under the restrictions of a bunch of different organizations.
**       Although I wrote it completely myself; it is kind of a derivative
**       of a routine I once authored and released under the GPL, so it
**       may fall under the free software foundation's restrictions;
**       it was worked on as a Stanford Univ project, so they claim
**       some rights to it; it was further optimized at work here, so
**       I think this company claims parts of it.  The patents are
**       held by R. Bracewell (the FHT algorithm) and O. Buneman (the
**       trig generator), both at Stanford Univ.
**       If it were up to me, I'd say go do whatever you want with it;
**       but it would be polite to give credit to the following people
**       if you use this anywhere:
**           Euler     - probable inventor of the fourier transform.
**           Gauss     - probable inventor of the FFT.
**           Hartley   - probable inventor of the hartley transform.
**           Buneman   - for a really cool trig generator
**           Mayer(me) - for authoring this particular version and
**                       including all the optimizations in one package.
**       Thanks,
**       Ron Mayer; mayer@acuson.com
**
*/

#include <stdio.h>
#include <math.h>

#include "twolame.h"
#include "common.h"
#include "fft.h"



#define	SQRT2		1.4142135623730951454746218587388284504414


static const FLOAT costab[20] = {
    .00000000000000000000000000000000000000000000000000,
    .70710678118654752440084436210484903928483593768847,
    .92387953251128675612818318939678828682241662586364,
    .98078528040323044912618223613423903697393373089333,
    .99518472667219688624483695310947992157547486872985,
    .99879545620517239271477160475910069444320361470461,
    .99969881869620422011576564966617219685006108125772,
    .99992470183914454092164649119638322435060646880221,
    .99998117528260114265699043772856771617391725094433,
    .99999529380957617151158012570011989955298763362218,
    .99999882345170190992902571017152601904826792288976,
    .99999970586288221916022821773876567711626389934930,
    .99999992646571785114473148070738785694820115568892,
    .99999998161642929380834691540290971450507605124278,
    .99999999540410731289097193313960614895889430318945,
    .99999999885102682756267330779455410840053741619428
};
static const FLOAT sintab[20] = {
    1.0000000000000000000000000000000000000000000000000,
    .70710678118654752440084436210484903928483593768846,
    .38268343236508977172845998403039886676134456248561,
    .19509032201612826784828486847702224092769161775195,
    .09801714032956060199419556388864184586113667316749,
    .04906767432741801425495497694268265831474536302574,
    .02454122852291228803173452945928292506546611923944,
    .01227153828571992607940826195100321214037231959176,
    .00613588464915447535964023459037258091705788631738,
    .00306795676296597627014536549091984251894461021344,
    .00153398018628476561230369715026407907995486457522,
    .00076699031874270452693856835794857664314091945205,
    .00038349518757139558907246168118138126339502603495,
    .00019174759731070330743990956198900093346887403385,
    .00009587379909597734587051721097647635118706561284,
    .00004793689960306688454900399049465887274686668768
};

/* This is a simplified version for n an even power of 2 */
/* MFC: In the case of LayerII encoding, n==1024 always. */

static void fht(FLOAT * fz)
{
    int i, k, k1, k2, k3, k4, kx;
    FLOAT *fi, *fn, *gi;
    FLOAT t_c, t_s;

    FLOAT a;
    static const struct {
        unsigned short k1, k2;
    } k1k2tab[8 * 62] = {
        {
        0x020, 0x010}, {
        0x040, 0x008}, {
        0x050, 0x028}, {
        0x060, 0x018}, {
        0x068, 0x058}, {
        0x070, 0x038}, {
        0x080, 0x004}, {
        0x088, 0x044}, {
        0x090, 0x024}, {
        0x098, 0x064}, {
        0x0a0, 0x014}, {
        0x0a4, 0x094}, {
        0x0a8, 0x054}, {
        0x0b0, 0x034}, {
        0x0b8, 0x074}, {
        0x0c0, 0x00c}, {
        0x0c4, 0x08c}, {
        0x0c8, 0x04c}, {
        0x0d0, 0x02c}, {
        0x0d4, 0x0ac}, {
        0x0d8, 0x06c}, {
        0x0e0, 0x01c}, {
        0x0e4, 0x09c}, {
        0x0e8, 0x05c}, {
        0x0ec, 0x0dc}, {
        0x0f0, 0x03c}, {
        0x0f4, 0x0bc}, {
        0x0f8, 0x07c}, {
        0x100, 0x002}, {
        0x104, 0x082}, {
        0x108, 0x042}, {
        0x10c, 0x0c2}, {
        0x110, 0x022}, {
        0x114, 0x0a2}, {
        0x118, 0x062}, {
        0x11c, 0x0e2}, {
        0x120, 0x012}, {
        0x122, 0x112}, {
        0x124, 0x092}, {
        0x128, 0x052}, {
        0x12c, 0x0d2}, {
        0x130, 0x032}, {
        0x134, 0x0b2}, {
        0x138, 0x072}, {
        0x13c, 0x0f2}, {
        0x140, 0x00a}, {
        0x142, 0x10a}, {
        0x144, 0x08a}, {
        0x148, 0x04a}, {
        0x14c, 0x0ca}, {
        0x150, 0x02a}, {
        0x152, 0x12a}, {
        0x154, 0x0aa}, {
        0x158, 0x06a}, {
        0x15c, 0x0ea}, {
        0x160, 0x01a}, {
        0x162, 0x11a}, {
        0x164, 0x09a}, {
        0x168, 0x05a}, {
        0x16a, 0x15a}, {
        0x16c, 0x0da}, {
        0x170, 0x03a}, {
        0x172, 0x13a}, {
        0x174, 0x0ba}, {
        0x178, 0x07a}, {
        0x17c, 0x0fa}, {
        0x180, 0x006}, {
        0x182, 0x106}, {
        0x184, 0x086}, {
        0x188, 0x046}, {
        0x18a, 0x146}, {
        0x18c, 0x0c6}, {
        0x190, 0x026}, {
        0x192, 0x126}, {
        0x194, 0x0a6}, {
        0x198, 0x066}, {
        0x19a, 0x166}, {
        0x19c, 0x0e6}, {
        0x1a0, 0x016}, {
        0x1a2, 0x116}, {
        0x1a4, 0x096}, {
        0x1a6, 0x196}, {
        0x1a8, 0x056}, {
        0x1aa, 0x156}, {
        0x1ac, 0x0d6}, {
        0x1b0, 0x036}, {
        0x1b2, 0x136}, {
        0x1b4, 0x0b6}, {
        0x1b8, 0x076}, {
        0x1ba, 0x176}, {
        0x1bc, 0x0f6}, {
        0x1c0, 0x00e}, {
        0x1c2, 0x10e}, {
        0x1c4, 0x08e}, {
        0x1c6, 0x18e}, {
        0x1c8, 0x04e}, {
        0x1ca, 0x14e}, {
        0x1cc, 0x0ce}, {
        0x1d0, 0x02e}, {
        0x1d2, 0x12e}, {
        0x1d4, 0x0ae}, {
        0x1d6, 0x1ae}, {
        0x1d8, 0x06e}, {
        0x1da, 0x16e}, {
        0x1dc, 0x0ee}, {
        0x1e0, 0x01e}, {
        0x1e2, 0x11e}, {
        0x1e4, 0x09e}, {
        0x1e6, 0x19e}, {
        0x1e8, 0x05e}, {
        0x1ea, 0x15e}, {
        0x1ec, 0x0de}, {
        0x1ee, 0x1de}, {
        0x1f0, 0x03e}, {
        0x1f2, 0x13e}, {
        0x1f4, 0x0be}, {
        0x1f6, 0x1be}, {
        0x1f8, 0x07e}, {
        0x1fa, 0x17e}, {
        0x1fc, 0x0fe}, {
        0x200, 0x001}, {
        0x202, 0x101}, {
        0x204, 0x081}, {
        0x206, 0x181}, {
        0x208, 0x041}, {
        0x20a, 0x141}, {
        0x20c, 0x0c1}, {
        0x20e, 0x1c1}, {
        0x210, 0x021}, {
        0x212, 0x121}, {
        0x214, 0x0a1}, {
        0x216, 0x1a1}, {
        0x218, 0x061}, {
        0x21a, 0x161}, {
        0x21c, 0x0e1}, {
        0x21e, 0x1e1}, {
        0x220, 0x011}, {
        0x221, 0x211}, {
        0x222, 0x111}, {
        0x224, 0x091}, {
        0x226, 0x191}, {
        0x228, 0x051}, {
        0x22a, 0x151}, {
        0x22c, 0x0d1}, {
        0x22e, 0x1d1}, {
        0x230, 0x031}, {
        0x232, 0x131}, {
        0x234, 0x0b1}, {
        0x236, 0x1b1}, {
        0x238, 0x071}, {
        0x23a, 0x171}, {
        0x23c, 0x0f1}, {
        0x23e, 0x1f1}, {
        0x240, 0x009}, {
        0x241, 0x209}, {
        0x242, 0x109}, {
        0x244, 0x089}, {
        0x246, 0x189}, {
        0x248, 0x049}, {
        0x24a, 0x149}, {
        0x24c, 0x0c9}, {
        0x24e, 0x1c9}, {
        0x250, 0x029}, {
        0x251, 0x229}, {
        0x252, 0x129}, {
        0x254, 0x0a9}, {
        0x256, 0x1a9}, {
        0x258, 0x069}, {
        0x25a, 0x169}, {
        0x25c, 0x0e9}, {
        0x25e, 0x1e9}, {
        0x260, 0x019}, {
        0x261, 0x219}, {
        0x262, 0x119}, {
        0x264, 0x099}, {
        0x266, 0x199}, {
        0x268, 0x059}, {
        0x269, 0x259}, {
        0x26a, 0x159}, {
        0x26c, 0x0d9}, {
        0x26e, 0x1d9}, {
        0x270, 0x039}, {
        0x271, 0x239}, {
        0x272, 0x139}, {
        0x274, 0x0b9}, {
        0x276, 0x1b9}, {
        0x278, 0x079}, {
        0x27a, 0x179}, {
        0x27c, 0x0f9}, {
        0x27e, 0x1f9}, {
        0x280, 0x005}, {
        0x281, 0x205}, {
        0x282, 0x105}, {
        0x284, 0x085}, {
        0x286, 0x185}, {
        0x288, 0x045}, {
        0x289, 0x245}, {
        0x28a, 0x145}, {
        0x28c, 0x0c5}, {
        0x28e, 0x1c5}, {
        0x290, 0x025}, {
        0x291, 0x225}, {
        0x292, 0x125}, {
        0x294, 0x0a5}, {
        0x296, 0x1a5}, {
        0x298, 0x065}, {
        0x299, 0x265}, {
        0x29a, 0x165}, {
        0x29c, 0x0e5}, {
        0x29e, 0x1e5}, {
        0x2a0, 0x015}, {
        0x2a1, 0x215}, {
        0x2a2, 0x115}, {
        0x2a4, 0x095}, {
        0x2a5, 0x295}, {
        0x2a6, 0x195}, {
        0x2a8, 0x055}, {
        0x2a9, 0x255}, {
        0x2aa, 0x155}, {
        0x2ac, 0x0d5}, {
        0x2ae, 0x1d5}, {
        0x2b0, 0x035}, {
        0x2b1, 0x235}, {
        0x2b2, 0x135}, {
        0x2b4, 0x0b5}, {
        0x2b6, 0x1b5}, {
        0x2b8, 0x075}, {
        0x2b9, 0x275}, {
        0x2ba, 0x175}, {
        0x2bc, 0x0f5}, {
        0x2be, 0x1f5}, {
        0x2c0, 0x00d}, {
        0x2c1, 0x20d}, {
        0x2c2, 0x10d}, {
        0x2c4, 0x08d}, {
        0x2c5, 0x28d}, {
        0x2c6, 0x18d}, {
        0x2c8, 0x04d}, {
        0x2c9, 0x24d}, {
        0x2ca, 0x14d}, {
        0x2cc, 0x0cd}, {
        0x2ce, 0x1cd}, {
        0x2d0, 0x02d}, {
        0x2d1, 0x22d}, {
        0x2d2, 0x12d}, {
        0x2d4, 0x0ad}, {
        0x2d5, 0x2ad}, {
        0x2d6, 0x1ad}, {
        0x2d8, 0x06d}, {
        0x2d9, 0x26d}, {
        0x2da, 0x16d}, {
        0x2dc, 0x0ed}, {
        0x2de, 0x1ed}, {
        0x2e0, 0x01d}, {
        0x2e1, 0x21d}, {
        0x2e2, 0x11d}, {
        0x2e4, 0x09d}, {
        0x2e5, 0x29d}, {
        0x2e6, 0x19d}, {
        0x2e8, 0x05d}, {
        0x2e9, 0x25d}, {
        0x2ea, 0x15d}, {
        0x2ec, 0x0dd}, {
        0x2ed, 0x2dd}, {
        0x2ee, 0x1dd}, {
        0x2f0, 0x03d}, {
        0x2f1, 0x23d}, {
        0x2f2, 0x13d}, {
        0x2f4, 0x0bd}, {
        0x2f5, 0x2bd}, {
        0x2f6, 0x1bd}, {
        0x2f8, 0x07d}, {
        0x2f9, 0x27d}, {
        0x2fa, 0x17d}, {
        0x2fc, 0x0fd}, {
        0x2fe, 0x1fd}, {
        0x300, 0x003}, {
        0x301, 0x203}, {
        0x302, 0x103}, {
        0x304, 0x083}, {
        0x305, 0x283}, {
        0x306, 0x183}, {
        0x308, 0x043}, {
        0x309, 0x243}, {
        0x30a, 0x143}, {
        0x30c, 0x0c3}, {
        0x30d, 0x2c3}, {
        0x30e, 0x1c3}, {
        0x310, 0x023}, {
        0x311, 0x223}, {
        0x312, 0x123}, {
        0x314, 0x0a3}, {
        0x315, 0x2a3}, {
        0x316, 0x1a3}, {
        0x318, 0x063}, {
        0x319, 0x263}, {
        0x31a, 0x163}, {
        0x31c, 0x0e3}, {
        0x31d, 0x2e3}, {
        0x31e, 0x1e3}, {
        0x320, 0x013}, {
        0x321, 0x213}, {
        0x322, 0x113}, {
        0x323, 0x313}, {
        0x324, 0x093}, {
        0x325, 0x293}, {
        0x326, 0x193}, {
        0x328, 0x053}, {
        0x329, 0x253}, {
        0x32a, 0x153}, {
        0x32c, 0x0d3}, {
        0x32d, 0x2d3}, {
        0x32e, 0x1d3}, {
        0x330, 0x033}, {
        0x331, 0x233}, {
        0x332, 0x133}, {
        0x334, 0x0b3}, {
        0x335, 0x2b3}, {
        0x336, 0x1b3}, {
        0x338, 0x073}, {
        0x339, 0x273}, {
        0x33a, 0x173}, {
        0x33c, 0x0f3}, {
        0x33d, 0x2f3}, {
        0x33e, 0x1f3}, {
        0x340, 0x00b}, {
        0x341, 0x20b}, {
        0x342, 0x10b}, {
        0x343, 0x30b}, {
        0x344, 0x08b}, {
        0x345, 0x28b}, {
        0x346, 0x18b}, {
        0x348, 0x04b}, {
        0x349, 0x24b}, {
        0x34a, 0x14b}, {
        0x34c, 0x0cb}, {
        0x34d, 0x2cb}, {
        0x34e, 0x1cb}, {
        0x350, 0x02b}, {
        0x351, 0x22b}, {
        0x352, 0x12b}, {
        0x353, 0x32b}, {
        0x354, 0x0ab}, {
        0x355, 0x2ab}, {
        0x356, 0x1ab}, {
        0x358, 0x06b}, {
        0x359, 0x26b}, {
        0x35a, 0x16b}, {
        0x35c, 0x0eb}, {
        0x35d, 0x2eb}, {
        0x35e, 0x1eb}, {
        0x360, 0x01b}, {
        0x361, 0x21b}, {
        0x362, 0x11b}, {
        0x363, 0x31b}, {
        0x364, 0x09b}, {
        0x365, 0x29b}, {
        0x366, 0x19b}, {
        0x368, 0x05b}, {
        0x369, 0x25b}, {
        0x36a, 0x15b}, {
        0x36b, 0x35b}, {
        0x36c, 0x0db}, {
        0x36d, 0x2db}, {
        0x36e, 0x1db}, {
        0x370, 0x03b}, {
        0x371, 0x23b}, {
        0x372, 0x13b}, {
        0x373, 0x33b}, {
        0x374, 0x0bb}, {
        0x375, 0x2bb}, {
        0x376, 0x1bb}, {
        0x378, 0x07b}, {
        0x379, 0x27b}, {
        0x37a, 0x17b}, {
        0x37c, 0x0fb}, {
        0x37d, 0x2fb}, {
        0x37e, 0x1fb}, {
        0x380, 0x007}, {
        0x381, 0x207}, {
        0x382, 0x107}, {
        0x383, 0x307}, {
        0x384, 0x087}, {
        0x385, 0x287}, {
        0x386, 0x187}, {
        0x388, 0x047}, {
        0x389, 0x247}, {
        0x38a, 0x147}, {
        0x38b, 0x347}, {
        0x38c, 0x0c7}, {
        0x38d, 0x2c7}, {
        0x38e, 0x1c7}, {
        0x390, 0x027}, {
        0x391, 0x227}, {
        0x392, 0x127}, {
        0x393, 0x327}, {
        0x394, 0x0a7}, {
        0x395, 0x2a7}, {
        0x396, 0x1a7}, {
        0x398, 0x067}, {
        0x399, 0x267}, {
        0x39a, 0x167}, {
        0x39b, 0x367}, {
        0x39c, 0x0e7}, {
        0x39d, 0x2e7}, {
        0x39e, 0x1e7}, {
        0x3a0, 0x017}, {
        0x3a1, 0x217}, {
        0x3a2, 0x117}, {
        0x3a3, 0x317}, {
        0x3a4, 0x097}, {
        0x3a5, 0x297}, {
        0x3a6, 0x197}, {
        0x3a7, 0x397}, {
        0x3a8, 0x057}, {
        0x3a9, 0x257}, {
        0x3aa, 0x157}, {
        0x3ab, 0x357}, {
        0x3ac, 0x0d7}, {
        0x3ad, 0x2d7}, {
        0x3ae, 0x1d7}, {
        0x3b0, 0x037}, {
        0x3b1, 0x237}, {
        0x3b2, 0x137}, {
        0x3b3, 0x337}, {
        0x3b4, 0x0b7}, {
        0x3b5, 0x2b7}, {
        0x3b6, 0x1b7}, {
        0x3b8, 0x077}, {
        0x3b9, 0x277}, {
        0x3ba, 0x177}, {
        0x3bb, 0x377}, {
        0x3bc, 0x0f7}, {
        0x3bd, 0x2f7}, {
        0x3be, 0x1f7}, {
        0x3c0, 0x00f}, {
        0x3c1, 0x20f}, {
        0x3c2, 0x10f}, {
        0x3c3, 0x30f}, {
        0x3c4, 0x08f}, {
        0x3c5, 0x28f}, {
        0x3c6, 0x18f}, {
        0x3c7, 0x38f}, {
        0x3c8, 0x04f}, {
        0x3c9, 0x24f}, {
        0x3ca, 0x14f}, {
        0x3cb, 0x34f}, {
        0x3cc, 0x0cf}, {
        0x3cd, 0x2cf}, {
        0x3ce, 0x1cf}, {
        0x3d0, 0x02f}, {
        0x3d1, 0x22f}, {
        0x3d2, 0x12f}, {
        0x3d3, 0x32f}, {
        0x3d4, 0x0af}, {
        0x3d5, 0x2af}, {
        0x3d6, 0x1af}, {
        0x3d7, 0x3af}, {
        0x3d8, 0x06f}, {
        0x3d9, 0x26f}, {
        0x3da, 0x16f}, {
        0x3db, 0x36f}, {
        0x3dc, 0x0ef}, {
        0x3dd, 0x2ef}, {
        0x3de, 0x1ef}, {
        0x3e0, 0x01f}, {
        0x3e1, 0x21f}, {
        0x3e2, 0x11f}, {
        0x3e3, 0x31f}, {
        0x3e4, 0x09f}, {
        0x3e5, 0x29f}, {
        0x3e6, 0x19f}, {
        0x3e7, 0x39f}, {
        0x3e8, 0x05f}, {
        0x3e9, 0x25f}, {
        0x3ea, 0x15f}, {
        0x3eb, 0x35f}, {
        0x3ec, 0x0df}, {
        0x3ed, 0x2df}, {
        0x3ee, 0x1df}, {
        0x3ef, 0x3df}, {
        0x3f0, 0x03f}, {
        0x3f1, 0x23f}, {
        0x3f2, 0x13f}, {
        0x3f3, 0x33f}, {
        0x3f4, 0x0bf}, {
        0x3f5, 0x2bf}, {
        0x3f6, 0x1bf}, {
        0x3f7, 0x3bf}, {
        0x3f8, 0x07f}, {
        0x3f9, 0x27f}, {
        0x3fa, 0x17f}, {
        0x3fb, 0x37f}, {
        0x3fc, 0x0ff}, {
        0x3fd, 0x2ff}, {
        0x3fe, 0x1ff}
    };


    {
        int i;
        for (i = 0; i < sizeof k1k2tab / sizeof k1k2tab[0]; ++i) {
            k1 = k1k2tab[i].k1;
            k2 = k1k2tab[i].k2;
            a = fz[k1];
            fz[k1] = fz[k2];
            fz[k2] = a;
        }
    }

    for (fi = fz, fn = fz + 1024; fi < fn; fi += 4) {
        FLOAT f0, f1, f2, f3;
        f1 = fi[0] - fi[1];
        f0 = fi[0] + fi[1];
        f3 = fi[2] - fi[3];
        f2 = fi[2] + fi[3];
        fi[2] = (f0 - f2);
        fi[0] = (f0 + f2);
        fi[3] = (f1 - f3);
        fi[1] = (f1 + f3);
    }

    k = 0;
    do {
        FLOAT s1, c1;
        k += 2;
        k1 = 1 << k;
        k2 = k1 << 1;
        k4 = k2 << 1;
        k3 = k2 + k1;
        kx = k1 >> 1;
        fi = fz;
        gi = fi + kx;
        fn = fz + 1024;
        do {
            FLOAT g0, f0, f1, g1, f2, g2, f3, g3;
            f1 = fi[0] - fi[k1];
            f0 = fi[0] + fi[k1];
            f3 = fi[k2] - fi[k3];
            f2 = fi[k2] + fi[k3];
            fi[k2] = f0 - f2;
            fi[0] = f0 + f2;
            fi[k3] = f1 - f3;
            fi[k1] = f1 + f3;
            g1 = gi[0] - gi[k1];
            g0 = gi[0] + gi[k1];
            g3 = SQRT2 * gi[k3];
            g2 = SQRT2 * gi[k2];
            gi[k2] = g0 - g2;
            gi[0] = g0 + g2;
            gi[k3] = g1 - g3;
            gi[k1] = g1 + g3;
            gi += k4;
            fi += k4;
        }
        while (fi < fn);

        t_c = costab[k];
        t_s = sintab[k];
        c1 = 1;
        s1 = 0;
        for (i = 1; i < kx; i++) {
            FLOAT c2, s2;
            FLOAT t = c1;
            c1 = t * t_c - s1 * t_s;
            s1 = t * t_s + s1 * t_c;
            c2 = c1 * c1 - s1 * s1;
            s2 = 2 * (c1 * s1);
            fn = fz + 1024;
            fi = fz + i;
            gi = fz + k1 - i;
            do {
                FLOAT a, b, g0, f0, f1, g1, f2, g2, f3, g3;
                b = s2 * fi[k1] - c2 * gi[k1];
                a = c2 * fi[k1] + s2 * gi[k1];
                f1 = fi[0] - a;
                f0 = fi[0] + a;
                g1 = gi[0] - b;
                g0 = gi[0] + b;
                b = s2 * fi[k3] - c2 * gi[k3];
                a = c2 * fi[k3] + s2 * gi[k3];
                f3 = fi[k2] - a;
                f2 = fi[k2] + a;
                g3 = gi[k2] - b;
                g2 = gi[k2] + b;
                b = s1 * f2 - c1 * g3;
                a = c1 * f2 + s1 * g3;
                fi[k2] = f0 - a;
                fi[0] = f0 + a;
                gi[k3] = g1 - b;
                gi[k1] = g1 + b;
                b = c1 * g2 - s1 * f3;
                a = s1 * g2 + c1 * f3;
                gi[k2] = g0 - a;
                gi[0] = g0 + a;
                fi[k3] = f1 - b;
                fi[k1] = f1 + b;
                gi += k4;
                fi += k4;
            }
            while (fi < fn);
        }
    }
    while (k4 < 1024);
}

#ifdef NEWATAN
#define ATANSIZE 6000
#define ATANSCALE 100.0
/* Create a table of ATAN2 values.
   Valid for ratios of (y/x) from 0 to ATANSIZE/ATANSCALE (60) 
   Largest absolute error in angle: 0.0167 radians i.e. ATANSCALE/ATANSIZE 
   Depending on how you want to trade off speed/accuracy and mem usage, twiddle the defines
   MFC March 2003 */
static FLOAT atan_t[ATANSIZE];

static inline FLOAT atan_table(FLOAT y, FLOAT x)
{
    int index;

    index = (int) (ATANSCALE * fabs(y / x));
    if (index >= ATANSIZE)
        index = ATANSIZE - 1;

    /* Have to work out the correct quadrant as well */
    if (y > 0 && x < 0)
        return (PI - atan_t[index]);

    if (y < 0 && x > 0)
        return (-atan_t[index]);

    if (y < 0 && x < 0)
        return (atan_t[index] - PI);

    return (atan_t[index]);
}

static void atan_table_init(void)
{
    int i;
    for (i = 0; i < ATANSIZE; i++)
        atan_t[i] = atan((FLOAT) i / ATANSCALE);
}

#endif                          // NEWATAN

/* For variations on psycho model 2:
   N always equals 1024
   BUT in the returned values, no energy/phi is used at or above an index of 513 */
void psycho_2_fft(FLOAT * x_real, FLOAT * energy, FLOAT * phi)
/* got rid of size "N" argument as it is always 1024 for layerII */
{
    FLOAT imag, real;
    int i, j;
#ifdef NEWATAN
    static int init = 0;

    if (!init) {
        atan_table_init();
        init++;
    }
#endif


    fht(x_real);


    energy[0] = x_real[0] * x_real[0];

    for (i = 1, j = 1023; i < 512; i++, j--) {
        imag = x_real[i];
        real = x_real[j];
        /* MFC FIXME Mar03 Why is this divided by 2.0? if a and b are the real and imaginary
           components then r = sqrt(a^2 + b^2), but, back in the psycho2 model, they calculate
           r=sqrt(energy), which, if you look at the original equation below is different */
        energy[i] = (imag * imag + real * real) / 2.0;
        if (energy[i] < 0.0005) {
            energy[i] = 0.0005;
            phi[i] = 0;
        } else
#ifdef NEWATAN
        {
            phi[i] = atan_table(-imag, real) + PI / 4;
        }
#else
        {
            phi[i] = atan2(-(FLOAT) imag, (FLOAT) real) + PI / 4;
        }
#endif
    }
    energy[512] = x_real[512] * x_real[512];
    phi[512] = atan2(0.0, (FLOAT) x_real[512]);
}


void psycho_1_fft(FLOAT * x_real, FLOAT * energy, int N)
{
    FLOAT a, b;
    int i, j;

    fht(x_real);

    energy[0] = x_real[0] * x_real[0];

    for (i = 1, j = N - 1; i < N / 2; i++, j--) {
        a = x_real[i];
        b = x_real[j];
        energy[i] = (a * a + b * b) / 2.0;
    }
    energy[N / 2] = x_real[N / 2] * x_real[N / 2];
}


// vim:ts=4:sw=4:nowrap: 
