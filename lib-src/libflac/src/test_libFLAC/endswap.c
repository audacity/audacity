/* test_libFLAC - Unit tester for libFLAC
 * Copyright (C) 2014  Xiph.Org Foundation
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include <string.h>

#include "share/compat.h"
#include "FLAC/assert.h"
#include "share/endswap.h"
#include "private/md5.h"
#include "endswap.h"


FLAC__bool test_endswap(void)
{
	int16_t i16 = 0x1234;
	uint16_t u16 = 0xabcd;
	int32_t i32 = 0x12345678;
	uint32_t u32 = 0xabcdef01;

	union {
		unsigned char bytes[4];
		uint16_t u16;
		uint32_t u32;
	} data;

	printf("\n+++ libFLAC unit test: endswap (%s endian host)\n\n", CPU_IS_LITTLE_ENDIAN ? "little" : "big");

	printf("testing ENDSWAP_16 on int16_t ... ");
	if (((int16_t) ENDSWAP_16(i16)) == i16) {
		printf("\nFAILED, ENDSWAP_16(0x%04x) -> 0x%04x == 0x%04x\n", i16, ENDSWAP_16(i16), i16);
		return false;
	}
	if (((int16_t) ENDSWAP_16(ENDSWAP_16(i16))) != i16) {
		printf("\nFAILED, ENDSWAP_16(ENDSWAP_16(0x%04x)) -> 0x%04x != 0x%04x\n", i16, ENDSWAP_16(ENDSWAP_16(i16)), i16);
		return false;
	}
	puts("OK");

	printf("testing ENDSWAP_16 on uint16_t ... ");
	if (((uint16_t) ENDSWAP_16(u16)) == u16) {
		printf("\nFAILED, ENDSWAP_16(0x%04x) -> 0x%04x == 0x%04x\n", u16, ENDSWAP_16(u16), u16);
		return false;
	}
	if (((uint16_t) ENDSWAP_16(ENDSWAP_16(u16))) != u16) {
		printf("\nFAILED, ENDSWAP_16(ENDSWAP_16(0x%04x)) -> 0x%04x != 0x%04x\n", u16, ENDSWAP_16(ENDSWAP_16(u16)), u16);
		return false;
	}
	puts("OK");

	printf("testing ENDSWAP_32 on int32_t ... ");
	if (((int32_t) ENDSWAP_32 (i32)) == i32) {
		printf("\nFAILED, ENDSWAP_32(0x%08x) -> 0x%08x == 0x%08x\n", i32, (unsigned) ENDSWAP_32 (i32), i32);
		return false;
	}
	if (((int32_t) ENDSWAP_32 (ENDSWAP_32 (i32))) != i32) {
		printf("\nFAILED, ENDSWAP_32(ENDSWAP_32(0x%08x)) -> 0x%08x != 0x%08x\n", i32, (unsigned) ENDSWAP_32(ENDSWAP_32 (i32)), i32);
		return false;
	}
	puts("OK");

	printf("testing ENDSWAP_32 on uint32_t ... ");
	if (((uint32_t) ENDSWAP_32(u32)) == u32) {
		printf("\nFAILED, ENDSWAP_32(0x%08x) -> 0x%08x == 0x%08x\n", u32, (unsigned) ENDSWAP_32(u32), u32);
		return false;
	}
	if (((uint32_t) ENDSWAP_32 (ENDSWAP_32(u32))) != u32) {
		printf("\nFAILED, ENDSWAP_32(ENDSWAP_32(0x%08x)) -> 0x%08x != 0%08x\n", u32, (unsigned) ENDSWAP_32(ENDSWAP_32(u32)), u32);
		return false;
	}
	puts("OK");

	printf("testing H2LE_16 on uint16_t ... ");
	data.u16 = H2LE_16(0x1234);
	if (data.bytes [0] != 0x34 || data.bytes [1] != 0x12) {
		printf("\nFAILED, H2LE_16(0x%04x) -> { 0x%02x, 0x%02x }\n", data.u16, data.bytes [0] & 0xff, data.bytes [1] & 0xff);
		return false;
	}
	puts("OK");

	printf("testing H2LE_32 on uint32_t ... ");
	data.u32 = H2LE_32(0x12345678);
	if (data.bytes [0] != 0x78 || data.bytes [1] != 0x56 || data.bytes [2] != 0x34 || data.bytes [3] != 0x12) {
		printf("\nFAILED,  H2LE_32(0x%08x) -> { 0x%02x, 0x%02x, 0x%02x, 0x%02x }\n",
			data.u32, data.bytes [0] & 0xff, data.bytes [1] & 0xff, data.bytes [2] & 0xff, data.bytes [3] & 0xff);
		return false;
	}
	puts("OK");

	printf("\nPASSED!\n");
	return true;
}
