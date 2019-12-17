/*
  Copyright 2011-2016 David Robillard <http://drobilla.net>

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/

#include "serd_internal.h"

#include <math.h>

void
serd_free(void* ptr)
{
	free(ptr);
}

const uint8_t*
serd_strerror(SerdStatus status)
{
	switch (status) {
	case SERD_SUCCESS:        return (const uint8_t*)"Success";
	case SERD_FAILURE:        return (const uint8_t*)"Non-fatal failure";
	case SERD_ERR_UNKNOWN:    return (const uint8_t*)"Unknown error";
	case SERD_ERR_BAD_SYNTAX: return (const uint8_t*)"Invalid syntax";
	case SERD_ERR_BAD_ARG:    return (const uint8_t*)"Invalid argument";
	case SERD_ERR_NOT_FOUND:  return (const uint8_t*)"Not found";
	case SERD_ERR_ID_CLASH:   return (const uint8_t*)"Blank node ID clash";
	case SERD_ERR_BAD_CURIE:  return (const uint8_t*)"Invalid CURIE";
	case SERD_ERR_INTERNAL:   return (const uint8_t*)"Internal error";
	}
	return (const uint8_t*)"Unknown error";  // never reached
}

static inline void
serd_update_flags(const uint8_t c, SerdNodeFlags* const flags)
{
	switch (c) {
	case '\r': case '\n':
		*flags |= SERD_HAS_NEWLINE;
		break;
	case '"':
		*flags |= SERD_HAS_QUOTE;
	}
}

size_t
serd_substrlen(const uint8_t* const str,
               const size_t         len,
               size_t* const        n_bytes,
               SerdNodeFlags* const flags)
{
	size_t        n_chars = 0;
	size_t        i       = 0;
	SerdNodeFlags f       = 0;
	for (; i < len && str[i]; ++i) {
		if ((str[i] & 0xC0) != 0x80) {  // Start of new character
			++n_chars;
			serd_update_flags(str[i], &f);
		}
	}
	if (n_bytes) {
		*n_bytes = i;
	}
	if (flags) {
		*flags = f;
	}
	return n_chars;
}

size_t
serd_strlen(const uint8_t* str, size_t* n_bytes, SerdNodeFlags* flags)
{
	size_t        n_chars = 0;
	size_t        i       = 0;
	SerdNodeFlags f       = 0;
	for (; str[i]; ++i) {
		if ((str[i] & 0xC0) != 0x80) {  // Start of new character
			++n_chars;
			serd_update_flags(str[i], &f);
		}
	}
	if (n_bytes) {
		*n_bytes = i;
	}
	if (flags) {
		*flags = f;
	}
	return n_chars;
}

static inline double
read_sign(const char** sptr)
{
	double sign = 1.0;
	switch (**sptr) {
	case '-':
		sign = -1.0;
		// fallthru
	case '+':
		++(*sptr);
		// fallthru
	default:
		return sign;
	}
}

double
serd_strtod(const char* str, char** endptr)
{
	double result = 0.0;

	// Point s at the first non-whitespace character
	const char* s = str;
	while (is_space(*s)) { ++s; }

	// Read leading sign if necessary
	const double sign = read_sign(&s);

	// Parse integer part
	for (; is_digit(*s); ++s) {
		result = (result * 10.0) + (*s - '0');
	}

	// Parse fractional part
	if (*s == '.') {
		double denom = 10.0;
		for (++s; is_digit(*s); ++s) {
			result += (*s - '0') / denom;
			denom *= 10.0;
		}
	}

	// Parse exponent
	if (*s == 'e' || *s == 'E') {
		++s;
		double expt      = 0.0;
		double expt_sign = read_sign(&s);
		for (; is_digit(*s); ++s) {
			expt = (expt * 10.0) + (*s - '0');
		}
		result *= pow(10, expt * expt_sign);
	}

	if (endptr) {
		*endptr = (char*)s;
	}

	return result * sign;
}

/**
   Base64 decoding table.
   This is indexed by encoded characters and returns the numeric value used
   for decoding, shifted up by 47 to be in the range of printable ASCII.
   A '$' is a placeholder for characters not in the base64 alphabet.
*/
static const char b64_unmap[] =
	"$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$m$$$ncdefghijkl$$$$$$"
	"$/0123456789:;<=>?@ABCDEFGH$$$$$$IJKLMNOPQRSTUVWXYZ[\\]^_`ab$$$$"
	"$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"
	"$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$";

static inline uint8_t unmap(const uint8_t in) { return b64_unmap[in] - 47; }

/**
   Decode 4 base64 characters to 3 raw bytes.
*/
static inline size_t
decode_chunk(const uint8_t in[4], uint8_t out[3])
{
	out[0] = (uint8_t)(((unmap(in[0]) << 2))        | unmap(in[1]) >> 4);
	out[1] = (uint8_t)(((unmap(in[1]) << 4) & 0xF0) | unmap(in[2]) >> 2);
	out[2] = (uint8_t)(((unmap(in[2]) << 6) & 0xC0) | unmap(in[3]));
	return 1 + (in[2] != '=') + ((in[2] != '=') && (in[3] != '='));
}

void*
serd_base64_decode(const uint8_t* str, size_t len, size_t* size)
{
	void* buf = malloc((len * 3) / 4 + 2);
	*size = 0;
	for (size_t i = 0, j = 0; i < len; j += 3) {
		uint8_t in[] = "====";
		size_t  n_in = 0;
		for (; i < len && n_in < 4; ++n_in) {
			for (; i < len && !is_base64(str[i]); ++i) {}  // Skip junk
			in[n_in] = str[i++];
		}
		if (n_in > 1) {
			*size += decode_chunk(in, (uint8_t*)buf + j);
		}
	}
	return buf;
}
