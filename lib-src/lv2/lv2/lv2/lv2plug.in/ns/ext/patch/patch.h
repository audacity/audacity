/*
  Copyright 2012 David Robillard <http://drobilla.net>

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

/**
   @file patch.h C header for the LV2 Patch extension
   <http://lv2plug.in/ns/ext/patch>.

   The patch extension is purely data, this header merely defines URIs
   for convenience.
*/

#ifndef LV2_PATCH_H
#define LV2_PATCH_H

#define LV2_PATCH_URI    "http://lv2plug.in/ns/ext/patch"
#define LV2_PATCH_PREFIX LV2_PATCH_URI "#"

#define LV2_PATCH__Ack         LV2_PATCH_PREFIX "Ack"
#define LV2_PATCH__Delete      LV2_PATCH_PREFIX "Delete"
#define LV2_PATCH__Error       LV2_PATCH_PREFIX "Error"
#define LV2_PATCH__Get         LV2_PATCH_PREFIX "Get"
#define LV2_PATCH__Message     LV2_PATCH_PREFIX "Message"
#define LV2_PATCH__Move        LV2_PATCH_PREFIX "Move"
#define LV2_PATCH__Patch       LV2_PATCH_PREFIX "Patch"
#define LV2_PATCH__Post        LV2_PATCH_PREFIX "Post"
#define LV2_PATCH__Put         LV2_PATCH_PREFIX "Put"
#define LV2_PATCH__Request     LV2_PATCH_PREFIX "Request"
#define LV2_PATCH__Response    LV2_PATCH_PREFIX "Response"
#define LV2_PATCH__Set         LV2_PATCH_PREFIX "Set"
#define LV2_PATCH__add         LV2_PATCH_PREFIX "add"
#define LV2_PATCH__body        LV2_PATCH_PREFIX "body"
#define LV2_PATCH__destination LV2_PATCH_PREFIX "destination"
#define LV2_PATCH__property    LV2_PATCH_PREFIX "property"
#define LV2_PATCH__readable    LV2_PATCH_PREFIX "readable"
#define LV2_PATCH__remove      LV2_PATCH_PREFIX "remove"
#define LV2_PATCH__request     LV2_PATCH_PREFIX "request"
#define LV2_PATCH__subject     LV2_PATCH_PREFIX "subject"
#define LV2_PATCH__value       LV2_PATCH_PREFIX "value"
#define LV2_PATCH__wildcard    LV2_PATCH_PREFIX "wildcard"
#define LV2_PATCH__writable    LV2_PATCH_PREFIX "writable"

#endif  /* LV2_PATCH_H */
