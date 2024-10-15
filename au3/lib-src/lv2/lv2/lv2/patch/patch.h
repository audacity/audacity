/*
  Copyright 2012-2016 David Robillard <http://drobilla.net>

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
   @defgroup patch Patch

   Messages for accessing and manipulating properties, see
   <http://lv2plug.in/ns/ext/patch> for details.

   Note the patch extension is purely data, this header merely defines URIs for
   convenience.

   @{
*/

#ifndef LV2_PATCH_H
#define LV2_PATCH_H

#define LV2_PATCH_URI    "http://lv2plug.in/ns/ext/patch"  ///< http://lv2plug.in/ns/ext/patch
#define LV2_PATCH_PREFIX LV2_PATCH_URI "#"                 ///< http://lv2plug.in/ns/ext/patch#

#define LV2_PATCH__Ack            LV2_PATCH_PREFIX "Ack"             ///< http://lv2plug.in/ns/ext/patch#Ack
#define LV2_PATCH__Delete         LV2_PATCH_PREFIX "Delete"          ///< http://lv2plug.in/ns/ext/patch#Delete
#define LV2_PATCH__Copy           LV2_PATCH_PREFIX "Copy"            ///< http://lv2plug.in/ns/ext/patch#Copy
#define LV2_PATCH__Error          LV2_PATCH_PREFIX "Error"           ///< http://lv2plug.in/ns/ext/patch#Error
#define LV2_PATCH__Get            LV2_PATCH_PREFIX "Get"             ///< http://lv2plug.in/ns/ext/patch#Get
#define LV2_PATCH__Message        LV2_PATCH_PREFIX "Message"         ///< http://lv2plug.in/ns/ext/patch#Message
#define LV2_PATCH__Move           LV2_PATCH_PREFIX "Move"            ///< http://lv2plug.in/ns/ext/patch#Move
#define LV2_PATCH__Patch          LV2_PATCH_PREFIX "Patch"           ///< http://lv2plug.in/ns/ext/patch#Patch
#define LV2_PATCH__Post           LV2_PATCH_PREFIX "Post"            ///< http://lv2plug.in/ns/ext/patch#Post
#define LV2_PATCH__Put            LV2_PATCH_PREFIX "Put"             ///< http://lv2plug.in/ns/ext/patch#Put
#define LV2_PATCH__Request        LV2_PATCH_PREFIX "Request"         ///< http://lv2plug.in/ns/ext/patch#Request
#define LV2_PATCH__Response       LV2_PATCH_PREFIX "Response"        ///< http://lv2plug.in/ns/ext/patch#Response
#define LV2_PATCH__Set            LV2_PATCH_PREFIX "Set"             ///< http://lv2plug.in/ns/ext/patch#Set
#define LV2_PATCH__accept         LV2_PATCH_PREFIX "accept"          ///< http://lv2plug.in/ns/ext/patch#accept
#define LV2_PATCH__add            LV2_PATCH_PREFIX "add"             ///< http://lv2plug.in/ns/ext/patch#add
#define LV2_PATCH__body           LV2_PATCH_PREFIX "body"            ///< http://lv2plug.in/ns/ext/patch#body
#define LV2_PATCH__context        LV2_PATCH_PREFIX "context"         ///< http://lv2plug.in/ns/ext/patch#context
#define LV2_PATCH__destination    LV2_PATCH_PREFIX "destination"     ///< http://lv2plug.in/ns/ext/patch#destination
#define LV2_PATCH__property       LV2_PATCH_PREFIX "property"        ///< http://lv2plug.in/ns/ext/patch#property
#define LV2_PATCH__readable       LV2_PATCH_PREFIX "readable"        ///< http://lv2plug.in/ns/ext/patch#readable
#define LV2_PATCH__remove         LV2_PATCH_PREFIX "remove"          ///< http://lv2plug.in/ns/ext/patch#remove
#define LV2_PATCH__request        LV2_PATCH_PREFIX "request"         ///< http://lv2plug.in/ns/ext/patch#request
#define LV2_PATCH__subject        LV2_PATCH_PREFIX "subject"         ///< http://lv2plug.in/ns/ext/patch#subject
#define LV2_PATCH__sequenceNumber LV2_PATCH_PREFIX "sequenceNumber"  ///< http://lv2plug.in/ns/ext/patch#sequenceNumber
#define LV2_PATCH__value          LV2_PATCH_PREFIX "value"           ///< http://lv2plug.in/ns/ext/patch#value
#define LV2_PATCH__wildcard       LV2_PATCH_PREFIX "wildcard"        ///< http://lv2plug.in/ns/ext/patch#wildcard
#define LV2_PATCH__writable       LV2_PATCH_PREFIX "writable"        ///< http://lv2plug.in/ns/ext/patch#writable

#endif  /* LV2_PATCH_H */

/**
   @}
*/
