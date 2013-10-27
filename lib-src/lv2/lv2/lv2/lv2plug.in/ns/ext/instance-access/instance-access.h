/*
  LV2 Instance Access Extension
  Copyright 2008-2012 David Robillard <http://drobilla.net>

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

#ifndef LV2_INSTANCE_ACCESS_H
#define LV2_INSTANCE_ACCESS_H

#define LV2_INSTANCE_ACCESS_URI "http://lv2plug.in/ns/ext/instance-access"

/**
   @file instance-access.h
   C header for the LV2 Instance Access extension
   <http://lv2plug.in/ns/ext/instance-access>.
 
   This extension defines a method for (e.g.) plugin UIs to get a direct
   handle to an LV2 plugin instance (LV2_Handle), if possible.
 
   To support this feature the host must pass an LV2_Feature struct to the
   UI instantiate method with URI "http://lv2plug.in/ns/ext/instance-access"
   and data pointed directly to the LV2_Handle of the plugin instance.
*/

#endif  /* LV2_INSTANCE_ACCESS_H */

