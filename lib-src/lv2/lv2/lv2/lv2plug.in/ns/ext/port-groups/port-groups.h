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
   @file port-groups.h
   C definitions for the LV2 Port Groups extension
   <http://lv2plug.in/ns/ext/port-groups>.
*/

#ifndef LV2_PORT_GROUPS_H
#define LV2_PORT_GROUPS_H

#define LV2_PORT_GROUPS_URI    "http://lv2plug.in/ns/ext/port-groups"
#define LV2_PORT_GROUPS_PREFIX LV2_PORT_GROUPS_URI "#"

#define LV2_PORT_GROUPS__DiscreteGroup          LV2_PORT_GROUPS_PREFIX "DiscreteGroup"
#define LV2_PORT_GROUPS__Element                LV2_PORT_GROUPS_PREFIX "Element"
#define LV2_PORT_GROUPS__FivePointOneGroup      LV2_PORT_GROUPS_PREFIX "FivePointOneGroup"
#define LV2_PORT_GROUPS__FivePointZeroGroup     LV2_PORT_GROUPS_PREFIX "FivePointZeroGroup"
#define LV2_PORT_GROUPS__FourPointZeroGroup     LV2_PORT_GROUPS_PREFIX "FourPointZeroGroup"
#define LV2_PORT_GROUPS__Group                  LV2_PORT_GROUPS_PREFIX "Group"
#define LV2_PORT_GROUPS__InputGroup             LV2_PORT_GROUPS_PREFIX "InputGroup"
#define LV2_PORT_GROUPS__MidSideGroup           LV2_PORT_GROUPS_PREFIX "MidSideGroup"
#define LV2_PORT_GROUPS__MonoGroup              LV2_PORT_GROUPS_PREFIX "MonoGroup"
#define LV2_PORT_GROUPS__OutputGroup            LV2_PORT_GROUPS_PREFIX "OutputGroup"
#define LV2_PORT_GROUPS__SevenPointOneGroup     LV2_PORT_GROUPS_PREFIX "SevenPointOneGroup"
#define LV2_PORT_GROUPS__SevenPointOneWideGroup LV2_PORT_GROUPS_PREFIX "SevenPointOneWideGroup"
#define LV2_PORT_GROUPS__SixPointOneGroup       LV2_PORT_GROUPS_PREFIX "SixPointOneGroup"
#define LV2_PORT_GROUPS__StereoGroup            LV2_PORT_GROUPS_PREFIX "StereoGroup"
#define LV2_PORT_GROUPS__ThreePointZeroGroup    LV2_PORT_GROUPS_PREFIX "ThreePointZeroGroup"
#define LV2_PORT_GROUPS__center                 LV2_PORT_GROUPS_PREFIX "center"
#define LV2_PORT_GROUPS__centerLeft             LV2_PORT_GROUPS_PREFIX "centerLeft"
#define LV2_PORT_GROUPS__centerRight            LV2_PORT_GROUPS_PREFIX "centerRight"
#define LV2_PORT_GROUPS__element                LV2_PORT_GROUPS_PREFIX "element"
#define LV2_PORT_GROUPS__group                  LV2_PORT_GROUPS_PREFIX "group"
#define LV2_PORT_GROUPS__left                   LV2_PORT_GROUPS_PREFIX "left"
#define LV2_PORT_GROUPS__lowFrequencyEffects    LV2_PORT_GROUPS_PREFIX "lowFrequencyEffects"
#define LV2_PORT_GROUPS__mainInput              LV2_PORT_GROUPS_PREFIX "mainInput"
#define LV2_PORT_GROUPS__mainOutput             LV2_PORT_GROUPS_PREFIX "mainOutput"
#define LV2_PORT_GROUPS__rearCenter             LV2_PORT_GROUPS_PREFIX "rearCenter"
#define LV2_PORT_GROUPS__rearLeft               LV2_PORT_GROUPS_PREFIX "rearLeft"
#define LV2_PORT_GROUPS__rearRight              LV2_PORT_GROUPS_PREFIX "rearRight"
#define LV2_PORT_GROUPS__right                  LV2_PORT_GROUPS_PREFIX "right"
#define LV2_PORT_GROUPS__side                   LV2_PORT_GROUPS_PREFIX "side"
#define LV2_PORT_GROUPS__sideChainOf            LV2_PORT_GROUPS_PREFIX "sideChainOf"
#define LV2_PORT_GROUPS__sideLeft               LV2_PORT_GROUPS_PREFIX "sideLeft"
#define LV2_PORT_GROUPS__sideRight              LV2_PORT_GROUPS_PREFIX "sideRight"
#define LV2_PORT_GROUPS__source                 LV2_PORT_GROUPS_PREFIX "source"
#define LV2_PORT_GROUPS__subGroupOf             LV2_PORT_GROUPS_PREFIX "subGroupOf"

#endif  /* LV2_PORT_GROUPS_H */
