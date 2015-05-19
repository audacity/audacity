/* libxmms-flac - XMMS FLAC input plugin
 * Copyright (C) 2004-2009  Josh Coalson
 * Copyright (C) 2011-2014  Xiph.Org Foundation
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

#ifndef FLAC__PLUGIN_XMMS__PLUGIN_H
#define FLAC__PLUGIN_XMMS__PLUGIN_H

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#if defined(__GNUC_STDC_INLINE__)
#  define G_INLINE_FUNC extern inline __attribute__((gnu_inline))
#endif

void set_track_info(const char* title, int length_in_msec);

#endif
