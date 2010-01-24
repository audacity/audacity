/*
** Copyright (C) 2008-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU Lesser General Public License as published by
** the Free Software Foundation; either version 2.1 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU Lesser General Public License for more details.
**
** You should have received a copy of the GNU Lesser General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

static inline void
print_test_name (const char * name)
{	printf ("    %-40s : ", name) ;
	fflush (stdout) ;
} /* print_test_name */



void test_conversions (void) ;
void test_endswap (void) ;
void test_log_printf (void) ;
void test_file_io (void) ;

void test_float_convert (void) ;
void test_double_convert (void) ;

void test_audio_detect (void) ;
void test_ima_oki_adpcm (void) ;
