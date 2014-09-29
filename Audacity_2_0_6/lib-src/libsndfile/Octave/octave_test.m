# Copyright (C) 2007-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation; either version 2.1 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

# These tests are nowhere near comprehensive.

printf ("    Running Octave tests                     : ") ;
fflush (stdout) ;

filename = "whatever" ;
srate_out = 32000 ;
fmt_out = "wav-float" ;

t = (2 * pi / srate_out * (0:srate_out-1))' ;
data_out = sin (440.0 * t) ;

# Write out a file.
sfwrite (filename, data_out, srate_out, fmt_out) ;

# Read it back in again.
[ data_in, srate_in, fmt_in ] = sfread (filename) ;

if (srate_in != srate_out)
	error ("\n\nSample rate mismatch : %d -> %d.\n\n", srate_out, srate_in) ;
	endif

# Octave strcmp return 1 for the same.
if (strcmp (fmt_in, fmt_out) != 1)
	error ("\n\nFormat error : '%s' -> '%s'.\n\n", fmt_out, fmt_in) ;
	endif

err = max (abs (data_out - data_in)) ;

if (err > 1e-7)
	error ("err : %g\n", err) ;
	endif

printf ("ok") ;

unlink (filename) ;
