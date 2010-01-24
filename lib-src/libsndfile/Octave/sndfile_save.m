## Copyright (C) 2002  Erik de Castro Lopo
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this file.  If not, write to the Free Software Foundation,
## 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} sndfile_save (@var{filename, data, fs})
## Save the given @var{data} as audio data to the given at @var{fs}. Set
## the sample rate to @var{fs}.
## @end deftypefn

## Author: Erik de Castro Lopo <erikd@mega-nerd.com>
## Description: Save data as a sound file

function sndfile_save (filename, data, fs)

if nargin != 3,
	error ("Need three input arguments: filename, data and fs.") ;
	endif

if (! isstr (filename)),
	error ("First parameter 'filename' is must be a string.") ;
	endif

if (max (size (fs)) > 1),
	error ("Second parameter 'fs' must be a single value, not an array or matrix.") ;
	endif

[nr nc] = size (data) ;

if (nr > nc),
	data = data' ;
	endif

samplerate = fs ;
wavedata = data ;

str = sprintf ("save -mat-binary %s samplerate wavedata", filename) ;

eval (str) ;

endfunction
