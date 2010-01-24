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
## @deftypefn {Function File} {} sndfile_play (@var{data, fs})
## Play @var{data} at sample rate @var{fs} using the sndfile-play
## program.
## @end deftypefn

## Author: Erik de Castro Lopo <erikd@mega-nerd.com>
## Description: Play the given data as a sound file

function sndfile_play (data, fs)

if nargin != 2,
	error ("Need two input arguments: data and fs.") ;
	endif

if (max (size (fs)) > 1),
	error ("Second parameter fs must be a single value.") ;
	endif

[nr nc] = size (data) ;

if (nr > nc),
	data = data' ;
	endif

samplerate = fs ;
wavedata = data ;

filename = tmpnam () ;

cmd = sprintf ("save -mat-binary %s fs data", filename) ;

eval (cmd) ;

cmd = sprintf ("sndfile-play %s", filename) ;

[output, status] = system (cmd) ;

if (status),
	disp (outout) ;
	endif

endfunction
