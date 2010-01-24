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
## @deftypefn {Function File} {} sndfile_load (@var{filename})
## Load data from the file given by @var{filename}.
## @end deftypefn

## Author: Erik de Castro Lopo <erikd@mega-nerd.com>
## Description: Load the sound data from the given file name

function [data fs] = sndfile_load (filename)

if (nargin != 1),
	error ("Need an input filename") ;
	endif

samplerate = -1 ;
samplingrate = -1 ;
wavedata = -1 ;


eval (sprintf ('load -f %s', filename)) ; 

if (samplerate > 0),
	fs = samplerate ;
elseif (samplingrate > 0),
	fs = samplingrate ;
else
	error ("Not able to find sample rate.") ;
	endif
	
if (max (size (wavedata)) > 1),
	data = wavedata ;
else
	error ("Not able to find waveform data.") ;
	endif

endfunction
