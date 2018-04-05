@echo off
rem SoX Resampler Library      Copyright (c) 2007-16 robs@users.sourceforge.net
rem Licence for this file: LGPL v2.1                  See LICENCE for details.

for /L %%i in (0,1,3) DO throughput 44.1 48 1 0 %%i
