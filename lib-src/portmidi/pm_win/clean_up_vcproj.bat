@echo off
rem Start this program in portmidi\pm_win
cd ..\pm_common

rename portmidi-static.vcproj portmidi-static.vcproj-cmake
gawk -f ..\pm_win\clean_up_vcproj.awk -v base_relative=.. portmidi-static.vcproj-cmake > portmidi-static.vcproj
del portmidi-static.vcproj-cmake
echo portmidi-static

rename pmjni.vcproj pmjni.vcproj-cmake
gawk -f ..\pm_win\clean_up_vcproj.awk -v base_relative=.. pmjni.vcproj-cmake > pmjni.vcproj
del pmjni.vcproj-cmake
echo pmjni

cd ../pm_dylib

rename portmidi-dynamic.vcproj portmidi-dynamic.vcproj-cmake
gawk -f ..\pm_win\clean_up_vcproj.awk -v base_relative=.. portmidi-dynamic.vcproj-cmake > portmidi-dynamic.vcproj
del portmidi-dynamic.vcproj-cmake
echo portmidi-dynamic

cd ..\pm_test

rename latency.vcproj latency.vcproj-cmake
gawk -f ..\pm_win\clean_up_vcproj.awk -v base_relative=.. latency.vcproj-cmake > latency.vcproj
del latency.vcproj-cmake
echo latency

rename midiclock.vcproj midiclock.vcproj-cmake
gawk -f ..\pm_win\clean_up_vcproj.awk -v base_relative=.. midiclock.vcproj-cmake > midiclock.vcproj
del midiclock.vcproj-cmake
echo midiclock

rename midithread.vcproj midithread.vcproj-cmake
gawk -f ..\pm_win\clean_up_vcproj.awk -v base_relative=.. midithread.vcproj-cmake > midithread.vcproj
del midithread.vcproj-cmake
echo midithread

rename midithru.vcproj midithru.vcproj-cmake
gawk -f ..\pm_win\clean_up_vcproj.awk -v base_relative=.. midithru.vcproj-cmake > midithru.vcproj
del midithru.vcproj-cmake
echo midithru

rename mm.vcproj mm.vcproj-cmake
gawk -f ..\pm_win\clean_up_vcproj.awk -v base_relative=.. mm.vcproj-cmake > mm.vcproj
del mm.vcproj-cmake
echo mm

rename qtest.vcproj qtest.vcproj-cmake
gawk -f ..\pm_win\clean_up_vcproj.awk -v base_relative=.. qtest.vcproj-cmake > qtest.vcproj
del qtest.vcproj-cmake
echo qtest

rename sysex.vcproj sysex.vcproj-cmake
gawk -f ..\pm_win\clean_up_vcproj.awk -v base_relative=.. sysex.vcproj-cmake > sysex.vcproj
del sysex.vcproj-cmake
echo sysex

rename test.vcproj test.vcproj-cmake
gawk -f ..\pm_win\clean_up_vcproj.awk -v base_relative=.. test.vcproj-cmake > test.vcproj
del test.vcproj-cmake

cd ..
echo test

rename ALL_BUILD.vcproj ALL_BUILD.vcproj-cmake
gawk -f pm_win\clean_up_vcproj.awk -v base_relative=. ALL_BUILD.vcproj-cmake > ALL_BUILD.vcproj
del ALL_BUILD.vcproj-cmake
echo ALL_BUILD

rename ZERO_CHECK.vcproj ZERO_CHECK.vcproj-cmake
gawk -f pm_win\clean_up_vcproj.awk -v base_relative=. ZERO_CHECK.vcproj-cmake > ZERO_CHECK.vcproj
del ZERO_CHECK.vcproj-cmake
echo ZERO_CHECK

cd pm_win
