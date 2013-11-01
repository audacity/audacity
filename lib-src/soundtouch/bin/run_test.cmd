set SOUND_DIR=d:\dev\test_sounds
set OUT_DIR=.
set TEST_NAME=semmari
set OUT_NAME=out
set SS=soundstretch
set TEST_PARAM=-pitch=-3 -bpm

call %SS% %SOUND_DIR%\%TEST_NAME%-8b1.wav %OUT_DIR%\%OUT_NAME%-8b1.wav %TEST_PARAM%
call %SS% %SOUND_DIR%\%TEST_NAME%-8b2.wav %OUT_DIR%\%OUT_NAME%-8b2.wav %TEST_PARAM%

call %SS% %SOUND_DIR%\%TEST_NAME%-16b1.wav %OUT_DIR%\%OUT_NAME%-16b1.wav %TEST_PARAM%
call %SS% %SOUND_DIR%\%TEST_NAME%-16b2.wav %OUT_DIR%\%OUT_NAME%-16b2.wav %TEST_PARAM%

call %SS% %SOUND_DIR%\%TEST_NAME%-24b1.wav %OUT_DIR%\%OUT_NAME%-24b1.wav %TEST_PARAM%
call %SS% %SOUND_DIR%\%TEST_NAME%-24b2.wav %OUT_DIR%\%OUT_NAME%-24b2.wav %TEST_PARAM%

call %SS% %SOUND_DIR%\%TEST_NAME%-32b1.wav %OUT_DIR%\%OUT_NAME%-32b1.wav %TEST_PARAM%
call %SS% %SOUND_DIR%\%TEST_NAME%-32b2.wav %OUT_DIR%\%OUT_NAME%-32b2.wav %TEST_PARAM%

