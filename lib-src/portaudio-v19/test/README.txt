TODO - This should be moved into a doxydoc page.

For more information on the TestPlan please visit:

  http://www.portaudio.com/trac/wiki/TestPlan
  
This directory contains various programs to test PortAudio. The files 
named patest_* are tests.

All following tests are up to date with the V19 API. They should all compile
(without any warnings on GCC 3.3). Note that this does not necessarily mean that 
the tests pass, just that they compile.

    x- paqa_devs.c 
    x- paqa_errs.c   (needs reviewing)
    x- patest1.c
    x- patest_buffer.c
    x- patest_callbackstop.c
    x- patest_clip.c (last test fails, dither doesn't currently force clip in V19)
    x- patest_dither.c
    x- patest_hang.c
    x- patest_latency.c
    x- patest_leftright.c
    x- patest_longsine.c
    x- patest_many.c
    x- patest_maxsines.c
	x- patest_mono.c
    x- patest_multi_sine.c
    x- patest_pink.c
    x- patest_prime.c
    x- patest_read_record.c
    x- patest_record.c
    x- patest_ringmix.c
    x- patest_saw.c
    x- patest_sine.c
    x- patest_sine8.c
    x- patest_sine_formats.c
    x- patest_sine_time.c
    x- patest_start_stop.c
    x- patest_stop.c
    x- patest_sync.c
    x- patest_toomanysines.c
	x- patest_two_rates.c
    x- patest_underflow.c
    x- patest_wire.c
    x- patest_write_sine.c
    x- pa_devs.c
    x- pa_fuzz.c
    x- pa_minlat.c

Note that Phil Burk deleted the debug_* tests on 2/26/11. They were just hacked
versions of old V18 tests. If we need to debug then we can just hack a working V19 test.
