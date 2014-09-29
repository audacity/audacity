/* latency.c -- measure latency of OS */

#include "porttime.h"
#include "portmidi.h"
#include "stdlib.h"
#include "stdio.h"
#include "string.h"
#include "assert.h"

/* Latency is defined here to mean the time starting when a
   process becomes ready to run, and ending when the process
   actually runs. Latency is due to contention for the
   processor, usually due to other processes, OS activity
   including device drivers handling interrupts, and
   waiting for the scheduler to suspend the currently running
   process and activate the one that is waiting.

   Latency can affect PortMidi applications: if a process fails
   to wake up promptly, MIDI input may sit in the input buffer
   waiting to be handled, and MIDI output may not be generated
   with accurate timing. Using the latency parameter when 
   opening a MIDI output port allows the caller to defer timing
   to PortMidi, which in most implementations will pass the
   data on to the OS. By passing timestamps and data to the
   OS kernel, device driver, or even hardware, there are fewer
   sources of latency that can affect the ultimate timing of
   the data. On the other hand, the application must generate
   and deliver the data ahead of the timestamp. The amount by 
   which data is computed early must be at least as large as
   the worst-case latency to avoid timing problems.

   Latency is even more important in audio applications. If an
   application lets an audio output buffer underflow, an audible
   pop or click is produced. Audio input buffers can overflow,
   causing data to be lost. In general the audio buffers must
   be large enough to buffer the worst-case latency that the
   application will encounter.

   This program measures latency by recording the difference
   between the scheduled callback time and the current real time.
   We do not really know the scheduled callback time, so we will
   record the differences between the real time of each callback
   and the real time of the previous callback. Differences that
   are larger than the scheduled difference are recorded. Smaller
   differences indicate the system is recovering from an earlier
   latency, so these are ignored.
   Since printing by the callback process can cause all sorts of
   delays, this program records latency observations in a
   histogram. When the program is stopped, the histogram is
   printed to the console.

   Optionally the system can be tested under a load of MIDI input,
   MIDI output, or both.  If MIDI input is selected, the callback
   thread will read any waiting MIDI events each iteration.  You
   must generate events on this interface for the test to actually
   put any appreciable load on PortMidi.  If MIDI output is
   selected, alternating note on and note off events are sent each
   X iterations, where you specify X.  For example, with a timer
   callback period of 2ms and X=1, a MIDI event is sent every 2ms.


   INTERPRETING RESULTS: Time is quantized to 1ms, so there is
   some uncertainty due to rounding. A microsecond latency that
   spans the time when the clock is incremented will be reported
   as a latency of 1. On the other hand, a latency of almost
   1ms that falls between two clock ticks will be reported as 
   zero. In general, if the highest nonzero bin is numbered N,
   then the maximum latency is N+1.

CHANGE LOG

18-Jul-03 Mark Nelson -- Added code to generate MIDI or receive
            MIDI during test, and made period user-settable.
 */

#define HIST_LEN 21 /* how many 1ms bins in the histogram */

#define STRING_MAX 80 /* used for console input */

#define INPUT_BUFFER_SIZE 100
#define OUTPUT_BUFFER_SIZE 0

#ifndef max
#define max(a, b) ((a) > (b) ? (a) : (b))
#endif
#ifndef min
#define min(a, b) ((a) <= (b) ? (a) : (b))
#endif

int get_number(char *prompt);

PtTimestamp previous_callback_time = 0;

int period;            /* milliseconds per callback */

int histogram[HIST_LEN];
int max_latency = 0;  /* worst latency observed */
int out_of_range = 0; /* how many points outside of HIST_LEN? */

int test_in, test_out; /* test MIDI in and/or out? */
int output_period;     /* output MIDI every __ iterations if test_out true */
int iteration = 0;
PmStream *in, *out;
int note_on = 0;       /* is the note currently on? */

/* callback function for PortTime -- computes histogram */
void pt_callback(PtTimestamp timestamp, void *userData)
{
    PtTimestamp difference = timestamp - previous_callback_time - period;
    previous_callback_time = timestamp;

    /* allow 5 seconds for the system to settle down */
    if (timestamp < 5000) return;

    iteration++;
    /* send a note on/off if user requested it */
    if (test_out && (iteration % output_period == 0)) {
        PmEvent buffer[1];
        buffer[0].timestamp = Pt_Time(NULL);
        if (note_on) {
            /* note off */
            buffer[0].message = Pm_Message(0x90, 60, 0);
            note_on = 0;
        } else {
            /* note on */
            buffer[0].message = Pm_Message(0x90, 60, 100);
            note_on = 1;
        }
        Pm_Write(out, buffer, 1);
        iteration = 0;
    }

    /* read all waiting events (if user requested) */
    if (test_in) {
       PmError status;
       PmEvent buffer[1];
       do {
          status = Pm_Poll(in);
          if (status == TRUE) {
              Pm_Read(in,buffer,1);
          }
       } while (status == TRUE);
    }

    if (difference < 0) return; /* ignore when system is "catching up" */

    /* update the histogram */
    if (difference < HIST_LEN) {
        histogram[difference]++;
    } else {
        out_of_range++;
    }

    if (max_latency < difference) max_latency = difference;
}


int main()
{
    char line[STRING_MAX];
    int i;
    int len;
    int choice;
    PtTimestamp stop;
    printf("Latency histogram.\n");
    period = 0;
    while (period < 1) {
        period = get_number("Choose timer period (in ms, >= 1): ");
    }
    printf("Benchmark with:\n\t%s\n\t%s\n\t%s\n\t%s\n",
           "1. No MIDI traffic",
           "2. MIDI input",
           "3. MIDI output",
           "4. MIDI input and output");
    choice = get_number("? ");
    switch (choice) {
      case 1: test_in = 0; test_out = 0; break;
      case 2: test_in = 1; test_out = 0; break;
      case 3: test_in = 0; test_out = 1; break;
      case 4: test_in = 1; test_out = 1; break;
      default: assert(0);
    }
    if (test_in || test_out) {
        /* list device information */
        for (i = 0; i < Pm_CountDevices(); i++) {
            const PmDeviceInfo *info = Pm_GetDeviceInfo(i);
            if ((test_in && info->input) ||
                (test_out && info->output)) {
                printf("%d: %s, %s", i, info->interf, info->name);
                if (info->input) printf(" (input)");
                if (info->output) printf(" (output)");
                printf("\n");
            }
        }
        /* open stream(s) */
        if (test_in) {
            int i = get_number("MIDI input device number: ");
            Pm_OpenInput(&in, 
                  i,
                  NULL, 
                  INPUT_BUFFER_SIZE, 
                  (PmTimestamp (*)(void *)) Pt_Time, 
                  NULL);
            /* turn on filtering; otherwise, input might overflow in the 
               5-second period before timer callback starts reading midi */
            Pm_SetFilter(in, PM_FILT_ACTIVE | PM_FILT_CLOCK);
        }
        if (test_out) {
            int i = get_number("MIDI output device number: ");
            PmEvent buffer[1];
            Pm_OpenOutput(&out, 
                  i,
                  NULL,
                  OUTPUT_BUFFER_SIZE,
                  (PmTimestamp (*)(void *)) Pt_Time,
                  NULL, 
                  0); /* no latency scheduling */

            /* send a program change to force a status byte -- this fixes
               a problem with a buggy linux MidiSport driver, and shouldn't
               hurt anything else
             */
            buffer[0].timestamp = 0;
            buffer[0].message = Pm_Message(0xC0, 0, 0); /* program change */
            Pm_Write(out, buffer, 1);

            output_period = get_number(
                "MIDI out should be sent every __ callback iterations: ");

            assert(output_period >= 1);
        }
    }

    printf("%s%s", "Latency measurements will start in 5 seconds. ",
                   "Type return to stop: ");
    Pt_Start(period, &pt_callback, 0);
    fgets(line, STRING_MAX, stdin);
    stop = Pt_Time();
    Pt_Stop();

    /* courteously turn off the last note, if necessary */
    if (note_on) {
       PmEvent buffer[1];
       buffer[0].timestamp = Pt_Time(NULL);
       buffer[0].message = Pm_Message(0x90, 60, 0);
       Pm_Write(out, buffer, 1);
    }

    /* print the histogram */
    printf("Duration of test: %g seconds\n\n", max(0, stop - 5000) * 0.001);
    printf("Latency(ms)  Number of occurrences\n");
    /* avoid printing beyond last non-zero histogram entry */
    len = min(HIST_LEN, max_latency + 1);
    for (i = 0; i < len; i++) {
        printf("%2d      %10d\n", i, histogram[i]);
    }
    printf("Number of points greater than %dms: %d\n", 
           HIST_LEN - 1, out_of_range);
    printf("Maximum latency: %d milliseconds\n", max_latency);
    printf("\nNote that due to rounding, actual latency can be 1ms higher\n");
    printf("than the numbers reported here.\n");
    printf("Type return to exit...");
    fgets(line, STRING_MAX, stdin);

	if(choice == 2)
		Pm_Close(in);
	else if(choice == 3)
		Pm_Close(out);
	else if(choice == 4)
	{
		Pm_Close(in);
		Pm_Close(out);
	}
    return 0;
}


/* read a number from console */
int get_number(char *prompt)
{
    char line[STRING_MAX];
    int n = 0, i;
    printf(prompt);
    while (n != 1) {
        n = scanf("%d", &i);
        fgets(line, STRING_MAX, stdin);

    }
    return i;
}
