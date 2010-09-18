/* miditime.c -- a test program that sends midi clock and MTC */

#include "portmidi.h"
#include "porttime.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>

#ifndef false
#define false 0
#define true 1
#endif

#define private static
typedef int boolean;

#define MIDI_TIME_CLOCK 0xf8
#define MIDI_START      0xfa
#define MIDI_CONTINUE	0xfb
#define MIDI_STOP       0xfc
#define MIDI_Q_FRAME	0xf1

#define OUTPUT_BUFFER_SIZE 0
#define DRIVER_INFO NULL
#define TIME_PROC ((int32_t (*)(void *)) Pt_Time)
#define TIME_INFO NULL
#define LATENCY 0
#define TIME_START Pt_Start(1, 0, 0) /* timer started w/millisecond accuracy */

#define STRING_MAX 80 /* used for console input */

/* to determine ms per clock:
 *    time per beat in seconds =  60 / tempo
 *    multiply by 1000 to get time per beat in ms: 60000 / tempo
 *    divide by 24 CLOCKs per beat: (60000/24) / tempo
 *    simplify: 2500 / tempo
 */
#define TEMPO_TO_CLOCK 2500.0

boolean done = false;
PmStream *midi;
/* shared flags to control callback output generation: */
boolean clock_running = false;
boolean send_start_stop = false;
boolean time_code_running = false;
boolean active = false; /* tells callback to do its thing */
float tempo = 60.0F;
/* protocol for handing off portmidi to callback thread:
    main owns portmidi
    main sets active = true: ownership transfers to callback
    main sets active = false: main requests ownership
    callback sees active == false, yields ownership back to main
    main waits 2ms to make sure callback has a chance to yield
       (stop making PortMidi calls), then assumes it can close
       PortMidi
 */

/* timer_poll -- the timer callback function */
/*
 * All MIDI sends take place here
 */
void timer_poll(PtTimestamp timestamp, void *userData)
{
    static int callback_owns_portmidi = false;
    static PmTimestamp clock_start_time = 0;
    static double next_clock_time = 0;
    /* SMPTE time */
    static int frames = 0;
    static int seconds = 0;
    static int minutes = 0;
    static int hours = 0;
    static int mtc_count = 0; /* where are we in quarter frame sequence? */
    static int smpte_start_time = 0;
    static double next_smpte_time = 0;
    #define QUARTER_FRAME_PERIOD (1.0 / 120.0) /* 30fps, 1/4 frame */

    if (callback_owns_portmidi && !active) {
        /* main is requesting (by setting active to false) that we shut down */
        callback_owns_portmidi = false;
        return;
    }
    if (!active) return; /* main still getting ready or it's closing down */
    callback_owns_portmidi = true; /* main is ready, we have portmidi */
    if (send_start_stop) {
        if (clock_running) {
            Pm_WriteShort(midi, 0, MIDI_STOP);
        } else {
            Pm_WriteShort(midi, 0, MIDI_START);
            clock_start_time = timestamp;
            next_clock_time = TEMPO_TO_CLOCK / tempo;
        }
        clock_running = !clock_running;
        send_start_stop = false; /* until main sets it again */
        /* note that there's a slight race condition here: main could
           set send_start_stop asynchronously, but we assume user is 
           typing slower than the clock rate */
    }
    if (clock_running) {
        if ((timestamp - clock_start_time) > next_clock_time) {
            Pm_WriteShort(midi, 0, MIDI_TIME_CLOCK);
            next_clock_time += TEMPO_TO_CLOCK / tempo;
        }
    }
    if (time_code_running) {
        int data = 0; // initialization avoids compiler warning
        if ((timestamp - smpte_start_time) < next_smpte_time) 
            return;
        switch (mtc_count) {
        case 0: /* frames low nibble */
            data = frames;
            break;
        case 1: /* frames high nibble */
            data = frames >> 4;
            break;
        case 2: /* frames seconds low nibble */
            data = seconds;
            break;
        case 3: /* frames seconds high nibble */
            data = seconds >> 4;
            break;
        case 4: /* frames minutes low nibble */
            data = minutes;
            break;
        case 5: /* frames minutes high nibble */
            data = minutes >> 4;
            break;
        case 6: /* hours low nibble */
            data = hours;
            break;
        case 7: /* hours high nibble */
            data = hours >> 4;
            break;
        }
        data &= 0xF; /* take only 4 bits */
        Pm_WriteShort(midi, 0, 
                      Pm_Message(MIDI_Q_FRAME, (mtc_count << 4) + data, 0));
        mtc_count = (mtc_count + 1) & 7; /* wrap around */
        if (mtc_count == 0) { /* update time by two frames */
            frames += 2;
            if (frames >= 30) {
                frames = 0;
                seconds++;
                if (seconds >= 60) {
                    seconds = 0;
                    minutes++;
                    if (minutes >= 60) {
                        minutes = 0;
                        hours++;
                        /* just let hours wrap if it gets that far */
                    }
                }
            }
        }
        next_smpte_time += QUARTER_FRAME_PERIOD;
    } else { /* time_code_running is false */
        smpte_start_time = timestamp;
        /* so that when it finally starts, we'll be in sync */
    }
}


/* read a number from console */
/**/
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

/****************************************************************************
*               showhelp
* Effect: print help text
****************************************************************************/

private void showhelp()
{
    printf("\n");
    printf("t toggles sending MIDI Time Code (MTC)\n");
    printf("c toggles sending MIDI CLOCK (initially on)\n");
    printf("m to set tempo (from 1bpm to 300bpm)\n");
    printf("q quits\n");
    printf("\n");
}

/****************************************************************************
*               doascii
* Inputs:
*    char c: input character
* Effect: interpret to control output
****************************************************************************/

private void doascii(char c)
{
    if (isupper(c)) c = tolower(c);
    if (c == 'q') done = true;
    else if (c == 'c') {
        printf("%s MIDI CLOCKs\n", (clock_running ? "Stopping" : "Starting"));
        send_start_stop = true;
    } else if (c == 't') {
        printf("%s MIDI Time Code\n", 
               (time_code_running ? "Stopping" : "Starting"));
        time_code_running = !time_code_running;
    } else if (c == 'm') {
        int input_tempo = get_number("Enter new tempo (bpm): ");
        if (input_tempo >= 1 && input_tempo <= 300) {
            printf("Changing tempo to %d\n", input_tempo);
            tempo = (float) input_tempo;
        } else {
            printf("Tempo range is 1 to 300, current tempo is %g bpm\n", 
                   tempo);
        }
    } else {
        showhelp();
    }
}


/* main - prompt for parameters, start processing */
/*
 * Prompt user to type return.
 * Then send START and MIDI CLOCK for 60 beats/min.
 * Commands:
 *     t - toggle sending MIDI Time Code (MTC)
 *     c - toggle sending MIDI CLOCK
 *     m - set tempo
 *     q - quit
 */
int main(int argc, char **argv)
{
    char s[STRING_MAX]; /* console input */
    int outp;
    PmError err;
    int i;
    if (argc > 1) { 
        printf("Warning: command line arguments ignored\n");
    }
    showhelp();
    /* use porttime callback to send midi */
    Pt_Start(1, timer_poll, 0);
    /* list device information */
    printf("MIDI output devices:\n");
    for (i = 0; i < Pm_CountDevices(); i++) {
        const PmDeviceInfo *info = Pm_GetDeviceInfo(i);
        if (info->output) printf("%d: %s, %s\n", i, info->interf, info->name);
    }
    outp = get_number("Type output device number: ");
    err = Pm_OpenOutput(&midi, outp, DRIVER_INFO, OUTPUT_BUFFER_SIZE, 
                        TIME_PROC, TIME_INFO, LATENCY);
    if (err) {
        printf(Pm_GetErrorText(err));
        goto error_exit_no_device;
    }
    active = true;

    printf("Type RETURN to start MIDI CLOCK:\n");
    if (!fgets(s, STRING_MAX, stdin)) goto error_exit;
    send_start_stop = true; /* send START and then CLOCKs */

    while (!done) {
        if (fgets(s, STRING_MAX, stdin)) {
            doascii(s[0]);
        }
    }

 error_exit:
    active = false;
    Pt_Sleep(2); /* this is to allow callback to complete -- it's
                    real time, so it's either ok and it runs on
                    time, or there's no point to synchronizing
                    with it */
    /* now we "own" portmidi again */
    Pm_Close(midi);
 error_exit_no_device:
    Pt_Stop();
    Pm_Terminate();
    exit(0);
}

