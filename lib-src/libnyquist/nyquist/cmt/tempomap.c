/* tempomap.c -- used by midifile reader to record and lookup time maps */
/*
 *  This module is designed to provide tempo change list insert and
 *  lookup facilities.  The exact interpretation of beat and tempo are
 *  defined in one function, elapsed_time, which takes a tempo and a
 *  number of beats, and returns time.
 */

#include "cext.h"
#include "midifns.h"
#include "timebase.h"
#include "moxc.h"
#include "seq.h"
#include "tempomap.h"

static time_type elapsed_time(long tempo, long beat);

/* tempomap_create -- create a tempomap */
/**/
tempomap_type tempomap_create()
{
    tempochange_type tempochange = tempochange_alloc();
    tempomap_type tempomap = tempomap_alloc();
    tempomap->hint = tempomap->entries = tempochange;
    tempochange->beat = 0L;
    /* default tempo is 120 (= 50000microsec/beat) and 24 divisions/quarter */
    /* this may be overridden by a tempomap_insert */
    tempochange->tempo = 500000L / 24;
    tempochange->rtime = 0L;
    tempochange->next = NULL;
    return tempomap;
}


/* tempomap_free -- deallocate storage for tempo map */
/**/
void tempomap_free(tm)
  tempomap_type tm;
{
    while (tm->entries) {
        tempochange_type tc = tm->entries;
        tm->entries = tc->next;
        tempochange_free(tc);
    }
    memfree(tm, sizeof(tempomap_node));
}


/* tempomap_insert -- insert a tempo change into the map */
/**/
void tempomap_insert(tempomap, beat, tempo)
  tempomap_type tempomap;
  long beat;    /* beat division number */
  long tempo;   /* microseconds per beat division */
{
    tempochange_type tempochange = tempochange_alloc();
    register tempochange_type prev;
    register tempochange_type next;

    tempochange->tempo = tempo;
    tempochange->beat = beat;

    if ((!(tempomap->hint->next)) ||
          (tempomap->hint->beat > beat))
        tempomap->hint = tempomap->entries;

    /* find the insert point */
    for (prev = tempomap->hint;
         (next = prev->next) && (next->beat <= beat);
         prev = next);

     /* make the insert */
    tempochange->next = next;
    prev->next = tempochange;
    tempomap->hint = prev;

    /* update the real time of each change */
    for (tempochange = prev;
         tempochange->next;
         tempochange = tempochange->next) {
        tempochange->next->rtime =
            tempochange->rtime +
            elapsed_time(tempochange->tempo,
                         tempochange->next->beat - tempochange->beat);
    }
}


/* tempomap_lookup -- convert beat to 4us time */
/*
 * The returned time is real time in units of 4us.
 */
time_type tempomap_lookup(tempomap, beat)
  tempomap_type tempomap;
  long beat;
{
    register tempochange_type prev;
    register tempochange_type next;

    if ((!(tempomap->hint->next)) || 
          (tempomap->hint->beat > beat))
        tempomap->hint = tempomap->entries;

    /* find the last inflection point */
    for (prev = tempomap->hint;
         (next = prev->next) && (next->beat <= beat);
         prev = next);

    /* interpolate */
    return prev->rtime +
           elapsed_time(prev->tempo, beat - prev->beat);
}


/* elapsed_time -- compute the real elapsed time at a given tempo */
/*
 * the time returned is in units of 4us.
 */
static time_type elapsed_time(long tempo, long beat)
{
    return (time_type)((tempo * beat) >> 2);
}
