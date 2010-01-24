/* tempomap.h -- list of tempo changes */

typedef struct tempochange_struct {
    struct tempochange_struct *next;
    time_type rtime;
    long beat;
    long tempo;
} tempochange_node, *tempochange_type;


typedef struct tempomap_struct {
    tempochange_type entries;
    tempochange_type hint;
} tempomap_node, *tempomap_type;


#define tempochange_alloc() (tempochange_type) memget(sizeof(tempochange_node))
#define tempochange_free(tc) memfree(tc, sizeof(tempochange_node))
#define tempomap_alloc() (tempomap_type) memget(sizeof(tempomap_node))
tempomap_type tempomap_create(void);
void tempomap_free(tempomap_type tm);
void tempomap_insert(tempomap_type tempomap, long beat, long tempo);
time_type tempomap_lookup(tempomap_type tempomap, long beat);

    
