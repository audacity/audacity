#define portResType                             'port'
#define timePortResInfoID               128             
#define inputPortResInfoID              129     
#define outputPortResInfoID             130     



extern short InputRefNum;   /* Input port reference number. */
extern short OutputRefNum;  /* Output port reference number. */
extern short TimeRefNum;    /* Time base port reference number. */

void setup_midimgr(void);
void finish_midimgr(void);
void midi_show_errors();
