#define CR '\n'
#define ABORT_CHAR 0x03
#define BREAK_CHAR 0x02
#define CLEANUP_CHAR 0x07
#define INFO_CHAR '\024'

#define BREAK_LEVEL 1
#define ABORT_LEVEL 2

#ifdef __cplusplus
extern "C" {
#endif

int ggetchar(void);
void gprintf(long where, char *format, ...);
void gputchar(int c);
int get_ascii(char *c);
char *ggets(char *str);
int check_aborted();
int askbool(char *prompt, int deflt);
char wait_ascii();
void io_init();
/* this is not entirely kosher: nyquist_printf is also declared in sound.h
   so that all C programs will see it. Perhaps it should go into cext.h, but
   I'm not sure I want to drag all that into here. 
 */
void nyquist_printf(const char *format, ...);

#ifdef __cplusplus
}
#endif

#define GTRANS   0
#define GERROR   1
#define GFATAL   2
#define GDEBUG   3
