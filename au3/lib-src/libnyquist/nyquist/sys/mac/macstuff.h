/* macstuff.h -- header for mac-specific functions */

void osfinish(void);
/* put searchpath into p, prefs_found tells if preference file exists */
void get_xlisp_path(char *p, long p_max, int *prefs_found);
void setup_preferences(char *filename);

