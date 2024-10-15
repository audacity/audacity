#ifdef __cplusplus 
extern "C" {
#endif

void get_xlisp_path(char *p, long p_max);
char *getfilename(char *deflt, char *extension, char *mode, char *prompt);
FILE *fileopen(char *deflt, char *extension, char *mode, char *prompt);

#ifdef __cplusplus 
}
#endif
