
xtype_desc create_desc(char *type_name, void (*fm)(void*),
                       void (*pm)(LVAL, void*),
                       void (*sm)(FILE*, void*), unsigned char * (*rm)(FILE*),
                       void (*mm)(void*));
int exttypep(LVAL x, LVAL type_sym);


