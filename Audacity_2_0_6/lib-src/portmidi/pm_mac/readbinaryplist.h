/* readbinaryplist.h -- header to read preference files

   Roger B. Dannenberg, Jun 2008
*/

#include <stdint.h> /* for uint8_t ... */

#ifndef TRUE
    #define TRUE 1
    #define FALSE 0
#endif

#define MAX_KEY_SIZE 256

enum
{
    // Object tags (high nybble)
    kTAG_SIMPLE = 0x00,        // Null, true, false, filler, or invalid
    kTAG_INT = 0x10,
    kTAG_REAL = 0x20,
    kTAG_DATE = 0x30,
    kTAG_DATA = 0x40,
    kTAG_ASCIISTRING = 0x50,
    kTAG_UNICODESTRING = 0x60,
    kTAG_UID = 0x80,
    kTAG_ARRAY = 0xA0,
    kTAG_DICTIONARY = 0xD0,
    
    // "simple" object values
    kVALUE_NULL = 0x00,
    kVALUE_FALSE = 0x08,
    kVALUE_TRUE = 0x09,
    kVALUE_FILLER = 0x0F,
    
    kVALUE_FULLDATETAG = 0x33        // Dates are tagged with a whole byte.
};


typedef struct pldata_struct {
    uint8_t *data;
    size_t len;
} pldata_node, *pldata_ptr;


typedef struct array_struct {
    struct value_struct **array;
    uint64_t length;
} array_node, *array_ptr;


// a dict_node is a list of <key, value> pairs
typedef struct dict_struct {
    struct value_struct *key;
    struct value_struct *value;
    struct dict_struct *next;
} dict_node, *dict_ptr;


// an value_node is a value with a tag telling the type
typedef struct value_struct {
    int tag;
    union {
        int64_t integer;
        uint64_t uinteger;
        double real;
        char *string;
        pldata_ptr data;
        array_ptr array;
        struct dict_struct *dict;
    };
} value_node, *value_ptr;


value_ptr bplist_read_file(char *filename);
value_ptr bplist_read_user_pref(char *filename);
value_ptr bplist_read_system_pref(char *filename);
void bplist_free_data();

/*************** functions for accessing values ****************/

char *value_get_asciistring(value_ptr v);
value_ptr value_dict_lookup_using_string(value_ptr v, char *key);
value_ptr value_dict_lookup_using_path(value_ptr v, char *path);

/*************** functions for debugging ***************/

void plist_print(value_ptr v);

