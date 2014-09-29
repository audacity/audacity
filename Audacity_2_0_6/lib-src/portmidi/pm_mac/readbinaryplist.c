/*

readbinaryplist.c -- Roger B. Dannenberg, Jun 2008
Based on ReadBinaryPList.m by Jens Ayton, 2007

Note that this code is intended to read preference files and has an upper
bound on file size (currently 100MB) and assumes in some places that 32 bit
offsets are sufficient.

Here are his comments:

Reader for binary property list files (version 00).

This has been found to work on all 566 binary plists in my ~/Library/Preferences/
and /Library/Preferences/ directories. This probably does not provide full
test coverage. It has also been found to provide different data to Apple's
implementation when presented with a key-value archive. This is because Apple's
implementation produces undocumented CFKeyArchiverUID objects. My implementation
produces dictionaries instead, matching the in-file representation used in XML
and OpenStep plists. See extract_uid().

Full disclosure: in implementing this software, I read one comment and one
struct defintion in CFLite, Apple's implementation, which is under the APSL
license. I also deduced the information about CFKeyArchiverUID from that code.
However, none of the implementation was copied.

Copyright (C) 2007 Jens Ayton

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

*/

/* A note about memory management:
Strings and possibly other values are unique and because the values
associated with IDs are cached, you end up with a directed graph rather
than a tree. It is tricky to free the data because if you do a simple
depth-first search to free nodes, you will free nodes twice. I decided
to allocate memory from blocks of 1024 bytes and keep the blocks in a
list associated with but private to this module. So the user should
access this module by calling:
    bplist_read_file() or bplist_read_user_pref() or 
    bplist_read_system_pref()
which returns a value. When you are done with the value, call
    bplist_free_data()
This will of course free the value_ptr returned by bplist_read_*()

To deal with memory exhaustion (what happens when malloc returns
NULL?), use setjmp/longjmp -- a single setjmp protects the whole
parser, and allocate uses longjmp to abort. After abort, memory
is freed and NULL is returned to caller. There is not much here
in the way of error reporting.

Memory is obtained by calling allocate which either returns the
memory requested or calls longjmp, so callers don't have to check.

*/

#include <sys/types.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <sys/stat.h>
#include "readbinaryplist.h"
#include <Carbon/Carbon.h>

#define NO 0
#define YES 1
#define BOOL int

#define MAXPATHLEN 256

/* there are 2 levels of error logging/printing:
 *   BPLIST_LOG and BPLIST_LOG_VERBOSE
 * either or both can be set to non-zero to turn on
 * If BPLIST_LOG_VERBOSE is true, then BPLIST_LOG 
 * is also true.
 * 
 * In the code, logging is done by calling either
 * bplist_log() or bplist_log_verbose(), which take
 * parameters like printf but might be a no-op.
 */
 
/* #define BPLIST_LOG_VERBOSE 1 */

#if BPLIST_LOG_VERBOSE
    #ifndef BPLIST_LOG
        #define BPLIST_LOG 1
    #endif
#endif

#if BPLIST_LOG
    #define bplist_log printf
#else
    #define bplist_log(...)
#endif

#if BPLIST_LOG_VERBOSE
    #define bplist_log_verbose bplist_log
#else
    #define bplist_log_verbose(...)
#endif


/********* MEMORY MANAGEMENT ********/
#define BLOCK_SIZE 1024
// memory is aligned to multiples of this; assume malloc automatically
// aligns to this number and assume this number is > sizeof(void *)
#define ALIGNMENT 8
static void *block_list = NULL;
static char *free_ptr = NULL;
static char *end_ptr = NULL;
static jmp_buf abort_parsing;

static void *allocate(size_t size)
{
    void *result;
    if (free_ptr + size > end_ptr) {
        size_t how_much = BLOCK_SIZE;
        // align everything to 8 bytes
        if (size > BLOCK_SIZE - ALIGNMENT) {
            how_much = size + ALIGNMENT;
        }
        result = malloc(how_much);
        if (result == NULL) {
            /* serious problem */
            longjmp(abort_parsing, 1);
        }
        *((void **)result) = block_list;
        block_list = result;
        free_ptr = ((char *) result) + ALIGNMENT;
        end_ptr = ((char *) result) + how_much;
    }
    // now, there is enough rooom at free_ptr
    result = free_ptr;
    free_ptr += size;
    return result;
}

void bplist_free_data()
{
    while (block_list) {
        void *next = *(void **)block_list;
        free(block_list);
        block_list = next;
    }
    free_ptr = NULL;
    end_ptr = NULL;
}

// layout of trailer -- last 32 bytes in plist data
    uint8_t unused[6];
    uint8_t offset_int_size;
    uint8_t object_ref_size;
    uint64_t object_count;
    uint64_t top_level_object;
    uint64_t offset_table_offset;


enum
{
    kHEADER_SIZE = 8,
    kTRAILER_SIZE = 32, //sizeof(bplist_trailer_node),
    kMINIMUM_SANE_SIZE = kHEADER_SIZE + kTRAILER_SIZE
};


static const char kHEADER_BYTES[kHEADER_SIZE] = "bplist00";

// map from UID key to previously parsed value
typedef struct cache_struct {
    uint64_t key;
    value_ptr value;
    struct cache_struct *next;
} cache_node, *cache_ptr;


typedef struct bplist_info
{
    uint64_t object_count;
    const uint8_t *data_bytes;
    uint64_t length;
    uint64_t offset_table_offset;
    uint8_t offset_int_size;
    uint8_t object_ref_size;
    cache_ptr cache;
} bplist_info_node, *bplist_info_ptr;


static value_ptr bplist_read_pldata(pldata_ptr data);
static value_ptr bplist_read_pref(char *filename, OSType folder_type);
static uint64_t read_sized_int(bplist_info_ptr bplist, uint64_t offset, uint8_t size);
static uint64_t read_offset(bplist_info_ptr bplist, uint64_t index);
static BOOL read_self_sized_int(bplist_info_ptr bplist, uint64_t offset, uint64_t *outValue, size_t *outSize);

static value_ptr extract_object(bplist_info_ptr bplist, uint64_t objectRef);
static value_ptr extract_simple(bplist_info_ptr bplist, uint64_t offset);
static value_ptr extract_int(bplist_info_ptr bplist, uint64_t offset);
static value_ptr extract_real(bplist_info_ptr bplist, uint64_t offset);
static value_ptr extract_date(bplist_info_ptr bplist, uint64_t offset);
static value_ptr extract_data(bplist_info_ptr bplist, uint64_t offset);
static value_ptr extract_ascii_string(bplist_info_ptr bplist, uint64_t offset);
static value_ptr extract_unicode_string(bplist_info_ptr bplist, uint64_t offset);
static value_ptr extract_uid(bplist_info_ptr bplist, uint64_t offset);
static value_ptr extract_array(bplist_info_ptr bplist, uint64_t offset);
static value_ptr extract_dictionary(bplist_info_ptr bplist, uint64_t offset);


value_ptr value_create()
{
    value_ptr value = (value_ptr) allocate(sizeof(value_node));
    return value;
}


void value_set_integer(value_ptr v, int64_t i) {
    v->tag = kTAG_INT; v->integer = i;
}

void value_set_real(value_ptr v, double d) {
    v->tag = kTAG_REAL; v->real = d;
}

// d is seconds since 1 January 2001
void value_set_date(value_ptr v, double d) {
    v->tag = kTAG_DATE; v->real = d;
}

void value_set_ascii_string(value_ptr v, const uint8_t *s, size_t len) {
    v->tag = kTAG_ASCIISTRING;
    v->string = (char *) allocate(len + 1);
    memcpy(v->string, s, len);
    v->string[len] = 0;
}

void value_set_unicode_string(value_ptr v, const uint8_t *s, size_t len) {
    v->tag = kTAG_UNICODESTRING;
    v->string = (char *) allocate(len + 1);
    memcpy(v->string, s, len);
    v->string[len] = 0;
}

void value_set_uid(value_ptr v, uint64_t uid)
{
    v->tag = kTAG_UID; v->uinteger = uid;
}

// v->data points to a pldata that points to the actual bytes
// the bytes are copied, so caller must free byte source (*data)
void value_set_data(value_ptr v, const uint8_t *data, size_t len) {
    v->tag = kTAG_DATA;
    pldata_ptr pldata = (pldata_ptr) allocate(sizeof(pldata_node));
    pldata->data = (uint8_t *) allocate(len);
    memcpy(pldata->data, data, len);
    pldata->len = len;
    v->data = pldata;
    printf("value at %p gets data at %p\n", v, pldata);
}

// caller releases ownership of array to value_ptr v
void value_set_array(value_ptr v, value_ptr *array, size_t length) {
    array_ptr a = (array_ptr) allocate(sizeof(array_node));
    a->array = array;
    a->length = length;
    v->tag = kTAG_ARRAY;
    v->array = a;
}

// caller releases ownership of dict to value_ptr v
void value_set_dict(value_ptr v, dict_ptr dict) {
    v->tag = kTAG_DICTIONARY;
    v->dict = dict;
}


// look up an objectref in the cache, a ref->value_ptr mapping
value_ptr cache_lookup(cache_ptr cache, uint64_t ref)
{
    while (cache) {
        if (cache->key == ref) {
            return cache->value;
        }
        cache = cache->next;
    }
    return NULL;
}


// insert an objectref and value in the cache
void cache_insert(cache_ptr *cache, uint64_t ref, value_ptr value)
{
    cache_ptr c = (cache_ptr) allocate(sizeof(cache_node));
    c->key = ref;
    c->value = value;
    c->next = *cache;
    *cache = c;
}


// insert an objectref and value in a dictionary
void dict_insert(dict_ptr *dict, value_ptr key, value_ptr value)
{
    dict_ptr d = (dict_ptr) allocate(sizeof(dict_node));
    d->key = key;
    d->value = value;
    d->next = *dict;
    *dict = d;
}


BOOL is_binary_plist(pldata_ptr data)
{
    if (data->len < kMINIMUM_SANE_SIZE)  return NO;
    return memcmp(data->data, kHEADER_BYTES, kHEADER_SIZE) == 0;
}


value_ptr bplist_read_file(char *filename)
{
    struct stat stbuf;
    pldata_node pldata;
    FILE *file;
    size_t n;
    value_ptr value;
    int rslt = stat(filename, &stbuf);
    if (rslt) {
        #if BPLIST_LOG
            perror("in stat");
        #endif
        bplist_log("Could not stat %s, error %d\n", filename, rslt);
        return NULL;
    }
    // if file is >100MB, assume it is not a preferences file and give up
    if (stbuf.st_size > 100000000) {
        bplist_log("Large file %s encountered (%llu bytes) -- not read\n",
                   filename, stbuf.st_size);
        return NULL;
    }
    pldata.len = (size_t) stbuf.st_size;
    // note: this is supposed to be malloc, not allocate. It is separate
    // from the graph structure, large, and easy to free right after
    // parsing.
    pldata.data = (uint8_t *) malloc(pldata.len);
    if (!pldata.data) {
        bplist_log("Could not allocate %lu bytes for %s\n",
                   (unsigned long) pldata.len, filename);
        return NULL;
    }
    file = fopen(filename, "rb");
    if (!file) {
        bplist_log("Could not open %s\n", filename);
        return NULL;
    }
    n = fread(pldata.data, 1, pldata.len, file);
    if (n != pldata.len) {
        bplist_log("Error reading from %s\n", filename);
        return NULL;
    }
    value = bplist_read_pldata(&pldata);
    free(pldata.data);
    return value;
}


value_ptr bplist_read_pref(char *filename, OSType folder_type)
{
    FSRef prefdir;
    char cstr[MAXPATHLEN];

    OSErr err = FSFindFolder(kOnAppropriateDisk, folder_type,
                             FALSE, &prefdir);
    if (err) {
        bplist_log("Error finding preferences folder: %d\n", err);
        return NULL;
    }
    err = FSRefMakePath(&prefdir, (UInt8 *) cstr, (UInt32) (MAXPATHLEN - 1));
    if (err) {
        bplist_log("Error making path name for preferences folder: %d\n", err);
        return NULL;
    }
    strlcat(cstr, "/", MAXPATHLEN);
    strlcat(cstr, filename, MAXPATHLEN);
    return bplist_read_file(cstr);
}


value_ptr bplist_read_system_pref(char *filename) {
    return bplist_read_pref(filename, kSystemPreferencesFolderType);
}


value_ptr bplist_read_user_pref(char *filename) {
    return bplist_read_pref(filename, kPreferencesFolderType);
}


// data is stored with high-order bytes first.
// read from plist data in a machine-independent fashion
//
uint64_t convert_uint64(uint8_t *ptr)
{
    uint64_t rslt = 0;
    int i;
    // shift in bytes, high-order first
    for (i = 0; i < sizeof(uint64_t); i++) {
        rslt <<= 8;
        rslt += ptr[i];
    }
    return rslt;
}


value_ptr bplist_read_pldata(pldata_ptr data)
{
    value_ptr result = NULL;
    bplist_info_node bplist;
    uint8_t *ptr;
    uint64_t top_level_object;
    int i;

    if (data == NULL)  return NULL;
    if (!is_binary_plist(data)) {
        bplist_log("Bad binary plist: too short or invalid header.\n");
        return NULL;
    }
        
    // read trailer
    ptr = (uint8_t *) (data->data + data->len - kTRAILER_SIZE);
    bplist.offset_int_size = ptr[6];
    bplist.object_ref_size = ptr[7];
    bplist.object_count = convert_uint64(ptr + 8);
    top_level_object = convert_uint64(ptr + 16);
    bplist.offset_table_offset = convert_uint64(ptr + 24);
        
    // Basic sanity checks
    if (bplist.offset_int_size < 1 || bplist.offset_int_size > 8 ||
        bplist.object_ref_size < 1 || bplist.object_ref_size > 8 ||
        bplist.offset_table_offset < kHEADER_SIZE) {
        bplist_log("Bad binary plist: trailer declared insane.\n");
        return NULL;                
    }
        
    // Ensure offset table is inside file
    uint64_t offsetTableSize = bplist.offset_int_size * bplist.object_count;
    if (offsetTableSize + bplist.offset_table_offset + kTRAILER_SIZE > 
        data->len) {
        bplist_log("Bad binary plist: offset table overlaps end of container.\n");
        return NULL;
    }
        
    bplist.data_bytes = data->data;
    bplist.length = data->len;
    bplist.cache = NULL; /* dictionary is empty */

    bplist_log_verbose("Got a sane bplist with %llu items, offset_int_size: %u, object_ref_size: %u\n", 
                      bplist.object_count, bplist.offset_int_size, 
                      bplist.object_ref_size);
    /* at this point, we are ready to do some parsing which allocates
        memory for the result data structure. If memory allocation (using
        allocate fails, a longjmp will return to here and we simply give up
     */
    i = setjmp(abort_parsing);
    if (i == 0) {
        result = extract_object(&bplist, top_level_object);
    } else {
        bplist_log("allocate() failed to allocate memory. Giving up.\n");
        result = NULL;
    }
    if (!result) {
        bplist_free_data();
    }
    return result;
}


static value_ptr extract_object(bplist_info_ptr bplist, uint64_t objectRef)
{
    uint64_t offset;
    value_ptr result = NULL;
    uint8_t objectTag;
    
    if (objectRef >= bplist->object_count) {
        // Out-of-range object reference.
        bplist_log("Bad binary plist: object index is out of range.\n");
        return NULL;
    }
        
    // Use cached object if it exists
    result = cache_lookup(bplist->cache, objectRef);
    if (result != NULL)  return result;
        
    // Otherwise, find object in file.
    offset = read_offset(bplist, objectRef);
    if (offset > bplist->length) {
        // Out-of-range offset.
        bplist_log("Bad binary plist: object outside container.\n");
        return NULL;
    }
    objectTag = *(bplist->data_bytes + offset);
    switch (objectTag & 0xF0) {
    case kTAG_SIMPLE:
        result = extract_simple(bplist, offset);
        break;
                
    case kTAG_INT:
        result = extract_int(bplist, offset);
        break;
                        
    case kTAG_REAL:
        result = extract_real(bplist, offset);
        break;
                        
    case kTAG_DATE:
        result = extract_date(bplist, offset);
        break;
                        
    case kTAG_DATA:
        result = extract_data(bplist, offset);
        break;
                        
    case kTAG_ASCIISTRING:
        result = extract_ascii_string(bplist, offset);
        break;
                        
    case kTAG_UNICODESTRING:
        result = extract_unicode_string(bplist, offset);
        break;
        
    case kTAG_UID:
        result = extract_uid(bplist, offset);
        break;
        
    case kTAG_ARRAY:
        result = extract_array(bplist, offset);
        break;
        
    case kTAG_DICTIONARY:
        result = extract_dictionary(bplist, offset);
        break;
        
    default:
        // Unknown tag.
        bplist_log("Bad binary plist: unknown tag 0x%X.\n", 
                   (objectTag & 0x0F) >> 4);
        result = NULL;
    }
    
    // Cache and return result.
    if (result != NULL)  
        cache_insert(&bplist->cache, objectRef, result);
    return result;
}


static uint64_t read_sized_int(bplist_info_ptr bplist, uint64_t offset, 
                               uint8_t size)
{
    assert(bplist->data_bytes != NULL && size >= 1 && size <= 8 && 
           offset + size <= bplist->length);
        
    uint64_t result = 0;
    const uint8_t *byte = bplist->data_bytes + offset;
        
    do {
        // note that ints seem to be high-order first
        result = (result << 8) | *byte++;
    } while (--size);
        
    return result;
}


static uint64_t read_offset(bplist_info_ptr bplist, uint64_t index)
{
    assert(index < bplist->object_count);
        
    return read_sized_int(bplist, 
            bplist->offset_table_offset + bplist->offset_int_size * index, 
            bplist->offset_int_size);
}


static BOOL read_self_sized_int(bplist_info_ptr bplist, uint64_t offset, 
                             uint64_t *outValue, size_t *outSize)
{
    uint32_t size;
    int64_t value;
        
    assert(bplist->data_bytes != NULL && offset < bplist->length);
        
    size = 1 << (bplist->data_bytes[offset] & 0x0F);
    if (size > 8) {
        // Maximum allowable size in this implementation is 1<<3 = 8 bytes.
        // This also happens to be the biggest we can handle.
        return NO;
    }
        
    if (offset + 1 + size > bplist->length) {
        // Out of range.
        return NO;
    }
        
    value = read_sized_int(bplist, offset + 1, size);
    
    if (outValue != NULL) *outValue = value;
    if (outSize != NULL) *outSize = size + 1; // +1 for tag byte.
    return YES;
}


static value_ptr extract_simple(bplist_info_ptr bplist, uint64_t offset)
{
    assert(bplist->data_bytes != NULL && offset < bplist->length);
    value_ptr value = value_create();
        
    switch (bplist->data_bytes[offset]) {
    case kVALUE_NULL:
        value->tag = kVALUE_NULL;
        return value;
        
    case kVALUE_TRUE:
        value->tag = kVALUE_TRUE;
        return value;
                        
    case kVALUE_FALSE:
        value->tag = kVALUE_FALSE;
        return value;
    }
        
    // Note: kVALUE_FILLER is treated as invalid, because it, er, is.
    bplist_log("Bad binary plist: invalid atom.\n");
    free(value);
    return NULL;
}


static value_ptr extract_int(bplist_info_ptr bplist, uint64_t offset)
{
    value_ptr value = value_create();
    value->tag = kTAG_INT;

    if (!read_self_sized_int(bplist, offset, &value->uinteger, NULL)) {
        bplist_log("Bad binary plist: invalid integer object.\n");
    }
        
    /* NOTE: originally, I sign-extended here. This was the wrong thing; it
       turns out that negative ints are always stored as 64-bit, and smaller
       ints are unsigned.
    */
    return value;
}


static value_ptr extract_real(bplist_info_ptr bplist, uint64_t offset)
{
    value_ptr value = value_create();
    uint32_t size;
        
    assert(bplist->data_bytes != NULL && offset < bplist->length);
    
    size = 1 << (bplist->data_bytes[offset] & 0x0F);
        
    // FIXME: what to do if faced with other sizes for float/double?
    assert (sizeof (float) == sizeof (uint32_t) && 
            sizeof (double) == sizeof (uint64_t));
        
    if (offset + 1 + size > bplist->length) {
        bplist_log("Bad binary plist: %s object overlaps end of container.\n", 
                  "floating-point number");
        free(value);
        return NULL;
    }
        
    if (size == sizeof (float)) {
        // cast is ok because we know size is 4 bytes
        uint32_t i = (uint32_t) read_sized_int(bplist, offset + 1, size); 
        // Note that this handles byte swapping.
        value_set_real(value, *(float *)&i);
        return value;
    } else if (size == sizeof (double)) {
        uint64_t i = read_sized_int(bplist, offset + 1, size);
        // Note that this handles byte swapping.
        value_set_real(value, *(double *)&i);
        return value;
    } else {
        // Can't handle floats of other sizes.
        bplist_log("Bad binary plist: can't handle %u-byte float.\n", size);
        free(value);
        return NULL;
    }
}


static value_ptr extract_date(bplist_info_ptr bplist, uint64_t offset)
{
    value_ptr value;
    assert(bplist->data_bytes != NULL && offset < bplist->length);
        
    // Data has size code like int and real, but only 3 (meaning 8 bytes) is valid.
    if (bplist->data_bytes[offset] != kVALUE_FULLDATETAG) {
        bplist_log("Bad binary plist: invalid size for date object.\n");
        return NULL;
    }
        
    if (offset + 1 + sizeof (double) > bplist->length) {
        bplist_log("Bad binary plist: %s object overlaps end of container.\n", 
                  "date");
        return NULL;
    }
        
    // FIXME: what to do if faced with other sizes for double?
    assert (sizeof (double) == sizeof (uint64_t));
        
    uint64_t date = read_sized_int(bplist, offset + 1, sizeof(double));
    // Note that this handles byte swapping.
    value = value_create();
    value_set_date(value, *(double *)&date);
    return value;
}


uint64_t bplist_get_a_size(bplist_info_ptr bplist, 
                           uint64_t *offset_ptr, char *msg)
{
    uint64_t size = bplist->data_bytes[*offset_ptr] & 0x0F;
    (*offset_ptr)++;
    if (size == 0x0F) {
        // 0x0F means separate int size follows. 
        // Smaller values are used for short data.
        size_t extra; // the length of the data size we are about to read
        if ((bplist->data_bytes[*offset_ptr] & 0xF0) != kTAG_INT) {
            // Bad data, mistagged size int
            bplist_log("Bad binary plist: %s object size is not tagged as int.\n",
                       msg);
            return UINT64_MAX; // error
        }
                
        // read integer data as size, extra tells how many bytes to skip
        if (!read_self_sized_int(bplist, *offset_ptr, &size, &extra)) {
            bplist_log("Bad binary plist: invalid %s object size tag.\n", 
                      "data");
            return UINT64_MAX; // error
        }
        (*offset_ptr) += extra;
    }

    if (*offset_ptr + size > bplist->length) {
        bplist_log("Bad binary plist: %s object overlaps end of container.\n", 
                  "data");
        return UINT64_MAX; // error
    }
    return size;
}


static value_ptr extract_data(bplist_info_ptr bplist, uint64_t offset)
{
    uint64_t size;
    value_ptr value;
        
    assert(bplist->data_bytes != NULL && offset < bplist->length);
        
    if ((size = bplist_get_a_size(bplist, &offset, "data")) == UINT64_MAX) 
        return NULL;
        
    value = value_create();
    // cast is ok because we only allow files up to 100MB:
    value_set_data(value, bplist->data_bytes + (size_t) offset, (size_t) size);
    return value;
}


static value_ptr extract_ascii_string(bplist_info_ptr bplist, uint64_t offset)
{
    uint64_t size;
    value_ptr value; // return value
        
    assert(bplist->data_bytes != NULL && offset < bplist->length);
        
    if ((size = bplist_get_a_size(bplist, &offset, "ascii string")) ==
        UINT64_MAX) 
        return NULL;

    value = value_create();
    // cast is ok because we only allow 100MB files
    value_set_ascii_string(value, bplist->data_bytes + (size_t) offset, 
                           (size_t) size);
    return value;
}


static value_ptr extract_unicode_string(bplist_info_ptr bplist, uint64_t offset)
{
    uint64_t size;
    value_ptr value;
        
    assert(bplist->data_bytes != NULL && offset < bplist->length);
        
    if ((size = bplist_get_a_size(bplist, &offset, "unicode string")) == 
        UINT64_MAX)
        return NULL;
        
    value = value_create();
    // cast is ok because we only allow 100MB files
    value_set_unicode_string(value, bplist->data_bytes + (size_t) offset, 
                             (size_t) size);
    return value;
}


static value_ptr extract_uid(bplist_info_ptr bplist, uint64_t offset)
{
    /* UIDs are used by Cocoa's key-value coder.
       When writing other plist formats, they are expanded to dictionaries of
       the form <dict><key>CF$UID</key><integer>value</integer></dict>, so we
       do the same here on reading. This results in plists identical to what
       running plutil -convert xml1 gives us. However, this is not the same
       result as [Core]Foundation's plist parser, which extracts them as un-
       introspectable CF objects. In fact, it even seems to convert the CF$UID
       dictionaries from XML plists on the fly.
    */
        
    value_ptr value;
    uint64_t uid;
        
    if (!read_self_sized_int(bplist, offset, &uid, NULL)) {
        bplist_log("Bad binary plist: invalid UID object.\n");
        return NULL;
    }
        
    // assert(NO); // original code suggests using a string for a key
    // but our dictionaries all use big ints for keys, so I don't know
    // what to do here
    
    // In practice, I believe this code is never executed by PortMidi.
    // I changed it to do something and not raise compiler warnings, but
    // not sure what the code should do.

    value = value_create();
    value_set_uid(value, uid);
    // return [NSDictionary dictionaryWithObject:
    //         [NSNumber numberWithUnsignedLongLong:value] 
    //         forKey:"CF$UID"];
    return value;
}


static value_ptr extract_array(bplist_info_ptr bplist, uint64_t offset)
{
    uint64_t i, count;
    uint64_t size;
    uint64_t elementID;
    value_ptr element = NULL;
    value_ptr *array = NULL;
    value_ptr value = NULL;
    BOOL ok = YES;
        
    assert(bplist->data_bytes != NULL && offset < bplist->length);
        
    if ((count = bplist_get_a_size(bplist, &offset, "array")) == UINT64_MAX)
        return NULL;
        
    if (count > UINT64_MAX / bplist->object_ref_size - offset) {
        // Offset overflow.
        bplist_log("Bad binary plist: %s object overlaps end of container.\n", 
                   "array");
        return NULL;
    }
        
    size = bplist->object_ref_size * count;
    if (size + offset > bplist->length) {
        bplist_log("Bad binary plist: %s object overlaps end of container.\n", 
                   "array");
        return NULL;
    }
        
    // got count, the number of array elements

    value = value_create();
    assert(value);

    if (count == 0) {
        // count must be size_t or smaller because max file size is 100MB
        value_set_array(value, array, (size_t) count);
        return value;
    }
        
    array = allocate(sizeof(value_ptr) * (size_t) count);
        
    for (i = 0; i != count; ++i) {
        bplist_log_verbose("[%u]\n", i);
        elementID = read_sized_int(bplist, offset + i * bplist->object_ref_size, 
                                 bplist->object_ref_size);
        element = extract_object(bplist, elementID);
        if (element != NULL) {
            array[i] = element;
        } else {
            ok = NO;
            break;
        }
    }
    if (ok) { // count is smaller than size_t max because of 100MB file limit
        value_set_array(value, array, (size_t) count);
    }

    return value;
}


static value_ptr extract_dictionary(bplist_info_ptr bplist, uint64_t offset)
{
    uint64_t i, count;
    uint64_t size;
    uint64_t elementID;
    value_ptr value = NULL;
    dict_ptr dict = NULL;
    BOOL ok = YES;
        
    assert(bplist->data_bytes != NULL && offset < bplist->length);
        
        
    if ((count = bplist_get_a_size(bplist, &offset, "array")) == UINT64_MAX)
        return NULL;

    if (count > UINT64_MAX / (bplist->object_ref_size * 2) - offset) {
        // Offset overflow.
        bplist_log("Bad binary plist: %s object overlaps end of container.\n", 
                   "dictionary");
        return NULL;
    }
    
    size = bplist->object_ref_size * count * 2;
    if (size + offset > bplist->length) {
        bplist_log("Bad binary plist: %s object overlaps end of container.\n", 
                   "dictionary");
        return NULL;
    }
    
    value = value_create();
    if (count == 0) {
        value_set_dict(value, NULL);
        return value;
    }

    for (i = 0; i != count; ++i) {
        value_ptr key;
        value_ptr val;
        elementID = read_sized_int(bplist, offset + i * bplist->object_ref_size, 
                                 bplist->object_ref_size);
        key = extract_object(bplist, elementID);
        if (key != NULL) {
            bplist_log_verbose("key: %p\n", key);
        } else {
            ok = NO;
            break;
        }
                    
        elementID = read_sized_int(bplist, 
                            offset + (i + count) * bplist->object_ref_size, 
                            bplist->object_ref_size);
        val = extract_object(bplist, elementID);
        if (val != NULL) {
            dict_insert(&dict, key, val);
        } else {
            ok = NO;
            break;
        }
    }
    if (ok) {
        value_set_dict(value, dict);
    }
    
    return value;
}

/*************** functions for accessing values ****************/


char *value_get_asciistring(value_ptr v)
{
    if (v->tag != kTAG_ASCIISTRING) return NULL;
    return v->string;
}


value_ptr value_dict_lookup_using_string(value_ptr v, char *key)
{
    dict_ptr dict;
    if (v->tag != kTAG_DICTIONARY) return NULL; // not a dictionary
    dict = v->dict;
    /* search for key */
    while (dict) {
        if (dict->key && dict->key->tag == kTAG_ASCIISTRING &&
            strcmp(key, dict->key->string) == 0) { // found it
            return dict->value;
        }
        dict = dict->next;
    }
    return NULL; /* not found */
}

value_ptr value_dict_lookup_using_path(value_ptr v, char *path)
{
    char key[MAX_KEY_SIZE];
    while (*path) { /* more to the path */
        int i = 0;
        while (i < MAX_KEY_SIZE - 1) {
            key[i] = *path++;
            if (key[i] == '/') { /* end of entry in path */
                key[i + 1] = 0;
                break;
            }
            if (!key[i]) {
                path--; /* back up to end of string char */
                break;  /* this will cause outer loop to exit */
            }
            i++;
        }
        if (!v || v->tag != kTAG_DICTIONARY) return NULL;
        /* now, look up the key to get next value */
        v = value_dict_lookup_using_string(v, key);
        if (v == NULL) return NULL;
    }
    return v;
}
                

/*************** functions for debugging ***************/

void plist_print(value_ptr v)
{
    size_t i;
    int comma_needed;
    dict_ptr dict;
    if (!v) {
        printf("NULL");
        return;
    }
    switch (v->tag & 0xF0) {
    case kTAG_SIMPLE:
        switch (v->tag) {
        case kVALUE_NULL: 
            printf("NULL@%p", v); break;
        case kVALUE_FALSE: 
            printf("FALSE@%p", v); break;
        case kVALUE_TRUE:
            printf("TRUE@%p", v); break;
        default:
            printf("UNKNOWN tag=%x@%p", v->tag, v); break;
        }
        break;
    case kTAG_INT:
        printf("%lld@%p", v->integer, v); break;
    case kTAG_REAL:
        printf("%g@%p", v->real, v); break;
    case kTAG_DATE:
        printf("date:%g@%p", v->real, v); break;
    case kTAG_DATA:
        printf("data@%p->%p:[%p:", v, v->data, v->data->data);
        for (i = 0; i < v->data->len; i++) {
            printf(" %2x", v->data->data[i]);
        }
        printf("]"); break;
    case kTAG_ASCIISTRING:
        printf("%p:\"%s\"@%p", v->string, v->string, v); break;
    case kTAG_UNICODESTRING:
        printf("unicode:%p:\"%s\"@%p", v->string, v->string, v); break;
    case kTAG_UID:
        printf("UID:%llu@%p", v->uinteger, v); break;
    case kTAG_ARRAY:
        comma_needed = FALSE;
        printf("%p->%p:[%p:", v, v->array, v->array->array);
        for (i = 0; i < v->array->length; i++) {
            if (comma_needed) printf(", ");
            plist_print(v->array->array[i]);
            comma_needed = TRUE;
        }
        printf("]"); break;
    case kTAG_DICTIONARY:
        comma_needed = FALSE;
        printf("%p:[", v);
        dict = v->dict;
        while (dict) {
            if (comma_needed) printf(", ");
            printf("%p:", dict);
            plist_print(dict->key);
            printf("->");
            plist_print(dict->value);
            comma_needed = TRUE;
            dict = dict->next;
        }
        printf("]"); break;
    default:
        printf("UNKNOWN tag=%x", v->tag);
        break;
    }
}

            
