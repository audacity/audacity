/* Copyright (c) 1998, 1999, 2000 Thai Open Source Software Center Ltd
   See the file COPYING for copying permission.

   runtest.c : run the Expat test suite
*/

#ifdef HAVE_EXPAT_CONFIG_H
#include <expat_config.h>
#endif

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stddef.h>  /* ptrdiff_t */
#include <ctype.h>
#ifndef __cplusplus
# include <stdbool.h>
#endif
#include <limits.h>

#include "expat.h"
#include "chardata.h"
#include "internal.h"  /* for UNUSED_P only */
#include "minicheck.h"
#include "memcheck.h"
#include "siphash.h"

#ifdef XML_LARGE_SIZE
#define XML_FMT_INT_MOD "ll"
#else
#define XML_FMT_INT_MOD "l"
#endif

static XML_Parser parser = NULL;


static void
basic_setup(void)
{
    parser = XML_ParserCreate(NULL);
    if (parser == NULL)
        fail("Parser not created.");
}

static void
basic_teardown(void)
{
    if (parser != NULL) {
        XML_ParserFree(parser);
        parser = NULL;
    }
}

/* Generate a failure using the parser state to create an error message;
   this should be used when the parser reports an error we weren't
   expecting.
*/
static void
_xml_failure(XML_Parser parser, const char *file, int line)
{
    char buffer[1024];
    enum XML_Error err = XML_GetErrorCode(parser);
    sprintf(buffer,
            "    %d: %s (line %" XML_FMT_INT_MOD "u, offset %"\
                XML_FMT_INT_MOD "u)\n    reported from %s, line %d\n",
            err,
            XML_ErrorString(err),
            XML_GetCurrentLineNumber(parser),
            XML_GetCurrentColumnNumber(parser),
            file, line);
    _fail_unless(0, file, line, buffer);
}

static enum XML_Status
_XML_Parse_SINGLE_BYTES(XML_Parser parser, const char *s, int len, int isFinal)
{
    enum XML_Status res = XML_STATUS_ERROR;
    int offset = 0;

    if (len == 0) {
        return XML_Parse(parser, s, len, isFinal);
    }

    for (; offset < len; offset++) {
        const int innerIsFinal = (offset == len - 1) && isFinal;
        const char c = s[offset]; /* to help out-of-bounds detection */
        res = XML_Parse(parser, &c, sizeof(char), innerIsFinal);
        if (res != XML_STATUS_OK) {
            return res;
        }
    }
    return res;
}

#define xml_failure(parser) _xml_failure((parser), __FILE__, __LINE__)

static void
_expect_failure(const char *text, enum XML_Error errorCode, const char *errorMessage,
                const char *file, int lineno)
{
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) == XML_STATUS_OK)
        /* Hackish use of _fail_unless() macro, but let's us report
           the right filename and line number. */
        _fail_unless(0, file, lineno, errorMessage);
    if (XML_GetErrorCode(parser) != errorCode)
        _xml_failure(parser, file, lineno);
}

#define expect_failure(text, errorCode, errorMessage) \
        _expect_failure((text), (errorCode), (errorMessage), \
                        __FILE__, __LINE__)

/* Dummy handlers for when we need to set a handler to tickle a bug,
   but it doesn't need to do anything.
*/

static void XMLCALL
dummy_start_doctype_handler(void           *UNUSED_P(userData),
                            const XML_Char *UNUSED_P(doctypeName),
                            const XML_Char *UNUSED_P(sysid),
                            const XML_Char *UNUSED_P(pubid),
                            int            UNUSED_P(has_internal_subset))
{}

static void XMLCALL
dummy_end_doctype_handler(void *UNUSED_P(userData))
{}

static void XMLCALL
dummy_entity_decl_handler(void           *UNUSED_P(userData),
                          const XML_Char *UNUSED_P(entityName),
                          int            UNUSED_P(is_parameter_entity),
                          const XML_Char *UNUSED_P(value),
                          int            UNUSED_P(value_length),
                          const XML_Char *UNUSED_P(base),
                          const XML_Char *UNUSED_P(systemId),
                          const XML_Char *UNUSED_P(publicId),
                          const XML_Char *UNUSED_P(notationName))
{}

static void XMLCALL
dummy_notation_decl_handler(void *UNUSED_P(userData),
                            const XML_Char *UNUSED_P(notationName),
                            const XML_Char *UNUSED_P(base),
                            const XML_Char *UNUSED_P(systemId),
                            const XML_Char *UNUSED_P(publicId))
{}

static void XMLCALL
dummy_element_decl_handler(void *UNUSED_P(userData),
                           const XML_Char *UNUSED_P(name),
                           XML_Content *UNUSED_P(model))
{}

static void XMLCALL
dummy_attlist_decl_handler(void           *UNUSED_P(userData),
                           const XML_Char *UNUSED_P(elname),
                           const XML_Char *UNUSED_P(attname),
                           const XML_Char *UNUSED_P(att_type),
                           const XML_Char *UNUSED_P(dflt),
                           int            UNUSED_P(isrequired))
{}

static void XMLCALL
dummy_comment_handler(void *UNUSED_P(userData), const XML_Char *UNUSED_P(data))
{}

static void XMLCALL
dummy_pi_handler(void *UNUSED_P(userData), const XML_Char *UNUSED_P(target), const XML_Char *UNUSED_P(data))
{}

static void XMLCALL
dummy_start_element(void *UNUSED_P(userData),
                    const XML_Char *UNUSED_P(name), const XML_Char **UNUSED_P(atts))
{}

static void XMLCALL
dummy_start_cdata_handler(void *UNUSED_P(userData))
{}

static void XMLCALL
dummy_end_cdata_handler(void *UNUSED_P(userData))
{}

static void XMLCALL
dummy_start_namespace_decl_handler(void *UNUSED_P(userData),
                                   const XML_Char *UNUSED_P(prefix),
                                   const XML_Char *UNUSED_P(uri))
{}

static void XMLCALL
dummy_end_namespace_decl_handler(void *UNUSED_P(userData),
                                 const XML_Char *UNUSED_P(prefix))
{}

/* This handler is obsolete, but while the code exists we should
 * ensure that dealing with the handler is covered by tests.
 */
static void XMLCALL
dummy_unparsed_entity_decl_handler(void *UNUSED_P(userData),
                                   const XML_Char *UNUSED_P(entityName),
                                   const XML_Char *UNUSED_P(base),
                                   const XML_Char *UNUSED_P(systemId),
                                   const XML_Char *UNUSED_P(publicId),
                                   const XML_Char *UNUSED_P(notationName))
{}


/*
 * Character & encoding tests.
 */

START_TEST(test_nul_byte)
{
    char text[] = "<doc>\0</doc>";

    /* test that a NUL byte (in US-ASCII data) is an error */
    if (_XML_Parse_SINGLE_BYTES(parser, text, sizeof(text) - 1, XML_TRUE) == XML_STATUS_OK)
        fail("Parser did not report error on NUL-byte.");
    if (XML_GetErrorCode(parser) != XML_ERROR_INVALID_TOKEN)
        xml_failure(parser);
}
END_TEST


START_TEST(test_u0000_char)
{
    /* test that a NUL byte (in US-ASCII data) is an error */
    expect_failure("<doc>&#0;</doc>",
                   XML_ERROR_BAD_CHAR_REF,
                   "Parser did not report error on NUL-byte.");
}
END_TEST

START_TEST(test_siphash_self)
{
    if (! sip24_valid())
        fail("SipHash self-test failed");
}
END_TEST

START_TEST(test_siphash_spec)
{
    /* https://131002.net/siphash/siphash.pdf (page 19, "Test values") */
    const char message[] = "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09"
            "\x0a\x0b\x0c\x0d\x0e";
    const size_t len = sizeof(message) - 1;
    const uint64_t expected = 0xa129ca6149be45e5U;
    struct siphash state;
    struct sipkey key;
    (void)sip_tobin;

    sip_tokey(&key,
            "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09"
            "\x0a\x0b\x0c\x0d\x0e\x0f");
    sip24_init(&state, &key);

    /* Cover spread across calls */
    sip24_update(&state, message, 4);
    sip24_update(&state, message + 4, len - 4);

    /* Cover null length */
    sip24_update(&state, message, 0);

    if (sip24_final(&state) != expected)
        fail("sip24_final failed spec test\n");

    /* Cover wrapper */
    if (siphash24(message, len, &key) != expected)
        fail("siphash24 failed spec test\n");
}
END_TEST

START_TEST(test_bom_utf8)
{
    /* This test is really just making sure we don't core on a UTF-8 BOM. */
    const char *text = "\357\273\277<e/>";

    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
}
END_TEST

START_TEST(test_bom_utf16_be)
{
    char text[] = "\376\377\0<\0e\0/\0>";

    if (_XML_Parse_SINGLE_BYTES(parser, text, sizeof(text)-1, XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
}
END_TEST

START_TEST(test_bom_utf16_le)
{
    char text[] = "\377\376<\0e\0/\0>\0";

    if (_XML_Parse_SINGLE_BYTES(parser, text, sizeof(text)-1, XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
}
END_TEST

static void XMLCALL
accumulate_characters(void *userData, const XML_Char *s, int len)
{
    CharData_AppendXMLChars((CharData *)userData, s, len);
}

static void XMLCALL
accumulate_attribute(void *userData, const XML_Char *UNUSED_P(name),
                     const XML_Char **atts)
{
    CharData *storage = (CharData *)userData;
    if (storage->count < 0 && atts != NULL && atts[0] != NULL) {
        /* "accumulate" the value of the first attribute we see */
        CharData_AppendXMLChars(storage, atts[1], -1);
    }
}


static void
_run_character_check(const XML_Char *text, const XML_Char *expected,
                     const char *file, int line)
{
    CharData storage;

    CharData_Init(&storage);
    XML_SetUserData(parser, &storage);
    XML_SetCharacterDataHandler(parser, accumulate_characters);
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) == XML_STATUS_ERROR)
        _xml_failure(parser, file, line);
    CharData_CheckXMLChars(&storage, expected);
}

#define run_character_check(text, expected) \
        _run_character_check(text, expected, __FILE__, __LINE__)

static void
_run_attribute_check(const XML_Char *text, const XML_Char *expected,
                     const char *file, int line)
{
    CharData storage;

    CharData_Init(&storage);
    XML_SetUserData(parser, &storage);
    XML_SetStartElementHandler(parser, accumulate_attribute);
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) == XML_STATUS_ERROR)
        _xml_failure(parser, file, line);
    CharData_CheckXMLChars(&storage, expected);
}

#define run_attribute_check(text, expected) \
        _run_attribute_check(text, expected, __FILE__, __LINE__)

/* Regression test for SF bug #491986. */
START_TEST(test_danish_latin1)
{
    const char *text =
        "<?xml version='1.0' encoding='iso-8859-1'?>\n"
        "<e>J\xF8rgen \xE6\xF8\xE5\xC6\xD8\xC5</e>";
    run_character_check(text,
             "J\xC3\xB8rgen \xC3\xA6\xC3\xB8\xC3\xA5\xC3\x86\xC3\x98\xC3\x85");
}
END_TEST


/* Regression test for SF bug #514281. */
START_TEST(test_french_charref_hexidecimal)
{
    const char *text =
        "<?xml version='1.0' encoding='iso-8859-1'?>\n"
        "<doc>&#xE9;&#xE8;&#xE0;&#xE7;&#xEA;&#xC8;</doc>";
    run_character_check(text,
                        "\xC3\xA9\xC3\xA8\xC3\xA0\xC3\xA7\xC3\xAA\xC3\x88");
}
END_TEST

START_TEST(test_french_charref_decimal)
{
    const char *text =
        "<?xml version='1.0' encoding='iso-8859-1'?>\n"
        "<doc>&#233;&#232;&#224;&#231;&#234;&#200;</doc>";
    run_character_check(text,
                        "\xC3\xA9\xC3\xA8\xC3\xA0\xC3\xA7\xC3\xAA\xC3\x88");
}
END_TEST

START_TEST(test_french_latin1)
{
    const char *text =
        "<?xml version='1.0' encoding='iso-8859-1'?>\n"
        "<doc>\xE9\xE8\xE0\xE7\xEa\xC8</doc>";
    run_character_check(text,
                        "\xC3\xA9\xC3\xA8\xC3\xA0\xC3\xA7\xC3\xAA\xC3\x88");
}
END_TEST

START_TEST(test_french_utf8)
{
    const char *text =
        "<?xml version='1.0' encoding='utf-8'?>\n"
        "<doc>\xC3\xA9</doc>";
    run_character_check(text, "\xC3\xA9");
}
END_TEST

/* Regression test for SF bug #600479.
   XXX There should be a test that exercises all legal XML Unicode
   characters as PCDATA and attribute value content, and XML Name
   characters as part of element and attribute names.
*/
START_TEST(test_utf8_false_rejection)
{
    const char *text = "<doc>\xEF\xBA\xBF</doc>";
    run_character_check(text, "\xEF\xBA\xBF");
}
END_TEST

/* Regression test for SF bug #477667.
   This test assures that any 8-bit character followed by a 7-bit
   character will not be mistakenly interpreted as a valid UTF-8
   sequence.
*/
START_TEST(test_illegal_utf8)
{
    char text[100];
    int i;

    for (i = 128; i <= 255; ++i) {
        sprintf(text, "<e>%ccd</e>", i);
        if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) == XML_STATUS_OK) {
            sprintf(text,
                    "expected token error for '%c' (ordinal %d) in UTF-8 text",
                    i, i);
            fail(text);
        }
        else if (XML_GetErrorCode(parser) != XML_ERROR_INVALID_TOKEN)
            xml_failure(parser);
        /* Reset the parser since we use the same parser repeatedly. */
        XML_ParserReset(parser, NULL);
    }
}
END_TEST


/* Examples, not masks: */
#define UTF8_LEAD_1  "\x7f"  /* 0b01111111 */
#define UTF8_LEAD_2  "\xdf"  /* 0b11011111 */
#define UTF8_LEAD_3  "\xef"  /* 0b11101111 */
#define UTF8_LEAD_4  "\xf7"  /* 0b11110111 */
#define UTF8_FOLLOW  "\xbf"  /* 0b10111111 */

START_TEST(test_utf8_auto_align)
{
    struct TestCase {
        ptrdiff_t expectedMovementInChars;
        const char * input;
    };

    struct TestCase cases[] = {
        {00, ""},

        {00, UTF8_LEAD_1},

        {-1, UTF8_LEAD_2},
        {00, UTF8_LEAD_2 UTF8_FOLLOW},

        {-1, UTF8_LEAD_3},
        {-2, UTF8_LEAD_3 UTF8_FOLLOW},
        {00, UTF8_LEAD_3 UTF8_FOLLOW UTF8_FOLLOW},

        {-1, UTF8_LEAD_4},
        {-2, UTF8_LEAD_4 UTF8_FOLLOW},
        {-3, UTF8_LEAD_4 UTF8_FOLLOW UTF8_FOLLOW},
        {00, UTF8_LEAD_4 UTF8_FOLLOW UTF8_FOLLOW UTF8_FOLLOW},
    };

    size_t i = 0;
    bool success = true;
    for (; i < sizeof(cases) / sizeof(*cases); i++) {
        const char * fromLim = cases[i].input + strlen(cases[i].input);
        const char * const fromLimInitially = fromLim;
        ptrdiff_t actualMovementInChars;

        align_limit_to_full_utf8_characters(cases[i].input, &fromLim);

        actualMovementInChars = (fromLim - fromLimInitially);
        if (actualMovementInChars != cases[i].expectedMovementInChars) {
            size_t j = 0;
            success = false;
            printf("[-] UTF-8 case %2lu: Expected movement by %2ld chars"
                    ", actually moved by %2ld chars: \"",
                    i + 1, cases[i].expectedMovementInChars, actualMovementInChars);
            for (; j < strlen(cases[i].input); j++) {
                printf("\\x%02x", (unsigned char)cases[i].input[j]);
            }
            printf("\"\n");
        }
    }

    if (! success) {
        fail("UTF-8 auto-alignment is not bullet-proof\n");
    }
}
END_TEST

START_TEST(test_utf16)
{
    /* <?xml version="1.0" encoding="UTF-16"?>
       <doc a='123'>some text</doc>
    */
    char text[] =
        "\000<\000?\000x\000m\000\154\000 \000v\000e\000r\000s\000i\000o"
        "\000n\000=\000'\0001\000.\000\060\000'\000 \000e\000n\000c\000o"
        "\000d\000i\000n\000g\000=\000'\000U\000T\000F\000-\0001\000\066"
        "\000'\000?\000>\000\n"
        "\000<\000d\000o\000c\000 \000a\000=\000'\0001\0002\0003\000'"
        "\000>\000s\000o\000m\000e\000 \000t\000e\000x\000t\000<\000/"
        "\000d\000o\000c\000>";
    if (_XML_Parse_SINGLE_BYTES(parser, text, sizeof(text)-1, XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
}
END_TEST

START_TEST(test_utf16_le_epilog_newline)
{
    unsigned int first_chunk_bytes = 17;
    char text[] = 
        "\xFF\xFE"                      /* BOM */
        "<\000e\000/\000>\000"          /* document element */
        "\r\000\n\000\r\000\n\000";     /* epilog */

    if (first_chunk_bytes >= sizeof(text) - 1)
        fail("bad value of first_chunk_bytes");
    if (  _XML_Parse_SINGLE_BYTES(parser, text, first_chunk_bytes, XML_FALSE)
          == XML_STATUS_ERROR)
        xml_failure(parser);
    else {
        enum XML_Status rc;
        rc = _XML_Parse_SINGLE_BYTES(parser, text + first_chunk_bytes,
                       sizeof(text) - first_chunk_bytes - 1, XML_TRUE);
        if (rc == XML_STATUS_ERROR)
            xml_failure(parser);
    }
}
END_TEST

/* Regression test for SF bug #481609, #774028. */
START_TEST(test_latin1_umlauts)
{
    const char *text =
        "<?xml version='1.0' encoding='iso-8859-1'?>\n"
        "<e a='\xE4 \xF6 \xFC &#228; &#246; &#252; &#x00E4; &#x0F6; &#xFC; >'\n"
        "  >\xE4 \xF6 \xFC &#228; &#246; &#252; &#x00E4; &#x0F6; &#xFC; ></e>";
    const char *utf8 =
        "\xC3\xA4 \xC3\xB6 \xC3\xBC "
        "\xC3\xA4 \xC3\xB6 \xC3\xBC "
        "\xC3\xA4 \xC3\xB6 \xC3\xBC >";
    run_character_check(text, utf8);
    XML_ParserReset(parser, NULL);
    run_attribute_check(text, utf8);
}
END_TEST

/* Regression test #1 for SF bug #653180. */
START_TEST(test_line_number_after_parse)
{  
    const char *text =
        "<tag>\n"
        "\n"
        "\n</tag>";
    XML_Size lineno;

    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_FALSE) == XML_STATUS_ERROR)
        xml_failure(parser);
    lineno = XML_GetCurrentLineNumber(parser);
    if (lineno != 4) {
        char buffer[100];
        sprintf(buffer, 
            "expected 4 lines, saw %" XML_FMT_INT_MOD "u", lineno);
        fail(buffer);
    }
}
END_TEST

/* Regression test #2 for SF bug #653180. */
START_TEST(test_column_number_after_parse)
{
    const char *text = "<tag></tag>";
    XML_Size colno;

    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_FALSE) == XML_STATUS_ERROR)
        xml_failure(parser);
    colno = XML_GetCurrentColumnNumber(parser);
    if (colno != 11) {
        char buffer[100];
        sprintf(buffer, 
            "expected 11 columns, saw %" XML_FMT_INT_MOD "u", colno);
        fail(buffer);
    }
}
END_TEST

static void XMLCALL
start_element_event_handler2(void *userData, const XML_Char *name,
			     const XML_Char **UNUSED_P(attr))
{
    CharData *storage = (CharData *) userData;
    char buffer[100];

    sprintf(buffer,
        "<%s> at col:%" XML_FMT_INT_MOD "u line:%"\
            XML_FMT_INT_MOD "u\n", name,
	    XML_GetCurrentColumnNumber(parser),
	    XML_GetCurrentLineNumber(parser));
    CharData_AppendString(storage, buffer);
}

static void XMLCALL
end_element_event_handler2(void *userData, const XML_Char *name)
{
    CharData *storage = (CharData *) userData;
    char buffer[100];

    sprintf(buffer,
        "</%s> at col:%" XML_FMT_INT_MOD "u line:%"\
            XML_FMT_INT_MOD "u\n", name,
	    XML_GetCurrentColumnNumber(parser),
	    XML_GetCurrentLineNumber(parser));
    CharData_AppendString(storage, buffer);
}

/* Regression test #3 for SF bug #653180. */
START_TEST(test_line_and_column_numbers_inside_handlers)
{
    const char *text =
        "<a>\n"        /* Unix end-of-line */
        "  <b>\r\n"    /* Windows end-of-line */
        "    <c/>\r"   /* Mac OS end-of-line */
        "  </b>\n"
        "  <d>\n"
        "    <f/>\n"
        "  </d>\n"
        "</a>";
    const char *expected =
        "<a> at col:0 line:1\n"
        "<b> at col:2 line:2\n"
        "<c> at col:4 line:3\n"
        "</c> at col:8 line:3\n"
        "</b> at col:2 line:4\n"
        "<d> at col:2 line:5\n"
        "<f> at col:4 line:6\n"
        "</f> at col:8 line:6\n"
        "</d> at col:2 line:7\n"
        "</a> at col:0 line:8\n";
    CharData storage;

    CharData_Init(&storage);
    XML_SetUserData(parser, &storage);
    XML_SetStartElementHandler(parser, start_element_event_handler2);
    XML_SetEndElementHandler(parser, end_element_event_handler2);
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);

    CharData_CheckString(&storage, expected); 
}
END_TEST

/* Regression test #4 for SF bug #653180. */
START_TEST(test_line_number_after_error)
{
    const char *text =
        "<a>\n"
        "  <b>\n"
        "  </a>";  /* missing </b> */
    XML_Size lineno;
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_FALSE) != XML_STATUS_ERROR)
        fail("Expected a parse error");

    lineno = XML_GetCurrentLineNumber(parser);
    if (lineno != 3) {
        char buffer[100];
        sprintf(buffer, "expected 3 lines, saw %" XML_FMT_INT_MOD "u", lineno);
        fail(buffer);
    }
}
END_TEST
    
/* Regression test #5 for SF bug #653180. */
START_TEST(test_column_number_after_error)
{
    const char *text =
        "<a>\n"
        "  <b>\n"
        "  </a>";  /* missing </b> */
    XML_Size colno;
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_FALSE) != XML_STATUS_ERROR)
        fail("Expected a parse error");

    colno = XML_GetCurrentColumnNumber(parser);
    if (colno != 4) { 
        char buffer[100];
        sprintf(buffer, 
            "expected 4 columns, saw %" XML_FMT_INT_MOD "u", colno);
        fail(buffer);
    }
}
END_TEST

/* Regression test for SF bug #478332. */
START_TEST(test_really_long_lines)
{
    /* This parses an input line longer than INIT_DATA_BUF_SIZE
       characters long (defined to be 1024 in xmlparse.c).  We take a
       really cheesy approach to building the input buffer, because
       this avoids writing bugs in buffer-filling code.
    */
    const char *text =
        "<e>"
        /* 64 chars */
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-+"
        /* until we have at least 1024 characters on the line: */
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-+"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-+"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-+"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-+"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-+"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-+"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-+"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-+"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-+"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-+"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-+"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-+"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-+"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-+"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-+"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-+"
        "</e>";
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
}
END_TEST


/*
 * Element event tests.
 */

static void XMLCALL
end_element_event_handler(void *userData, const XML_Char *name)
{
    CharData *storage = (CharData *) userData;
    CharData_AppendString(storage, "/");
    CharData_AppendXMLChars(storage, name, -1);
}

START_TEST(test_end_element_events)
{
    const char *text = "<a><b><c/></b><d><f/></d></a>";
    const char *expected = "/c/b/f/d/a";
    CharData storage;

    CharData_Init(&storage);
    XML_SetUserData(parser, &storage);
    XML_SetEndElementHandler(parser, end_element_event_handler);
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
    CharData_CheckString(&storage, expected);
}
END_TEST


/*
 * Attribute tests.
 */

/* Helpers used by the following test; this checks any "attr" and "refs"
   attributes to make sure whitespace has been normalized.

   Return true if whitespace has been normalized in a string, using
   the rules for attribute value normalization.  The 'is_cdata' flag
   is needed since CDATA attributes don't need to have multiple
   whitespace characters collapsed to a single space, while other
   attribute data types do.  (Section 3.3.3 of the recommendation.)
*/
static int
is_whitespace_normalized(const XML_Char *s, int is_cdata)
{
    int blanks = 0;
    int at_start = 1;
    while (*s) {
        if (*s == ' ')
            ++blanks;
        else if (*s == '\t' || *s == '\n' || *s == '\r')
            return 0;
        else {
            if (at_start) {
                at_start = 0;
                if (blanks && !is_cdata)
                    /* illegal leading blanks */
                    return 0;
            }
            else if (blanks > 1 && !is_cdata)
                return 0;
            blanks = 0;
        }
        ++s;
    }
    if (blanks && !is_cdata)
        return 0;
    return 1;
}

/* Check the attribute whitespace checker: */
static void
testhelper_is_whitespace_normalized(void)
{
    assert(is_whitespace_normalized("abc", 0));
    assert(is_whitespace_normalized("abc", 1));
    assert(is_whitespace_normalized("abc def ghi", 0));
    assert(is_whitespace_normalized("abc def ghi", 1));
    assert(!is_whitespace_normalized(" abc def ghi", 0));
    assert(is_whitespace_normalized(" abc def ghi", 1));
    assert(!is_whitespace_normalized("abc  def ghi", 0));
    assert(is_whitespace_normalized("abc  def ghi", 1));
    assert(!is_whitespace_normalized("abc def ghi ", 0));
    assert(is_whitespace_normalized("abc def ghi ", 1));
    assert(!is_whitespace_normalized(" ", 0));
    assert(is_whitespace_normalized(" ", 1));
    assert(!is_whitespace_normalized("\t", 0));
    assert(!is_whitespace_normalized("\t", 1));
    assert(!is_whitespace_normalized("\n", 0));
    assert(!is_whitespace_normalized("\n", 1));
    assert(!is_whitespace_normalized("\r", 0));
    assert(!is_whitespace_normalized("\r", 1));
    assert(!is_whitespace_normalized("abc\t def", 1));
}

static void XMLCALL
check_attr_contains_normalized_whitespace(void *UNUSED_P(userData),
                                          const XML_Char *UNUSED_P(name),
                                          const XML_Char **atts)
{
    int i;
    for (i = 0; atts[i] != NULL; i += 2) {
        const XML_Char *attrname = atts[i];
        const XML_Char *value = atts[i + 1];
        if (strcmp("attr", attrname) == 0
            || strcmp("ents", attrname) == 0
            || strcmp("refs", attrname) == 0) {
            if (!is_whitespace_normalized(value, 0)) {
                char buffer[256];
                sprintf(buffer, "attribute value not normalized: %s='%s'",
                        attrname, value);
                fail(buffer);
            }
        }
    }
}

START_TEST(test_attr_whitespace_normalization)
{
    const char *text =
        "<!DOCTYPE doc [\n"
        "  <!ATTLIST doc\n"
        "            attr NMTOKENS #REQUIRED\n"
        "            ents ENTITIES #REQUIRED\n"
        "            refs IDREFS   #REQUIRED>\n"
        "]>\n"
        "<doc attr='    a  b c\t\td\te\t' refs=' id-1   \t  id-2\t\t'  \n"
        "     ents=' ent-1   \t\r\n"
        "            ent-2  ' >\n"
        "  <e id='id-1'/>\n"
        "  <e id='id-2'/>\n"
        "</doc>";

    XML_SetStartElementHandler(parser,
                               check_attr_contains_normalized_whitespace);
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
}
END_TEST


/*
 * XML declaration tests.
 */

START_TEST(test_xmldecl_misplaced)
{
    expect_failure("\n"
                   "<?xml version='1.0'?>\n"
                   "<a/>",
                   XML_ERROR_MISPLACED_XML_PI,
                   "failed to report misplaced XML declaration");
}
END_TEST

/* Regression test for SF bug #584832. */
static int XMLCALL
UnknownEncodingHandler(void *UNUSED_P(data),const XML_Char *encoding,XML_Encoding *info)
{
    if (strcmp(encoding,"unsupported-encoding") == 0) {
        int i;
        for (i = 0; i < 256; ++i)
            info->map[i] = i;
        info->data = NULL;
        info->convert = NULL;
        info->release = NULL;
        return XML_STATUS_OK;
    }
    return XML_STATUS_ERROR;
}

START_TEST(test_unknown_encoding_internal_entity)
{
    const char *text =
        "<?xml version='1.0' encoding='unsupported-encoding'?>\n"
        "<!DOCTYPE test [<!ENTITY foo 'bar'>]>\n"
        "<test a='&foo;'/>";

    XML_SetUnknownEncodingHandler(parser, UnknownEncodingHandler, NULL);
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
}
END_TEST

/* Test unrecognised encoding handler */
static void dummy_release(void *UNUSED_P(data))
{
}

static int XMLCALL
UnrecognisedEncodingHandler(void *UNUSED_P(data),
                            const XML_Char *UNUSED_P(encoding),
                            XML_Encoding *info)
{
    info->data = NULL;
    info->convert = NULL;
    info->release = dummy_release;
    return XML_STATUS_ERROR;
}

START_TEST(test_unrecognised_encoding_internal_entity)
{
    const char *text =
        "<?xml version='1.0' encoding='unsupported-encoding'?>\n"
        "<!DOCTYPE test [<!ENTITY foo 'bar'>]>\n"
        "<test a='&foo;'/>";

    XML_SetUnknownEncodingHandler(parser,
                                  UnrecognisedEncodingHandler,
                                  NULL);
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) != XML_STATUS_ERROR)
        fail("Unrecognised encoding not rejected");
}
END_TEST

/* Regression test for SF bug #620106. */
static int XMLCALL
external_entity_loader_set_encoding(XML_Parser parser,
                                    const XML_Char *context,
                                    const XML_Char *UNUSED_P(base),
                                    const XML_Char *UNUSED_P(systemId),
                                    const XML_Char *UNUSED_P(publicId))
{
    /* This text says it's an unsupported encoding, but it's really
       UTF-8, which we tell Expat using XML_SetEncoding().
    */
    const char *text =
        "<?xml encoding='iso-8859-3'?>"
        "\xC3\xA9";
    XML_Parser extparser;

    extparser = XML_ExternalEntityParserCreate(parser, context, NULL);
    if (extparser == NULL)
        fail("Could not create external entity parser.");
    if (!XML_SetEncoding(extparser, "utf-8"))
        fail("XML_SetEncoding() ignored for external entity");
    if (  _XML_Parse_SINGLE_BYTES(extparser, text, strlen(text), XML_TRUE)
          == XML_STATUS_ERROR) {
        xml_failure(parser);
        return 0;
    }
    return 1;
}

START_TEST(test_ext_entity_set_encoding)
{
    const char *text =
        "<!DOCTYPE doc [\n"
        "  <!ENTITY en SYSTEM 'http://xml.libexpat.org/dummy.ent'>\n"
        "]>\n"
        "<doc>&en;</doc>";

    XML_SetExternalEntityRefHandler(parser,
                                    external_entity_loader_set_encoding);
    run_character_check(text, "\xC3\xA9");
}
END_TEST

/* Test that no error is reported for unknown entities if we don't
   read an external subset.  This was fixed in Expat 1.95.5.
*/
START_TEST(test_wfc_undeclared_entity_unread_external_subset) {
    const char *text =
        "<!DOCTYPE doc SYSTEM 'foo'>\n"
        "<doc>&entity;</doc>";

    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
}
END_TEST

/* Test that an error is reported for unknown entities if we don't
   have an external subset.
*/
START_TEST(test_wfc_undeclared_entity_no_external_subset) {
    expect_failure("<doc>&entity;</doc>",
                   XML_ERROR_UNDEFINED_ENTITY,
                   "Parser did not report undefined entity w/out a DTD.");
}
END_TEST

/* Test that an error is reported for unknown entities if we don't
   read an external subset, but have been declared standalone.
*/
START_TEST(test_wfc_undeclared_entity_standalone) {
    const char *text =
        "<?xml version='1.0' encoding='us-ascii' standalone='yes'?>\n"
        "<!DOCTYPE doc SYSTEM 'foo'>\n"
        "<doc>&entity;</doc>";

    expect_failure(text,
                   XML_ERROR_UNDEFINED_ENTITY,
                   "Parser did not report undefined entity (standalone).");
}
END_TEST

static int XMLCALL
external_entity_loader(XML_Parser parser,
                       const XML_Char *context,
                       const XML_Char *UNUSED_P(base),
                       const XML_Char *UNUSED_P(systemId),
                       const XML_Char *UNUSED_P(publicId))
{
    char *text = (char *)XML_GetUserData(parser);
    XML_Parser extparser;

    extparser = XML_ExternalEntityParserCreate(parser, context, NULL);
    if (extparser == NULL)
        fail("Could not create external entity parser.");
    if (  _XML_Parse_SINGLE_BYTES(extparser, text, strlen(text), XML_TRUE)
          == XML_STATUS_ERROR) {
        xml_failure(parser);
        return XML_STATUS_ERROR;
    }
    return XML_STATUS_OK;
}

/* Test that an error is reported for unknown entities if we have read
   an external subset, and standalone is true.
*/
START_TEST(test_wfc_undeclared_entity_with_external_subset_standalone) {
    const char *text =
        "<?xml version='1.0' encoding='us-ascii' standalone='yes'?>\n"
        "<!DOCTYPE doc SYSTEM 'foo'>\n"
        "<doc>&entity;</doc>";
    char foo_text[] =
        "<!ELEMENT doc (#PCDATA)*>";

    XML_SetParamEntityParsing(parser, XML_PARAM_ENTITY_PARSING_ALWAYS);
    XML_SetUserData(parser, foo_text);
    XML_SetExternalEntityRefHandler(parser, external_entity_loader);
    expect_failure(text,
                   XML_ERROR_UNDEFINED_ENTITY,
                   "Parser did not report undefined entity (external DTD).");
}
END_TEST

/* Test that no error is reported for unknown entities if we have read
   an external subset, and standalone is false.
*/
START_TEST(test_wfc_undeclared_entity_with_external_subset) {
    const char *text =
        "<?xml version='1.0' encoding='us-ascii'?>\n"
        "<!DOCTYPE doc SYSTEM 'foo'>\n"
        "<doc>&entity;</doc>";
    char foo_text[] =
        "<!ELEMENT doc (#PCDATA)*>";

    XML_SetParamEntityParsing(parser, XML_PARAM_ENTITY_PARSING_ALWAYS);
    XML_SetUserData(parser, foo_text);
    XML_SetExternalEntityRefHandler(parser, external_entity_loader);
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
}
END_TEST

/* Test that an error is reported if our NotStandalone handler fails */
static int XMLCALL
reject_not_standalone_handler(void *UNUSED_P(userData))
{
    return XML_STATUS_ERROR;
}

START_TEST(test_not_standalone_handler_reject)
{
    const char *text =
        "<?xml version='1.0' encoding='us-ascii'?>\n"
        "<!DOCTYPE doc SYSTEM 'foo'>\n"
        "<doc>&entity;</doc>";
    char foo_text[] =
        "<!ELEMENT doc (#PCDATA)*>";

    XML_SetParamEntityParsing(parser, XML_PARAM_ENTITY_PARSING_ALWAYS);
    XML_SetUserData(parser, foo_text);
    XML_SetExternalEntityRefHandler(parser, external_entity_loader);
    XML_SetNotStandaloneHandler(parser, reject_not_standalone_handler);
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) != XML_STATUS_ERROR)
        fail("NotStandalone handler failed to reject");
}
END_TEST

/* Test that no error is reported if our NotStandalone handler succeeds */
static int XMLCALL
accept_not_standalone_handler(void *UNUSED_P(userData))
{
    return XML_STATUS_OK;
}

START_TEST(test_not_standalone_handler_accept)
{
    const char *text =
        "<?xml version='1.0' encoding='us-ascii'?>\n"
        "<!DOCTYPE doc SYSTEM 'foo'>\n"
        "<doc>&entity;</doc>";
    char foo_text[] =
        "<!ELEMENT doc (#PCDATA)*>";

    XML_SetParamEntityParsing(parser, XML_PARAM_ENTITY_PARSING_ALWAYS);
    XML_SetUserData(parser, foo_text);
    XML_SetExternalEntityRefHandler(parser, external_entity_loader);
    XML_SetNotStandaloneHandler(parser, accept_not_standalone_handler);
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
}
END_TEST

START_TEST(test_wfc_no_recursive_entity_refs)
{
    const char *text =
        "<!DOCTYPE doc [\n"
        "  <!ENTITY entity '&#38;entity;'>\n"
        "]>\n"
        "<doc>&entity;</doc>";

    expect_failure(text,
                   XML_ERROR_RECURSIVE_ENTITY_REF,
                   "Parser did not report recursive entity reference.");
}
END_TEST

/* Regression test for SF bug #483514. */
START_TEST(test_dtd_default_handling)
{
    const char *text =
        "<!DOCTYPE doc [\n"
        "<!ENTITY e SYSTEM 'http://xml.libexpat.org/e'>\n"
        "<!NOTATION n SYSTEM 'http://xml.libexpat.org/n'>\n"
        "<!ELEMENT doc EMPTY>\n"
        "<!ATTLIST doc a CDATA #IMPLIED>\n"
        "<?pi in dtd?>\n"
        "<!--comment in dtd-->\n"
        "]><doc/>";

    XML_SetDefaultHandler(parser, accumulate_characters);
    XML_SetStartDoctypeDeclHandler(parser, dummy_start_doctype_handler);
    XML_SetEndDoctypeDeclHandler(parser, dummy_end_doctype_handler);
    XML_SetEntityDeclHandler(parser, dummy_entity_decl_handler);
    XML_SetNotationDeclHandler(parser, dummy_notation_decl_handler);
    XML_SetElementDeclHandler(parser, dummy_element_decl_handler);
    XML_SetAttlistDeclHandler(parser, dummy_attlist_decl_handler);
    XML_SetProcessingInstructionHandler(parser, dummy_pi_handler);
    XML_SetCommentHandler(parser, dummy_comment_handler);
    XML_SetStartCdataSectionHandler(parser, dummy_start_cdata_handler);
    XML_SetEndCdataSectionHandler(parser, dummy_end_cdata_handler);
    run_character_check(text, "\n\n\n\n\n\n\n<doc/>");
}
END_TEST

/* See related SF bug #673791.
   When namespace processing is enabled, setting the namespace URI for
   a prefix is not allowed; this test ensures that it *is* allowed
   when namespace processing is not enabled.
   (See Namespaces in XML, section 2.)
*/
START_TEST(test_empty_ns_without_namespaces)
{
    const char *text =
        "<doc xmlns:prefix='http://www.example.com/'>\n"
        "  <e xmlns:prefix=''/>\n"
        "</doc>";

    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
}
END_TEST

/* Regression test for SF bug #824420.
   Checks that an xmlns:prefix attribute set in an attribute's default
   value isn't misinterpreted.
*/
START_TEST(test_ns_in_attribute_default_without_namespaces)
{
    const char *text =
        "<!DOCTYPE e:element [\n"
        "  <!ATTLIST e:element\n"
        "    xmlns:e CDATA 'http://example.com/'>\n"
        "      ]>\n"
        "<e:element/>";

    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
}
END_TEST

static const char *long_character_data_text =
    "<?xml version='1.0' encoding='iso-8859-1'?><s>"
    "012345678901234567890123456789012345678901234567890123456789"
    "012345678901234567890123456789012345678901234567890123456789"
    "012345678901234567890123456789012345678901234567890123456789"
    "012345678901234567890123456789012345678901234567890123456789"
    "012345678901234567890123456789012345678901234567890123456789"
    "012345678901234567890123456789012345678901234567890123456789"
    "012345678901234567890123456789012345678901234567890123456789"
    "012345678901234567890123456789012345678901234567890123456789"
    "012345678901234567890123456789012345678901234567890123456789"
    "012345678901234567890123456789012345678901234567890123456789"
    "012345678901234567890123456789012345678901234567890123456789"
    "012345678901234567890123456789012345678901234567890123456789"
    "012345678901234567890123456789012345678901234567890123456789"
    "012345678901234567890123456789012345678901234567890123456789"
    "012345678901234567890123456789012345678901234567890123456789"
    "012345678901234567890123456789012345678901234567890123456789"
    "012345678901234567890123456789012345678901234567890123456789"
    "012345678901234567890123456789012345678901234567890123456789"
    "012345678901234567890123456789012345678901234567890123456789"
    "012345678901234567890123456789012345678901234567890123456789"
    "</s>";

static XML_Bool resumable = XML_FALSE;

static void
clearing_aborting_character_handler(void *UNUSED_P(userData),
                                    const XML_Char *UNUSED_P(s), int UNUSED_P(len))
{
    XML_StopParser(parser, resumable);
    XML_SetCharacterDataHandler(parser, NULL);
}

/* Regression test for SF bug #1515266: missing check of stopped
   parser in doContext() 'for' loop. */
START_TEST(test_stop_parser_between_char_data_calls)
{
    /* The sample data must be big enough that there are two calls to
       the character data handler from within the inner "for" loop of
       the XML_TOK_DATA_CHARS case in doContent(), and the character
       handler must stop the parser and clear the character data
       handler.
    */
    const char *text = long_character_data_text;

    XML_SetCharacterDataHandler(parser, clearing_aborting_character_handler);
    resumable = XML_FALSE;
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) != XML_STATUS_ERROR)
        xml_failure(parser);
    if (XML_GetErrorCode(parser) != XML_ERROR_ABORTED)
        xml_failure(parser);
}
END_TEST

/* Regression test for SF bug #1515266: missing check of stopped
   parser in doContext() 'for' loop. */
START_TEST(test_suspend_parser_between_char_data_calls)
{
    /* The sample data must be big enough that there are two calls to
       the character data handler from within the inner "for" loop of
       the XML_TOK_DATA_CHARS case in doContent(), and the character
       handler must stop the parser and clear the character data
       handler.
    */
    const char *text = long_character_data_text;

    XML_SetCharacterDataHandler(parser, clearing_aborting_character_handler);
    resumable = XML_TRUE;
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) != XML_STATUS_SUSPENDED)
        xml_failure(parser);
    if (XML_GetErrorCode(parser) != XML_ERROR_NONE)
        xml_failure(parser);
    /* Try parsing directly */
    if (XML_Parse(parser, text, strlen(text), XML_TRUE) != XML_STATUS_ERROR)
        fail("Attempt to continue parse while suspended not faulted");
    if (XML_GetErrorCode(parser) != XML_ERROR_SUSPENDED)
        fail("Suspended parse not faulted with correct error");
}
END_TEST


static XML_Bool abortable = XML_FALSE;

static void
parser_stop_character_handler(void *UNUSED_P(userData),
                              const XML_Char *UNUSED_P(s),
                              int UNUSED_P(len))
{
    XML_StopParser(parser, resumable);
    XML_SetCharacterDataHandler(parser, NULL);
    if (!resumable) {
        /* Check that aborting an aborted parser is faulted */
        if (XML_StopParser(parser, XML_FALSE) != XML_STATUS_ERROR)
            fail("Aborting aborted parser not faulted");
        if (XML_GetErrorCode(parser) != XML_ERROR_FINISHED)
            xml_failure(parser);
    } else if (abortable) {
        /* Check that aborting a suspended parser works */
        if (XML_StopParser(parser, XML_FALSE) == XML_STATUS_ERROR)
            xml_failure(parser);
    } else {
        /* Check that suspending a suspended parser works */
        if (XML_StopParser(parser, XML_TRUE) != XML_STATUS_ERROR)
            fail("Suspending suspended parser not faulted");
        if (XML_GetErrorCode(parser) != XML_ERROR_SUSPENDED)
            xml_failure(parser);
    }
}

/* Test repeated calls to XML_StopParser are handled correctly */
START_TEST(test_repeated_stop_parser_between_char_data_calls)
{
    const char *text = long_character_data_text;

    XML_SetCharacterDataHandler(parser, parser_stop_character_handler);
    resumable = XML_FALSE;
    abortable = XML_FALSE;
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text),
                                XML_TRUE) != XML_STATUS_ERROR)
        fail("Failed to double-stop parser");

    XML_ParserReset(parser, NULL);
    XML_SetCharacterDataHandler(parser, parser_stop_character_handler);
    resumable = XML_TRUE;
    abortable = XML_FALSE;
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text),
                                XML_TRUE) != XML_STATUS_SUSPENDED)
        fail("Failed to double-suspend parser");

    XML_ParserReset(parser, NULL);
    XML_SetCharacterDataHandler(parser, parser_stop_character_handler);
    resumable = XML_TRUE;
    abortable = XML_TRUE;
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text),
                                XML_TRUE) != XML_STATUS_ERROR)
        fail("Failed to suspend-abort parser");
}
END_TEST


START_TEST(test_good_cdata_ascii)
{
    const char *text = "<a><![CDATA[<greeting>Hello, world!</greeting>]]></a>";
    const char *expected = "<greeting>Hello, world!</greeting>";

    CharData storage;
    CharData_Init(&storage);
    XML_SetUserData(parser, &storage);
    XML_SetCharacterDataHandler(parser, accumulate_characters);

    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
    CharData_CheckXMLChars(&storage, expected);
}
END_TEST

START_TEST(test_good_cdata_utf16)
{
    /* Test data is:
     *   <?xml version='1.0' encoding='utf-16'?>
     *   <a><![CDATA[hello]]></a>
     */
    const char text[] =
            "\0<\0?\0x\0m\0l\0"
                " \0v\0e\0r\0s\0i\0o\0n\0=\0'\0\x31\0.\0\x30\0'\0"
                " \0e\0n\0c\0o\0d\0i\0n\0g\0=\0'\0u\0t\0f\0-\0""1\0""6\0'"
                "\0?\0>\0\n"
            "\0<\0a\0>\0<\0!\0[\0C\0D\0A\0T\0A\0[\0h\0e\0l\0l\0o\0]\0]\0>\0<\0/\0a\0>";
    const char *expected = "hello";

    CharData storage;
    CharData_Init(&storage);
    XML_SetUserData(parser, &storage);
    XML_SetCharacterDataHandler(parser, accumulate_characters);

    if (_XML_Parse_SINGLE_BYTES(parser, text, sizeof(text) - 1, XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
    CharData_CheckXMLChars(&storage, expected);
}
END_TEST

START_TEST(test_bad_cdata)
{
    struct CaseData {
        const char *text;
        enum XML_Error expectedError;
    };

    struct CaseData cases[] = {
        {"<a><", XML_ERROR_UNCLOSED_TOKEN},
        {"<a><!", XML_ERROR_UNCLOSED_TOKEN},
        {"<a><![", XML_ERROR_UNCLOSED_TOKEN},
        {"<a><![C", XML_ERROR_UNCLOSED_TOKEN},
        {"<a><![CD", XML_ERROR_UNCLOSED_TOKEN},
        {"<a><![CDA", XML_ERROR_UNCLOSED_TOKEN},
        {"<a><![CDAT", XML_ERROR_UNCLOSED_TOKEN},
        {"<a><![CDATA", XML_ERROR_UNCLOSED_TOKEN},

        {"<a><![CDATA[", XML_ERROR_UNCLOSED_CDATA_SECTION},
        {"<a><![CDATA[]", XML_ERROR_UNCLOSED_CDATA_SECTION},
        {"<a><![CDATA[]]", XML_ERROR_UNCLOSED_CDATA_SECTION},

        {"<a><!<a/>", XML_ERROR_INVALID_TOKEN},
        {"<a><![<a/>", XML_ERROR_UNCLOSED_TOKEN}, /* ?! */
        {"<a><![C<a/>", XML_ERROR_UNCLOSED_TOKEN}, /* ?! */
        {"<a><![CD<a/>", XML_ERROR_INVALID_TOKEN},
        {"<a><![CDA<a/>", XML_ERROR_INVALID_TOKEN},
        {"<a><![CDAT<a/>", XML_ERROR_INVALID_TOKEN},
        {"<a><![CDATA<a/>", XML_ERROR_INVALID_TOKEN},

        {"<a><![CDATA[<a/>", XML_ERROR_UNCLOSED_CDATA_SECTION},
        {"<a><![CDATA[]<a/>", XML_ERROR_UNCLOSED_CDATA_SECTION},
        {"<a><![CDATA[]]<a/>", XML_ERROR_UNCLOSED_CDATA_SECTION}
    };

    size_t i = 0;
    for (; i < sizeof(cases) / sizeof(struct CaseData); i++) {
        const enum XML_Status actualStatus = _XML_Parse_SINGLE_BYTES(
                parser, cases[i].text, strlen(cases[i].text), XML_TRUE);
        const enum XML_Error actualError = XML_GetErrorCode(parser);

        assert(actualStatus == XML_STATUS_ERROR);

        if (actualError != cases[i].expectedError) {
            char message[100];
            sprintf(message, "Expected error %d but got error %d for case %u: \"%s\"\n",
                    cases[i].expectedError, actualError, (unsigned int)i + 1, cases[i].text);
            fail(message);
        }

        XML_ParserReset(parser, NULL);
    }
}
END_TEST

/* Test memory allocation functions */
START_TEST(test_memory_allocation)
{
    char *buffer = (char *)XML_MemMalloc(parser, 256);
    char *p;

    if (buffer == NULL) {
        fail("Allocation failed");
    } else {
        /* Try writing to memory; some OSes try to cheat! */
        buffer[0] = 'T';
        buffer[1] = 'E';
        buffer[2] = 'S';
        buffer[3] = 'T';
        buffer[4] = '\0';
        if (strcmp(buffer, "TEST") != 0) {
            fail("Memory not writable");
        } else {
            p = (char *)XML_MemRealloc(parser, buffer, 512);
            if (p == NULL) {
                fail("Reallocation failed");
            } else {
                /* Write again, just to be sure */
                buffer = p;
                buffer[0] = 'V';
                if (strcmp(buffer, "VEST") != 0) {
                    fail("Reallocated memory not writable");
                }
            }
        }
        XML_MemFree(parser, buffer);
    }
}
END_TEST

static void XMLCALL
record_default_handler(void *userData,
                       const XML_Char *UNUSED_P(s),
                       int UNUSED_P(len))
{
    CharData_AppendString((CharData *)userData, "D");
}

static void XMLCALL
record_cdata_handler(void *userData,
                     const XML_Char *UNUSED_P(s),
                     int UNUSED_P(len))
{
    CharData_AppendString((CharData *)userData, "C");
    XML_DefaultCurrent(parser);
}

static void XMLCALL
record_cdata_nodefault_handler(void *userData,
                     const XML_Char *UNUSED_P(s),
                     int UNUSED_P(len))
{
    CharData_AppendString((CharData *)userData, "c");
}

/* Test XML_DefaultCurrent() passes handling on correctly */
START_TEST(test_default_current)
{
    const char *text = "<doc>hello</doc>";
    const char *entity_text =
        "<!DOCTYPE doc [\n"
        "<!ENTITY entity '&#37;'>\n"
        "]>\n"
        "<doc>&entity;</doc>";
    CharData storage;

    XML_SetDefaultHandler(parser, record_default_handler);
    XML_SetCharacterDataHandler(parser, record_cdata_handler);
    CharData_Init(&storage);
    XML_SetUserData(parser, &storage);
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text),
                                XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
    CharData_CheckString(&storage, "DCDCDCDCDCDD");

    /* Again, without the defaulting */
    XML_ParserReset(parser, NULL);
    XML_SetDefaultHandler(parser, record_default_handler);
    XML_SetCharacterDataHandler(parser, record_cdata_nodefault_handler);
    CharData_Init(&storage);
    XML_SetUserData(parser, &storage);
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text),
                                XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
    CharData_CheckString(&storage, "DcccccD");

    /* Now with an internal entity to complicate matters */
    XML_ParserReset(parser, NULL);
    XML_SetDefaultHandler(parser, record_default_handler);
    XML_SetCharacterDataHandler(parser, record_cdata_handler);
    CharData_Init(&storage);
    XML_SetUserData(parser, &storage);
    if (_XML_Parse_SINGLE_BYTES(parser, entity_text, strlen(entity_text),
                                XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
    /* The default handler suppresses the entity */
    CharData_CheckString(&storage, "DDDDDDDDDDDDDDDDDDD");

    /* This time, allow the entity through */
    XML_ParserReset(parser, NULL);
    XML_SetDefaultHandlerExpand(parser, record_default_handler);
    XML_SetCharacterDataHandler(parser, record_cdata_handler);
    CharData_Init(&storage);
    XML_SetUserData(parser, &storage);
    if (_XML_Parse_SINGLE_BYTES(parser, entity_text, strlen(entity_text),
                                XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
    CharData_CheckString(&storage, "DDDDDDDDDDDDDDDDDCDD");

    /* Finally, without passing the cdata to the default handler */
    XML_ParserReset(parser, NULL);
    XML_SetDefaultHandlerExpand(parser, record_default_handler);
    XML_SetCharacterDataHandler(parser, record_cdata_nodefault_handler);
    CharData_Init(&storage);
    XML_SetUserData(parser, &storage);
    if (_XML_Parse_SINGLE_BYTES(parser, entity_text, strlen(entity_text),
                                XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
    CharData_CheckString(&storage, "DDDDDDDDDDDDDDDDDcD");
}
END_TEST

/* Test DTD element parsing code paths */
START_TEST(test_dtd_elements)
{
    const char *text =
        "<!DOCTYPE doc [\n"
        "<!ELEMENT doc (chapter)>\n"
        "<!ELEMENT chapter (#PCDATA)>\n"
        "]>\n"
        "<doc><chapter>Wombats are go</chapter></doc>";

    XML_SetElementDeclHandler(parser, dummy_element_decl_handler);
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text),
                                XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
}
END_TEST

/* Test foreign DTD handling */
START_TEST(test_set_foreign_dtd)
{
    const char *text1 =
        "<?xml version='1.0' encoding='us-ascii'?>\n";
    const char *text2 =
        "<doc>&entity;</doc>";
    char dtd_text[] = "<!ELEMENT doc (#PCDATA)*>";

    /* Check hash salt is passed through too */
    XML_SetHashSalt(parser, 0x12345678);
    XML_SetParamEntityParsing(parser, XML_PARAM_ENTITY_PARSING_ALWAYS);
    XML_SetUserData(parser, dtd_text);
    XML_SetExternalEntityRefHandler(parser, external_entity_loader);
    if (XML_UseForeignDTD(parser, XML_TRUE) != XML_ERROR_NONE)
        fail("Could not set foreign DTD");
    if (_XML_Parse_SINGLE_BYTES(parser, text1, strlen(text1),
                                XML_FALSE) == XML_STATUS_ERROR)
        xml_failure(parser);

    /* Ensure that trying to set the DTD after parsing has started
     * is faulted, even if it's the same setting.
     */
    if (XML_UseForeignDTD(parser, XML_TRUE) == XML_ERROR_NONE)
        fail("Failed to reject late foreign DTD setting");
    /* Ditto for the hash salt */
    if (XML_SetHashSalt(parser, 0x23456789))
        fail("Failed to reject late hash salt change");

    /* Now finish the parse */
    if (_XML_Parse_SINGLE_BYTES(parser, text2, strlen(text2),
                                XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
}
END_TEST

/* Test XML Base is set and unset appropriately */
START_TEST(test_set_base)
{
    const XML_Char *old_base;
    const XML_Char *new_base = "/local/file/name.xml";

    old_base = XML_GetBase(parser);
    if (XML_SetBase(parser, new_base) != XML_STATUS_OK)
        fail("Unable to set base");
    if (strcmp(XML_GetBase(parser), new_base) != 0)
        fail("Base setting not correct");
    if (XML_SetBase(parser, NULL) != XML_STATUS_OK)
        fail("Unable to NULL base");
    if (XML_GetBase(parser) != NULL)
        fail("Base setting not nulled");
    XML_SetBase(parser, old_base);
}
END_TEST

/* Test attribute counts, indexing, etc */
typedef struct attrInfo {
    const char *name;
    const char *value;
} AttrInfo;

typedef struct elementInfo {
    const char *name;
    int attr_count;
    const char *id_name;
    AttrInfo *attributes;
} ElementInfo;

static void XMLCALL
counting_start_element_handler(void *userData,
                               const XML_Char *name,
                               const XML_Char **atts)
{
    ElementInfo *info = (ElementInfo *)userData;
    AttrInfo *attr;
    int count, id, i;

    while (info->name != NULL) {
        if (!strcmp(name, info->name))
            break;
        info++;
    }
    if (info->name == NULL)
        fail("Element not recognised");
    /* Note attribute count is doubled */
    count = XML_GetSpecifiedAttributeCount(parser);
    if (info->attr_count * 2 != count) {
        fail("Not got expected attribute count");
        return;
    }
    id = XML_GetIdAttributeIndex(parser);
    if (id == -1 && info->id_name != NULL) {
        fail("ID not present");
        return;
    }
    if (id != -1 && strcmp(atts[id], info->id_name)) {
        fail("ID does not have the correct name");
        return;
    }
    for (i = 0; i < info->attr_count; i++) {
        attr = info->attributes;
        while (attr->name != NULL) {
            if (!strcmp(atts[0], attr->name))
                break;
            attr++;
        }
        if (attr->name == NULL) {
            fail("Attribute not recognised");
            return;
        }
        if (strcmp(atts[1], attr->value)) {
            fail("Attribute has wrong value");
            return;
        }
        atts += 2;
    }
}

START_TEST(test_attributes)
{
    const char *text =
        "<!DOCTYPE doc [\n"
        "<!ELEMENT doc (tag)>\n"
        "<!ATTLIST doc id ID #REQUIRED>\n"
        "]>"
        "<doc a='1' id='one' b='2'>"
        "<tag c='3'/>"
        "</doc>";
    AttrInfo doc_info[] = {
        { "a",  "1" },
        { "b",  "2" },
        { "id", "one" },
        { NULL, NULL }
    };
    AttrInfo tag_info[] = {
        { "c",  "3" },
        { NULL, NULL }
    };
    ElementInfo info[] = {
        { "doc", 3, "id", NULL },
        { "tag", 1, NULL, NULL },
        { NULL, 0, NULL, NULL }
    };
    info[0].attributes = doc_info;
    info[1].attributes = tag_info;

    XML_SetStartElementHandler(parser, counting_start_element_handler);
    XML_SetUserData(parser, info);
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
}
END_TEST

/* Test reset works correctly in the middle of processing an internal
 * entity.  Exercises some obscure code in XML_ParserReset().
 */
START_TEST(test_reset_in_entity)
{
    const char *text =
        "<!DOCTYPE doc [\n"
        "<!ENTITY wombat 'wom'>\n"
        "<!ENTITY entity 'hi &wom; there'>\n"
        "]>\n"
        "<doc>&entity;</doc>";
    XML_ParsingStatus status;

    resumable = XML_TRUE;
    XML_SetCharacterDataHandler(parser, clearing_aborting_character_handler);
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_FALSE) == XML_STATUS_ERROR)
        xml_failure(parser);
    XML_GetParsingStatus(parser, &status);
    if (status.parsing != XML_SUSPENDED)
        fail("Parsing status not SUSPENDED");
    XML_ParserReset(parser, NULL);
    XML_GetParsingStatus(parser, &status);
    if (status.parsing != XML_INITIALIZED)
        fail("Parsing status doesn't reset to INITIALIZED");
}
END_TEST

/* Test that resume correctly passes through parse errors */
START_TEST(test_resume_invalid_parse)
{
    const char *text = "<doc>Hello</doc"; /* Missing closing wedge */

    resumable = XML_TRUE;
    XML_SetCharacterDataHandler(parser,
                                clearing_aborting_character_handler);
    if (XML_Parse(parser, text, strlen(text), XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
    if (XML_ResumeParser(parser) == XML_STATUS_OK)
        fail("Resumed invalid parse not faulted");
    if (XML_GetErrorCode(parser) != XML_ERROR_UNCLOSED_TOKEN)
        fail("Invalid parse not correctly faulted");
}
END_TEST

/* Test that re-suspended parses are correctly passed through */
START_TEST(test_resume_resuspended)
{
    const char *text = "<doc>Hello<meep/>world</doc>";

    resumable = XML_TRUE;
    XML_SetCharacterDataHandler(parser,
                                clearing_aborting_character_handler);
    if (XML_Parse(parser, text, strlen(text), XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
    resumable = XML_TRUE;
    XML_SetCharacterDataHandler(parser,
                                clearing_aborting_character_handler);
    if (XML_ResumeParser(parser) != XML_STATUS_SUSPENDED)
        fail("Resumption not suspended");
    /* This one should succeed and finish up */
    if (XML_ResumeParser(parser) != XML_STATUS_OK)
        xml_failure(parser);
}
END_TEST

/* Test resetting a subordinate parser does exactly nothing */
static int XMLCALL
external_entity_resetter(XML_Parser parser,
                         const XML_Char *context,
                         const XML_Char *UNUSED_P(base),
                         const XML_Char *UNUSED_P(systemId),
                         const XML_Char *UNUSED_P(publicId))
{
    const char *text = "<!ELEMENT doc (#PCDATA)*>";
    XML_Parser ext_parser;
    XML_ParsingStatus status;

    ext_parser = XML_ExternalEntityParserCreate(parser, context, NULL);
    if (ext_parser == NULL)
        fail("Could not create external entity parser");
    XML_GetParsingStatus(ext_parser, &status);
    if (status.parsing != XML_INITIALIZED) {
        fail("Parsing status is not INITIALIZED");
        return XML_STATUS_ERROR;
    }
    if (_XML_Parse_SINGLE_BYTES(ext_parser, text, strlen(text),
                                XML_TRUE) == XML_STATUS_ERROR) {
        xml_failure(parser);
        return XML_STATUS_ERROR;
    }
    XML_GetParsingStatus(ext_parser, &status);
    if (status.parsing != XML_FINISHED) {
        fail("Parsing status is not FINISHED");
        return XML_STATUS_ERROR;
    }
    /* Check we can't parse here */
    if (XML_Parse(ext_parser, text, strlen(text),
                  XML_TRUE) != XML_STATUS_ERROR)
        fail("Parsing when finished not faulted");
    if (XML_GetErrorCode(ext_parser) != XML_ERROR_FINISHED)
        fail("Parsing when finished faulted with wrong code");
    XML_ParserReset(ext_parser, NULL);
    XML_GetParsingStatus(ext_parser, &status);
    if (status.parsing != XML_FINISHED) {
        fail("Parsing status not still FINISHED");
        return XML_STATUS_ERROR;
    }
    return XML_STATUS_OK;
}

START_TEST(test_subordinate_reset)
{
   const char *text =
        "<?xml version='1.0' encoding='us-ascii'?>\n"
        "<!DOCTYPE doc SYSTEM 'foo'>\n"
        "<doc>&entity;</doc>";

    XML_SetParamEntityParsing(parser, XML_PARAM_ENTITY_PARSING_ALWAYS);
    XML_SetExternalEntityRefHandler(parser, external_entity_resetter);
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
}
END_TEST


/* Test suspending a subordinate parser */

static void XMLCALL
entity_suspending_decl_handler(void *userData,
                               const XML_Char *UNUSED_P(name),
                               XML_Content *model)
{
    XML_Parser ext_parser = (XML_Parser)userData;

    if (XML_StopParser(ext_parser, XML_TRUE) != XML_STATUS_ERROR)
        fail("Attempting to suspend a subordinate parser not faulted");
    if (XML_GetErrorCode(ext_parser) != XML_ERROR_SUSPEND_PE)
        fail("Suspending subordinate parser get wrong code");
    XML_SetElementDeclHandler(ext_parser, NULL);
    XML_FreeContentModel(parser, model);
}

static int XMLCALL
external_entity_suspender(XML_Parser parser,
                          const XML_Char *context,
                          const XML_Char *UNUSED_P(base),
                          const XML_Char *UNUSED_P(systemId),
                          const XML_Char *UNUSED_P(publicId))
{
    const char *text = "<!ELEMENT doc (#PCDATA)*>";
    XML_Parser ext_parser;

    ext_parser = XML_ExternalEntityParserCreate(parser, context, NULL);
    if (ext_parser == NULL)
        fail("Could not create external entity parser");
    XML_SetElementDeclHandler(ext_parser, entity_suspending_decl_handler);
    XML_SetUserData(ext_parser, ext_parser);
    if (_XML_Parse_SINGLE_BYTES(ext_parser, text, strlen(text),
                                XML_TRUE) == XML_STATUS_ERROR) {
        xml_failure(ext_parser);
        return XML_STATUS_ERROR;
    }
    return XML_STATUS_OK;
}

START_TEST(test_subordinate_suspend)
{
    const char *text =
        "<?xml version='1.0' encoding='us-ascii'?>\n"
        "<!DOCTYPE doc SYSTEM 'foo'>\n"
        "<doc>&entity;</doc>";

    XML_SetParamEntityParsing(parser, XML_PARAM_ENTITY_PARSING_ALWAYS);
    XML_SetExternalEntityRefHandler(parser, external_entity_suspender);
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
}
END_TEST


/* Test setting an explicit encoding */
START_TEST(test_explicit_encoding)
{
    const char *text1 = "<doc>Hello ";
    const char *text2 = " World</doc>";

    /* First say we are UTF-8 */
    if (XML_SetEncoding(parser, "utf-8") != XML_STATUS_OK)
        fail("Failed to set explicit encoding");
    if (_XML_Parse_SINGLE_BYTES(parser, text1, strlen(text1),
                                XML_FALSE) == XML_STATUS_ERROR)
        xml_failure(parser);
    /* Try to switch encodings mid-parse */
    if (XML_SetEncoding(parser, "us-ascii") != XML_STATUS_ERROR)
        fail("Allowed encoding change");
    if (_XML_Parse_SINGLE_BYTES(parser, text2, strlen(text2),
                                XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
    /* Try now the parse is over */
    if (XML_SetEncoding(parser, NULL) != XML_STATUS_OK)
        fail("Failed to unset encoding");
}
END_TEST

/* Test user parameter settings */
/* Variable holding the expected handler userData */
static void *handler_data = NULL;
/* Count of the number of times the comment handler has been invoked */
static int comment_count = 0;
/* Count of the number of skipped entities */
static int skip_count = 0;
/* Count of the number of times the XML declaration handler is invoked */
static int xdecl_count = 0;

static void XMLCALL
xml_decl_handler(void *userData,
                 const XML_Char *UNUSED_P(version),
                 const XML_Char *UNUSED_P(encoding),
                 int standalone)
{
    if (userData != handler_data)
        fail("User data (xml decl) not correctly set");
    if (standalone != -1)
        fail("Standalone not show as not present");
    xdecl_count++;
}

static void XMLCALL
param_check_skip_handler(void *userData,
                         const XML_Char *UNUSED_P(entityName),
                         int UNUSED_P(is_parameter_entity))
{
    if (userData != handler_data)
        fail("User data (skip) not correctly set");
    skip_count++;
}

static void XMLCALL
data_check_comment_handler(void *userData, const XML_Char *UNUSED_P(data))
{
    /* Check that the userData passed through is what we expect */
    if (userData != handler_data)
        fail("User data (parser) not correctly set");
    /* Check that the user data in the parser is appropriate */
    if (XML_GetUserData(userData) != (void *)1)
        fail("User data in parser not correctly set");
    comment_count++;
}

static int XMLCALL
external_entity_param_checker(XML_Parser parser,
                              const XML_Char *context,
                              const XML_Char *UNUSED_P(base),
                              const XML_Char *UNUSED_P(systemId),
                              const XML_Char *UNUSED_P(publicId))
{
    const char *text =
        "<!-- Subordinate parser -->\n"
        "<!ELEMENT doc (#PCDATA)*>";
    XML_Parser ext_parser;

    ext_parser = XML_ExternalEntityParserCreate(parser, context, NULL);
    if (ext_parser == NULL)
        fail("Could not create external entity parser");
    handler_data = ext_parser;
    if (_XML_Parse_SINGLE_BYTES(ext_parser, text, strlen(text),
                                XML_TRUE) == XML_STATUS_ERROR) {
        xml_failure(parser);
        return XML_STATUS_ERROR;
    }
    handler_data = parser;
    return XML_STATUS_OK;
}

START_TEST(test_user_parameters)
{
    const char *text =
        "<?xml version='1.0' encoding='us-ascii'?>\n"
        "<!-- Primary parse -->\n"
        "<!DOCTYPE doc SYSTEM 'foo'>\n"
        "<doc>&entity;";
    const char *epilog =
        "<!-- Back to primary parser -->\n"
        "</doc>";

    comment_count = 0;
    skip_count = 0;
    xdecl_count = 0;
    XML_SetParamEntityParsing(parser, XML_PARAM_ENTITY_PARSING_ALWAYS);
    XML_SetXmlDeclHandler(parser, xml_decl_handler);
    XML_SetExternalEntityRefHandler(parser, external_entity_param_checker);
    XML_SetCommentHandler(parser, data_check_comment_handler);
    XML_SetSkippedEntityHandler(parser, param_check_skip_handler);
    XML_UseParserAsHandlerArg(parser);
    XML_SetUserData(parser, (void *)1);
    handler_data = parser;
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text),
                                XML_FALSE) == XML_STATUS_ERROR)
        xml_failure(parser);
    if (comment_count != 2)
        fail("Comment handler not invoked enough times");
    /* Ensure we can't change policy mid-parse */
    if (XML_SetParamEntityParsing(parser, XML_PARAM_ENTITY_PARSING_NEVER))
        fail("Changed param entity parsing policy while parsing");
    if (_XML_Parse_SINGLE_BYTES(parser, epilog, strlen(epilog),
                                XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
    if (comment_count != 3)
        fail("Comment handler not invoked enough times");
    if (skip_count != 1)
        fail("Skip handler not invoked enough times");
    if (xdecl_count != 1)
        fail("XML declaration handler not invoked");
}
END_TEST

/* Test that an explicit external entity handler argument replaces
 * the parser as the first argument.
 */
static int XMLCALL
external_entity_ref_param_checker(XML_Parser parser,
                                  const XML_Char *UNUSED_P(context),
                                  const XML_Char *UNUSED_P(base),
                                  const XML_Char *UNUSED_P(systemId),
                                  const XML_Char *UNUSED_P(publicId))
{
    if ((void *)parser != handler_data)
        fail("External entity ref handler parameter not correct");
    return XML_STATUS_OK;
}

START_TEST(test_ext_entity_ref_parameter)
{
    const char *text =
        "<?xml version='1.0' encoding='us-ascii'?>\n"
        "<!DOCTYPE doc SYSTEM 'foo'>\n"
        "<doc>&entity;</doc>";

    XML_SetParamEntityParsing(parser, XML_PARAM_ENTITY_PARSING_ALWAYS);
    XML_SetExternalEntityRefHandler(parser,
                                    external_entity_ref_param_checker);
    /* Set a handler arg that is not NULL and not parser (which is
     * what NULL would cause to be passed.
     */
    XML_SetExternalEntityRefHandlerArg(parser, (void *)text);
    handler_data = (void *)text;
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text),
                                XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);

    /* Now try again with unset args */
    XML_ParserReset(parser, NULL);
    XML_SetParamEntityParsing(parser, XML_PARAM_ENTITY_PARSING_ALWAYS);
    XML_SetExternalEntityRefHandler(parser,
                                    external_entity_ref_param_checker);
    XML_SetExternalEntityRefHandlerArg(parser, NULL);
    handler_data = (void *)parser;
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text),
                                XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
}
END_TEST

/* Test the parsing of an empty string */
START_TEST(test_empty_parse)
{
    const char *text = "<doc></doc>";
    const char *partial = "<doc>";

    if (XML_Parse(parser, NULL, 0, XML_FALSE) == XML_STATUS_ERROR)
        fail("Parsing empty string faulted");
    if (XML_Parse(parser, NULL, 0, XML_TRUE) != XML_STATUS_ERROR)
        fail("Parsing final empty string not faulted");
    if (XML_GetErrorCode(parser) != XML_ERROR_NO_ELEMENTS)
        fail("Parsing final empty string faulted for wrong reason");

    /* Now try with valid text before the empty end */
    XML_ParserReset(parser, NULL);
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text),
                                XML_FALSE) == XML_STATUS_ERROR)
        xml_failure(parser);
    if (XML_Parse(parser, NULL, 0, XML_TRUE) == XML_STATUS_ERROR)
        fail("Parsing final empty string faulted");

    /* Now try with invalid text before the empty end */
    XML_ParserReset(parser, NULL);
    if (_XML_Parse_SINGLE_BYTES(parser, partial, strlen(partial),
                                XML_FALSE) == XML_STATUS_ERROR)
        xml_failure(parser);
    if (XML_Parse(parser, NULL, 0, XML_TRUE) != XML_STATUS_ERROR)
        fail("Parsing final incomplete empty string not faulted");
}
END_TEST

/* Test odd corners of the XML_GetBuffer interface */
START_TEST(test_get_buffer_1)
{
    const char *text =
        "<documentwitharidiculouslylongelementnametotease" /* 0x030 */
        "aparticularcorneroftheallocationinXML_GetBuffers" /* 0x060 */
        "othatwecanimprovethecoverageyetagain012345678901" /* 0x090 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x0c0 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x0f0 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x120 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x150 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x180 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x1b0 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x1e0 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x210 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x240 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x270 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x2a0 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x2d0 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x300 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x330 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x360 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x390 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x3c0 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x3f0 */
        "123456789abcdef0123456789abcdef0123456789>\n<ef0"; /* 0x420 */
    void *buffer;

    /* Attempt to allocate a negative length buffer */
    if (XML_GetBuffer(parser, -12) != NULL)
        fail("Negative length buffer not failed");

    /* Now get a small buffer and extend it past valid length */
    buffer = XML_GetBuffer(parser, 1536);
    if (buffer == NULL)
        fail("1.5K buffer failed");
    memcpy(buffer, text, strlen(text));
    if (XML_ParseBuffer(parser, strlen(text), XML_FALSE) == XML_STATUS_ERROR)
        xml_failure(parser);
    if (XML_GetBuffer(parser, INT_MAX) != NULL)
        fail("INT_MAX buffer not failed");

    /* Now try extending it a more reasonable but still too large amount */
    if (XML_GetBuffer(parser, INT_MAX - 2049) != NULL)
        fail("INT_MAX- buffer not failed");

    /* Now try extending it a carefully crafted amount */
    if (XML_GetBuffer(parser, 1000) == NULL)
        fail("1000 buffer failed");
}
END_TEST

/* Test more corners of the XML_GetBuffer interface */
START_TEST(test_get_buffer_2)
{
    const char *text =
        "<documentwitharidiculouslylongelementnametotease" /* 0x030 */
        "aparticularcorneroftheallocationinXML_GetBuffers" /* 0x060 */
        "othatwecanimprovethecoverageyetagain012345678901" /* 0x090 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x0c0 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x0f0 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x120 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x150 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x180 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x1b0 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x1e0 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x210 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x240 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x270 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x2a0 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x2d0 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x300 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x330 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x360 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x390 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x3c0 */
        "123456789abcdef0123456789abcdef0123456789abcdef0" /* 0x3f0 */
        "123456789abcdef0123456789abcdef0123456789>\n<ef0"; /* 0x420 */
    void *buffer;

    /* Now get a decent buffer */
    buffer = XML_GetBuffer(parser, 1536);
    if (buffer == NULL)
        fail("1.5K buffer failed");
    memcpy(buffer, text, strlen(text));
    if (XML_ParseBuffer(parser, strlen(text), XML_FALSE) == XML_STATUS_ERROR)
        xml_failure(parser);

    /* Extend it, to catch a different code path */
    if (XML_GetBuffer(parser, 1024) == NULL)
        fail("1024 buffer failed");
}
END_TEST

/* Test position information macros */
START_TEST(test_byte_info_at_end)
{
    const char *text = "<doc></doc>";

    if (XML_GetCurrentByteIndex(parser) != -1 ||
        XML_GetCurrentByteCount(parser) != 0)
        fail("Byte index/count incorrect at start of parse");
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text),
                                XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
    /* At end, the count will be zero and the index the end of string */
    if (XML_GetCurrentByteCount(parser) != 0)
        fail("Terminal byte count incorrect");
    if (XML_GetCurrentByteIndex(parser) != (XML_Index)strlen(text))
        fail("Terminal byte index incorrect");
}
END_TEST

/* Test position information from errors */
START_TEST(test_byte_info_at_error)
{
    const char *text = "<doc></wombat></doc>";

    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text),
                                XML_TRUE) == XML_STATUS_OK)
        fail("Syntax error not faulted");
    if (XML_GetCurrentByteCount(parser) != 0)
        fail("Error byte count incorrect");
    if (XML_GetCurrentByteIndex(parser) != 7)
        fail("Error byte index incorrect");
}
END_TEST

/* Test position information in handler */
static void
byte_character_handler(void *userData,
                       const XML_Char *s,
                       int len)
{
#ifdef XML_CONTEXT_BYTES
    int offset, size;
    const char *buffer;
    intptr_t buflen = (intptr_t)userData;

    buffer = XML_GetInputContext(parser, &offset, &size);
    if (buffer == NULL)
        fail("Failed to get context buffer");
    if (XML_GetCurrentByteIndex(parser) != offset)
        fail("Character byte index incorrect");
    if (XML_GetCurrentByteCount(parser) != len)
        fail("Character byte count incorrect");
    if (buflen != size)
        fail("Buffer length incorrect");
    if (s != buffer + offset)
        fail("Buffer position incorrect");
#else
    (void)userData;
    (void)s;
    (void)len;
#endif
}

START_TEST(test_byte_info_at_cdata)
{
    const char *text = "<doc>Hello</doc>";
    int offset, size;

    /* Check initial context is empty */
    if (XML_GetInputContext(parser, &offset, &size) != NULL)
        fail("Unexpected context at start of parse");

    XML_SetCharacterDataHandler(parser, byte_character_handler);
    XML_SetUserData(parser, (void *)strlen(text));
    if (XML_Parse(parser, text, strlen(text), XML_TRUE) != XML_STATUS_OK)
        xml_failure(parser);
}
END_TEST

/* Regression test that an invalid tag in an external parameter
 * reference in an external DTD is correctly faulted.
 *
 * Only a few specific tags are legal in DTDs ignoring comments and
 * processing instructions, all of which begin with an exclamation
 * mark.  "<el/>" is not one of them, so the parser should raise an
 * error on encountering it.
 */
static int XMLCALL
external_entity_param(XML_Parser parser,
                      const XML_Char *context,
                      const XML_Char *UNUSED_P(base),
                      const XML_Char *systemId,
                      const XML_Char *UNUSED_P(publicId))
{
    const char *text1 =
        "<!ELEMENT doc EMPTY>\n"
        "<!ENTITY % e1 SYSTEM '004-2.ent'>\n"
        "<!ENTITY % e2 '%e1;'>\n"
        "%e1;\n";
    const char *text2 =
        "<!ELEMENT el EMPTY>\n"
        "<el/>\n";
    XML_Parser ext_parser;

    if (systemId == NULL)
        return XML_STATUS_OK;

    ext_parser = XML_ExternalEntityParserCreate(parser, context, NULL);
    if (ext_parser == NULL)
        fail("Could not create external entity parser");

    if (!strcmp(systemId, "004-1.ent")) {
        if (_XML_Parse_SINGLE_BYTES(ext_parser, text1, strlen(text1),
                                    XML_TRUE) != XML_STATUS_ERROR)
            fail("Inner DTD with invalid tag not rejected");
        if (XML_GetErrorCode(ext_parser) != XML_ERROR_EXTERNAL_ENTITY_HANDLING)
            xml_failure(ext_parser);
    }
    else if (!strcmp(systemId, "004-2.ent")) {
        if (_XML_Parse_SINGLE_BYTES(ext_parser, text2, strlen(text2),
                                    XML_TRUE) != XML_STATUS_ERROR)
            fail("Invalid tag in external param not rejected");
        if (XML_GetErrorCode(ext_parser) != XML_ERROR_SYNTAX)
            xml_failure(ext_parser);
    } else {
        fail("Unknown system ID");
    }

    return XML_STATUS_ERROR;
}

START_TEST(test_invalid_tag_in_dtd)
{
    const char *text =
        "<!DOCTYPE doc SYSTEM '004-1.ent'>\n"
        "<doc></doc>\n";

    XML_SetParamEntityParsing(parser, XML_PARAM_ENTITY_PARSING_ALWAYS);
    XML_SetExternalEntityRefHandler(parser, external_entity_param);
    expect_failure(text, XML_ERROR_EXTERNAL_ENTITY_HANDLING,
                   "Invalid tag IN DTD external param not rejected");
}
END_TEST


/*
 * Namespaces tests.
 */

static void
namespace_setup(void)
{
    parser = XML_ParserCreateNS(NULL, ' ');
    if (parser == NULL)
        fail("Parser not created.");
}

static void
namespace_teardown(void)
{
    basic_teardown();
}

/* Check that an element name and attribute name match the expected values.
   The expected values are passed as an array reference of string pointers
   provided as the userData argument; the first is the expected
   element name, and the second is the expected attribute name.
*/
static int triplet_count = 0;

static void XMLCALL
triplet_start_checker(void *userData, const XML_Char *name,
                      const XML_Char **atts)
{
    char **elemstr = (char **)userData;
    char buffer[1024];
    if (strcmp(elemstr[0], name) != 0) {
        sprintf(buffer, "unexpected start string: '%s'", name);
        fail(buffer);
    }
    if (strcmp(elemstr[1], atts[0]) != 0) {
        sprintf(buffer, "unexpected attribute string: '%s'", atts[0]);
        fail(buffer);
    }
    triplet_count++;
}

/* Check that the element name passed to the end-element handler matches
   the expected value.  The expected value is passed as the first element
   in an array of strings passed as the userData argument.
*/
static void XMLCALL
triplet_end_checker(void *userData, const XML_Char *name)
{
    char **elemstr = (char **)userData;
    if (strcmp(elemstr[0], name) != 0) {
        char buffer[1024];
        sprintf(buffer, "unexpected end string: '%s'", name);
        fail(buffer);
    }
    triplet_count++;
}

START_TEST(test_return_ns_triplet)
{
    const char *text =
        "<foo:e xmlns:foo='http://expat.sf.net/' bar:a='12'\n"
        "       xmlns:bar='http://expat.sf.net/'>";
    const char *epilog = "</foo:e>";
    const char *elemstr[] = {
        "http://expat.sf.net/ e foo",
        "http://expat.sf.net/ a bar"
    };
    XML_SetReturnNSTriplet(parser, XML_TRUE);
    XML_SetUserData(parser, elemstr);
    XML_SetElementHandler(parser, triplet_start_checker,
                          triplet_end_checker);
    XML_SetNamespaceDeclHandler(parser,
                                dummy_start_namespace_decl_handler,
                                dummy_end_namespace_decl_handler);
    triplet_count = 0;
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text),
                                XML_FALSE) == XML_STATUS_ERROR)
        xml_failure(parser);
    if (triplet_count != 1)
        fail("triplet_start_checker not invoked");
    /* Check that unsetting "return triplets" fails while still parsing */
    XML_SetReturnNSTriplet(parser, XML_FALSE);
    if (_XML_Parse_SINGLE_BYTES(parser, epilog, strlen(epilog),
                                XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
    if (triplet_count != 2)
        fail("triplet_end_checker not invoked");
}
END_TEST

static void XMLCALL
overwrite_start_checker(void *userData, const XML_Char *name,
                        const XML_Char **atts)
{
    CharData *storage = (CharData *) userData;
    CharData_AppendString(storage, "start ");
    CharData_AppendXMLChars(storage, name, -1);
    while (*atts != NULL) {
        CharData_AppendString(storage, "\nattribute ");
        CharData_AppendXMLChars(storage, *atts, -1);
        atts += 2;
    }
    CharData_AppendString(storage, "\n");
}

static void XMLCALL
overwrite_end_checker(void *userData, const XML_Char *name)
{
    CharData *storage = (CharData *) userData;
    CharData_AppendString(storage, "end ");
    CharData_AppendXMLChars(storage, name, -1);
    CharData_AppendString(storage, "\n");
}

static void
run_ns_tagname_overwrite_test(const char *text, const char *result)
{
    CharData storage;
    CharData_Init(&storage);
    XML_SetUserData(parser, &storage);
    XML_SetElementHandler(parser,
                          overwrite_start_checker, overwrite_end_checker);
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
    CharData_CheckString(&storage, result);
}

/* Regression test for SF bug #566334. */
START_TEST(test_ns_tagname_overwrite)
{
    const char *text =
        "<n:e xmlns:n='http://xml.libexpat.org/'>\n"
        "  <n:f n:attr='foo'/>\n"
        "  <n:g n:attr2='bar'/>\n"
        "</n:e>";
    const char *result =
        "start http://xml.libexpat.org/ e\n"
        "start http://xml.libexpat.org/ f\n"
        "attribute http://xml.libexpat.org/ attr\n"
        "end http://xml.libexpat.org/ f\n"
        "start http://xml.libexpat.org/ g\n"
        "attribute http://xml.libexpat.org/ attr2\n"
        "end http://xml.libexpat.org/ g\n"
        "end http://xml.libexpat.org/ e\n";
    run_ns_tagname_overwrite_test(text, result);
}
END_TEST

/* Regression test for SF bug #566334. */
START_TEST(test_ns_tagname_overwrite_triplet)
{
    const char *text =
        "<n:e xmlns:n='http://xml.libexpat.org/'>\n"
        "  <n:f n:attr='foo'/>\n"
        "  <n:g n:attr2='bar'/>\n"
        "</n:e>";
    const char *result =
        "start http://xml.libexpat.org/ e n\n"
        "start http://xml.libexpat.org/ f n\n"
        "attribute http://xml.libexpat.org/ attr n\n"
        "end http://xml.libexpat.org/ f n\n"
        "start http://xml.libexpat.org/ g n\n"
        "attribute http://xml.libexpat.org/ attr2 n\n"
        "end http://xml.libexpat.org/ g n\n"
        "end http://xml.libexpat.org/ e n\n";
    XML_SetReturnNSTriplet(parser, XML_TRUE);
    run_ns_tagname_overwrite_test(text, result);
}
END_TEST


/* Regression test for SF bug #620343. */
static void XMLCALL
start_element_fail(void *UNUSED_P(userData),
                   const XML_Char *UNUSED_P(name), const XML_Char **UNUSED_P(atts))
{
    /* We should never get here. */
    fail("should never reach start_element_fail()");
}

static void XMLCALL
start_ns_clearing_start_element(void *userData,
                                const XML_Char *UNUSED_P(prefix),
                                const XML_Char *UNUSED_P(uri))
{
    XML_SetStartElementHandler((XML_Parser) userData, NULL);
}

START_TEST(test_start_ns_clears_start_element)
{
    /* This needs to use separate start/end tags; using the empty tag
       syntax doesn't cause the problematic path through Expat to be
       taken.
    */
    const char *text = "<e xmlns='http://xml.libexpat.org/'></e>";

    XML_SetStartElementHandler(parser, start_element_fail);
    XML_SetStartNamespaceDeclHandler(parser, start_ns_clearing_start_element);
    XML_SetEndNamespaceDeclHandler(parser, dummy_end_namespace_decl_handler);
    XML_UseParserAsHandlerArg(parser);
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
}
END_TEST

/* Regression test for SF bug #616863. */
static int XMLCALL
external_entity_handler(XML_Parser parser,
                        const XML_Char *context,
                        const XML_Char *UNUSED_P(base),
                        const XML_Char *UNUSED_P(systemId),
                        const XML_Char *UNUSED_P(publicId))
{
    intptr_t callno = 1 + (intptr_t)XML_GetUserData(parser);
    const char *text;
    XML_Parser p2;

    if (callno == 1)
        text = ("<!ELEMENT doc (e+)>\n"
                "<!ATTLIST doc xmlns CDATA #IMPLIED>\n"
                "<!ELEMENT e EMPTY>\n");
    else
        text = ("<?xml version='1.0' encoding='us-ascii'?>"
                "<e/>");

    XML_SetUserData(parser, (void *) callno);
    p2 = XML_ExternalEntityParserCreate(parser, context, NULL);
    if (_XML_Parse_SINGLE_BYTES(p2, text, strlen(text), XML_TRUE) == XML_STATUS_ERROR) {
        xml_failure(p2);
        return 0;
    }
    XML_ParserFree(p2);
    return 1;
}

START_TEST(test_default_ns_from_ext_subset_and_ext_ge)
{
    const char *text =
        "<?xml version='1.0'?>\n"
        "<!DOCTYPE doc SYSTEM 'http://xml.libexpat.org/doc.dtd' [\n"
        "  <!ENTITY en SYSTEM 'http://xml.libexpat.org/entity.ent'>\n"
        "]>\n"
        "<doc xmlns='http://xml.libexpat.org/ns1'>\n"
        "&en;\n"
        "</doc>";

    XML_SetParamEntityParsing(parser, XML_PARAM_ENTITY_PARSING_ALWAYS);
    XML_SetExternalEntityRefHandler(parser, external_entity_handler);
    /* We actually need to set this handler to tickle this bug. */
    XML_SetStartElementHandler(parser, dummy_start_element);
    XML_SetUserData(parser, NULL);
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
}
END_TEST

/* Regression test #1 for SF bug #673791. */
START_TEST(test_ns_prefix_with_empty_uri_1)
{
    const char *text =
        "<doc xmlns:prefix='http://xml.libexpat.org/'>\n"
        "  <e xmlns:prefix=''/>\n"
        "</doc>";

    expect_failure(text,
                   XML_ERROR_UNDECLARING_PREFIX,
                   "Did not report re-setting namespace"
                   " URI with prefix to ''.");
}
END_TEST

/* Regression test #2 for SF bug #673791. */
START_TEST(test_ns_prefix_with_empty_uri_2)
{
    const char *text =
        "<?xml version='1.0'?>\n"
        "<docelem xmlns:pre=''/>";

    expect_failure(text,
                   XML_ERROR_UNDECLARING_PREFIX,
                   "Did not report setting namespace URI with prefix to ''.");
}
END_TEST

/* Regression test #3 for SF bug #673791. */
START_TEST(test_ns_prefix_with_empty_uri_3)
{
    const char *text =
        "<!DOCTYPE doc [\n"
        "  <!ELEMENT doc EMPTY>\n"
        "  <!ATTLIST doc\n"
        "    xmlns:prefix CDATA ''>\n"
        "]>\n"
        "<doc/>";

    expect_failure(text,
                   XML_ERROR_UNDECLARING_PREFIX,
                   "Didn't report attr default setting NS w/ prefix to ''.");
}
END_TEST

/* Regression test #4 for SF bug #673791. */
START_TEST(test_ns_prefix_with_empty_uri_4)
{
    const char *text =
        "<!DOCTYPE doc [\n"
        "  <!ELEMENT prefix:doc EMPTY>\n"
        "  <!ATTLIST prefix:doc\n"
        "    xmlns:prefix CDATA 'http://xml.libexpat.org/'>\n"
        "]>\n"
        "<prefix:doc/>";
    /* Packaged info expected by the end element handler;
       the weird structuring lets us re-use the triplet_end_checker()
       function also used for another test. */
    const char *elemstr[] = {
        "http://xml.libexpat.org/ doc prefix"
    };
    XML_SetReturnNSTriplet(parser, XML_TRUE);
    XML_SetUserData(parser, elemstr);
    XML_SetEndElementHandler(parser, triplet_end_checker);
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
}
END_TEST

START_TEST(test_ns_default_with_empty_uri)
{
    const char *text =
        "<doc xmlns='http://xml.libexpat.org/'>\n"
        "  <e xmlns=''/>\n"
        "</doc>";
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
}
END_TEST

/* Regression test for SF bug #692964: two prefixes for one namespace. */
START_TEST(test_ns_duplicate_attrs_diff_prefixes)
{
    const char *text =
        "<doc xmlns:a='http://xml.libexpat.org/a'\n"
        "     xmlns:b='http://xml.libexpat.org/a'\n"
        "     a:a='v' b:a='v' />";
    expect_failure(text,
                   XML_ERROR_DUPLICATE_ATTRIBUTE,
                   "did not report multiple attributes with same URI+name");
}
END_TEST

/* Regression test for SF bug #695401: unbound prefix. */
START_TEST(test_ns_unbound_prefix_on_attribute)
{
    const char *text = "<doc a:attr=''/>";
    expect_failure(text,
                   XML_ERROR_UNBOUND_PREFIX,
                   "did not report unbound prefix on attribute");
}
END_TEST

/* Regression test for SF bug #695401: unbound prefix. */
START_TEST(test_ns_unbound_prefix_on_element)
{
    const char *text = "<a:doc/>";
    expect_failure(text,
                   XML_ERROR_UNBOUND_PREFIX,
                   "did not report unbound prefix on element");
}
END_TEST

/* Test that the parsing status is correctly reset by XML_ParserReset().
 * We usE test_return_ns_triplet() for our example parse to improve
 * coverage of tidying up code executed.
 */
START_TEST(test_ns_parser_reset)
{
    XML_ParsingStatus status;

    XML_GetParsingStatus(parser, &status);
    if (status.parsing != XML_INITIALIZED)
        fail("parsing status doesn't start INITIALIZED");
    test_return_ns_triplet();
    XML_GetParsingStatus(parser, &status);
    if (status.parsing != XML_FINISHED)
        fail("parsing status doesn't end FINISHED");
    XML_ParserReset(parser, NULL);
    XML_GetParsingStatus(parser, &status);
    if (status.parsing != XML_INITIALIZED)
        fail("parsing status doesn't reset to INITIALIZED");
}
END_TEST

/* Control variable; the number of times duff_allocator() will successfully allocate */
static unsigned int allocation_count = 0;

/* Crocked allocator for allocation failure tests */
static void *duff_allocator(size_t size)
{
    if (allocation_count == 0)
        return NULL;
    allocation_count--;
    return malloc(size);
}

/* Test that a failure to allocate the parser structure fails gracefully */
START_TEST(test_misc_alloc_create_parser)
{
    XML_Memory_Handling_Suite memsuite = { duff_allocator, realloc, free };
    unsigned int i;

    /* Something this simple shouldn't need more than 10 allocations */
    for (i = 0; i < 10; i++)
    {
        allocation_count = i;
        parser = XML_ParserCreate_MM(NULL, &memsuite, NULL);
        if (parser != NULL)
            break;
    }
    if (i == 0)
        fail("Parser unexpectedly ignored failing allocator");
    else if (i == 10)
        fail("Parser not created with allocation count 10");
}
END_TEST

/* Test memory allocation failures for a parser with an encoding */
START_TEST(test_misc_alloc_create_parser_with_encoding)
{
    XML_Memory_Handling_Suite memsuite = { duff_allocator, realloc, free };
    unsigned int i;

    /* Try several levels of allocation */
    for (i = 0; i < 10; i++) {
        allocation_count = i;
        parser = XML_ParserCreate_MM("us-ascii", &memsuite, NULL);
        if (parser != NULL)
            break;
    }
    if (i == 0)
        fail("Parser ignored failing allocator");
    else if (i == 10)
        fail("Parser not created with allocation count 10");
}
END_TEST

/* Test the effects of allocation failure in simple namespace parsing.
 * Based on test_ns_default_with_empty_uri()
 */
START_TEST(test_misc_alloc_ns)
{
    XML_Memory_Handling_Suite memsuite = { duff_allocator, realloc, free };
    const char *text =
        "<doc xmlns='http://xml.libexpat.org/'>\n"
        "  <e xmlns=''/>\n"
        "</doc>";
    unsigned int i;
    int repeated = 0;
    XML_Char ns_sep[2] = { ' ', '\0' };

    allocation_count = 10000;
    parser = XML_ParserCreate_MM(NULL, &memsuite, ns_sep);
    if (parser == NULL) {
        fail("Parser not created");
    } else {
        for (i = 0; i < 10; i++) {
            /* Repeat some tests with the same allocation count to
             * catch cached allocations not freed by XML_ParserReset()
             */
            if (repeated < 2 && i == 3) {
                i--;
                repeated++;
            }
            if (repeated == 2 && i == 5) {
                i = 3;
                repeated++;
            }
            allocation_count = i;
            if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) != XML_STATUS_ERROR)
                break;
            XML_ParserReset(parser, NULL);
        }
        if (i == 0)
            fail("Parsing worked despite failing allocations");
        else if (i == 10)
            fail("Parsing failed even at allocation count 10");
    }
}
END_TEST

/* Test XML_ParseBuffer interface with namespace and a dicky allocator */
START_TEST(test_misc_alloc_ns_parse_buffer)
{
    XML_Memory_Handling_Suite memsuite = { duff_allocator, realloc, free };
    XML_Char ns_sep[2] = { ' ', '\0' };
    const char *text = "<doc>Hello</doc>";
    void *buffer;

    /* Make sure the basic parser is allocated */
    allocation_count = 10000;
    parser = XML_ParserCreate_MM(NULL, &memsuite, ns_sep);
    if (parser == NULL)
        fail("Parser not created");

    /* Try a parse before the start of the world */
    /* (Exercises new code path) */
    allocation_count = 0;
    if (XML_ParseBuffer(parser, 0, XML_FALSE) != XML_STATUS_ERROR)
        fail("Pre-init XML_ParseBuffer not faulted");
    if (XML_GetErrorCode(parser) != XML_ERROR_NO_MEMORY)
        fail("Pre-init XML_ParseBuffer faulted for wrong reason");

    /* Now with actual memory allocation */
    allocation_count = 10000;
    if (XML_ParseBuffer(parser, 0, XML_FALSE) != XML_STATUS_OK)
        xml_failure(parser);

    /* Check that resuming an unsuspended parser is faulted */
    if (XML_ResumeParser(parser) != XML_STATUS_ERROR)
        fail("Resuming unsuspended parser not faulted");
    if (XML_GetErrorCode(parser) != XML_ERROR_NOT_SUSPENDED)
        xml_failure(parser);

    /* Get the parser into suspended state */
    XML_SetCharacterDataHandler(parser, clearing_aborting_character_handler);
    resumable = XML_TRUE;
    buffer = XML_GetBuffer(parser, strlen(text));
    if (buffer == NULL)
        fail("Could not acquire parse buffer");
    memcpy(buffer, text, strlen(text));
    if (XML_ParseBuffer(parser, strlen(text),
                        XML_TRUE) != XML_STATUS_SUSPENDED)
        xml_failure(parser);
    if (XML_GetErrorCode(parser) != XML_ERROR_NONE)
        xml_failure(parser);
    if (XML_ParseBuffer(parser, strlen(text), XML_TRUE) != XML_STATUS_ERROR)
        fail("Suspended XML_ParseBuffer not faulted");
    if (XML_GetErrorCode(parser) != XML_ERROR_SUSPENDED)
        xml_failure(parser);
    if (XML_GetBuffer(parser, strlen(text)) != NULL)
        fail("Suspended XML_GetBuffer not faulted");

    /* Get it going again and complete the world */
    XML_SetCharacterDataHandler(parser, NULL);
    if (XML_ResumeParser(parser) != XML_STATUS_OK)
        xml_failure(parser);
    if (XML_ParseBuffer(parser, strlen(text), XML_TRUE) != XML_STATUS_ERROR)
        fail("Post-finishing XML_ParseBuffer not faulted");
    if (XML_GetErrorCode(parser) != XML_ERROR_FINISHED)
        xml_failure(parser);
    if (XML_GetBuffer(parser, strlen(text)) != NULL)
        fail("Post-finishing XML_GetBuffer not faulted");
}
END_TEST

/* Test that freeing a NULL parser doesn't cause an explosion.
 * (Not actually tested anywhere else)
 */
START_TEST(test_misc_null_parser)
{
    XML_ParserFree(NULL);
}
END_TEST

/* Test that XML_ErrorString rejects out-of-range codes */
START_TEST(test_misc_error_string)
{
    if (XML_ErrorString((enum XML_Error)-1) != NULL)
        fail("Negative error code not rejected");
    if (XML_ErrorString((enum XML_Error)100) != NULL)
        fail("Large error code not rejected");
}
END_TEST

/* Test the version information is consistent */
START_TEST(test_misc_version)
{
    XML_Expat_Version version_struct = XML_ExpatVersionInfo();
    const XML_LChar *version_text = XML_ExpatVersion();
    long value;
    const char *p;
    char *endp;

    if (version_text == NULL)
        fail("Could not obtain version text");
    for (p = version_text; *p != '\0'; p++)
        if (isdigit(*p))
            break;
    if (*p == '\0')
        fail("No numbers in version text");
    value = strtoul(p, &endp, 10);
    if (*endp != '.')
        fail("Major version conversion from text failed");
    if (value != version_struct.major)
        fail("Major version mismatch");
    p = endp + 1;
    value = strtoul(p, &endp, 10);
    if (*endp != '.')
        fail("Minor version conversion from text failed");
    if (value != version_struct.minor)
        fail("Minor version mismatch");
    p = endp + 1;
    value = strtoul(p, &endp, 10);
    if (*endp != '\0')
        fail("Micro version conversion from text failed");
    if (value != version_struct.micro)
        fail("Micro version mismatch");
}
END_TEST

/* Regression test for GitHub Issue #17: memory leak parsing attribute
 * values with mixed bound and unbound namespaces.
 */
START_TEST(test_misc_attribute_leak)
{
    const char *text = "<D xmlns:L=\"D\" l:a='' L:a=''/>";
    XML_Memory_Handling_Suite memsuite = {
        tracking_malloc,
        tracking_realloc,
        tracking_free
    };

    parser = XML_ParserCreate_MM("UTF-8", &memsuite, "\n");
    expect_failure(text, XML_ERROR_UNBOUND_PREFIX,
                   "Unbound prefixes not found");
    XML_ParserFree(parser);
    /* Prevent the teardown trying to double free */
    parser = NULL;

    if (!tracking_report())
        fail("Memory leak found");
}
END_TEST


static void
alloc_setup(void)
{
    XML_Memory_Handling_Suite memsuite = { duff_allocator, realloc, free };

    /* Ensure the parser creation will go through */
    allocation_count = 10000;
    parser = XML_ParserCreate_MM(NULL, &memsuite, NULL);
    if (parser == NULL)
        fail("Parser not created");
}

static void
alloc_teardown(void)
{
    basic_teardown();
}

static int XMLCALL
external_entity_duff_loader(XML_Parser parser,
                            const XML_Char *context,
                            const XML_Char *UNUSED_P(base),
                            const XML_Char *UNUSED_P(systemId),
                            const XML_Char *UNUSED_P(publicId))
{
    XML_Parser new_parser;
    unsigned int i;

    /* Try a few different allocation levels */
    for (i = 0; i < 10; i++)
    {
        allocation_count = i;
        new_parser = XML_ExternalEntityParserCreate(parser, context, NULL);
        if (new_parser != NULL)
        {
            XML_ParserFree(new_parser);
            break;
        }
    }
    if (i == 0)
        fail("External parser creation ignored failing allocator");
    else if (i == 10)
        fail("Extern parser not created with allocation count 10");

    /* Make sure other random allocation doesn't now fail */
    allocation_count = 10000;

    /* Make sure the failure code path is executed too */
    return XML_STATUS_ERROR;
}

/* Test that external parser creation running out of memory is
 * correctly reported.  Based on the external entity test cases.
 */
START_TEST(test_alloc_create_external_parser)
{
    const char *text =
        "<?xml version='1.0' encoding='us-ascii'?>\n"
        "<!DOCTYPE doc SYSTEM 'foo'>\n"
        "<doc>&entity;</doc>";
    char foo_text[] =
        "<!ELEMENT doc (#PCDATA)*>";

    XML_SetParamEntityParsing(parser, XML_PARAM_ENTITY_PARSING_ALWAYS);
    XML_SetUserData(parser, foo_text);
    XML_SetExternalEntityRefHandler(parser,
                                    external_entity_duff_loader);
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) != XML_STATUS_ERROR) {
        fail("External parser allocator returned success incorrectly");
    }
}
END_TEST

static int XMLCALL
external_entity_null_loader(XML_Parser UNUSED_P(parser),
                            const XML_Char *UNUSED_P(context),
                            const XML_Char *UNUSED_P(base),
                            const XML_Char *UNUSED_P(systemId),
                            const XML_Char *UNUSED_P(publicId))
{
    return XML_STATUS_OK;
}

/* More external parser memory allocation testing */
START_TEST(test_alloc_run_external_parser)
{
    const char *text =
        "<?xml version='1.0' encoding='us-ascii'?>\n"
        "<!DOCTYPE doc SYSTEM 'foo'>\n"
        "<doc>&entity;</doc>";
    char foo_text[] =
        "<!ELEMENT doc (#PCDATA)*>";
    unsigned int i;

    for (i = 0; i < 10; i++) {
        XML_SetParamEntityParsing(parser,
                                  XML_PARAM_ENTITY_PARSING_ALWAYS);
        XML_SetUserData(parser, foo_text);
        XML_SetExternalEntityRefHandler(parser,
                                        external_entity_null_loader);
        allocation_count = i;
        if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) != XML_STATUS_ERROR)
            break;
        /* Re-use the parser */
        XML_ParserReset(parser, NULL);
    }
    if (i == 0)
        fail("Parsing ignored failing allocator");
    else if (i == 10)
        fail("Parsing failed with allocation count 10");
}
END_TEST


static int XMLCALL
external_entity_dbl_handler(XML_Parser parser,
                            const XML_Char *context,
                            const XML_Char *UNUSED_P(base),
                            const XML_Char *UNUSED_P(systemId),
                            const XML_Char *UNUSED_P(publicId))
{
    intptr_t callno = (intptr_t)XML_GetUserData(parser);
    const char *text;
    XML_Parser new_parser;
    int i;

    if (callno == 0) {
        /* First time through, check how many calls to malloc occur */
        text = ("<!ELEMENT doc (e+)>\n"
                "<!ATTLIST doc xmlns CDATA #IMPLIED>\n"
                "<!ELEMENT e EMPTY>\n");
        allocation_count = 10000;
        new_parser = XML_ExternalEntityParserCreate(parser, context, NULL);
        if (new_parser == NULL) {
            fail("Unable to allocate first external parser");
            return 0;
        }
        /* Stash the number of calls in the user data */
        XML_SetUserData(parser, (void *)(intptr_t)(10000 - allocation_count));
    } else {
        text = ("<?xml version='1.0' encoding='us-ascii'?>"
                "<e/>");
        /* Try at varying levels to exercise more code paths */
        for (i = 0; i < 20; i++) {
            allocation_count = callno + i;
            new_parser = XML_ExternalEntityParserCreate(parser,
                                                        context,
                                                        NULL);
            if (new_parser != NULL)
                break;
        }
        if (i == 0) {
            fail("Second external parser unexpectedly created");
            XML_ParserFree(new_parser);
            return 0;
        }
        else if (i == 20) {
            fail("Second external parser not created");
            return 0;
        }
    }

    allocation_count = 10000;
    if (_XML_Parse_SINGLE_BYTES(new_parser, text, strlen(text), XML_TRUE) == XML_STATUS_ERROR) {
        xml_failure(new_parser);
        return 0;
    }
    XML_ParserFree(new_parser);
    return 1;
}

/* Test that running out of memory in dtdCopy is correctly reported.
 * Based on test_default_ns_from_ext_subset_and_ext_ge()
 */
START_TEST(test_alloc_dtd_copy_default_atts)
{
    const char *text =
        "<?xml version='1.0'?>\n"
        "<!DOCTYPE doc SYSTEM 'http://xml.libexpat.org/doc.dtd' [\n"
        "  <!ENTITY en SYSTEM 'http://xml.libexpat.org/entity.ent'>\n"
        "]>\n"
        "<doc xmlns='http://xml.libexpat.org/ns1'>\n"
        "&en;\n"
        "</doc>";

    XML_SetParamEntityParsing(parser, XML_PARAM_ENTITY_PARSING_ALWAYS);
    XML_SetExternalEntityRefHandler(parser,
                                    external_entity_dbl_handler);
    XML_SetUserData(parser, NULL);
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
}
END_TEST


static int XMLCALL
external_entity_dbl_handler_2(XML_Parser parser,
                              const XML_Char *context,
                              const XML_Char *UNUSED_P(base),
                              const XML_Char *UNUSED_P(systemId),
                              const XML_Char *UNUSED_P(publicId))
{
    intptr_t callno = (intptr_t)XML_GetUserData(parser);
    const char *text;
    XML_Parser new_parser;
    int i;

    if (callno == 0) {
        /* Try different allocation levels for whole exercise */
        text = ("<!ELEMENT doc (e+)>\n"
                "<!ATTLIST doc xmlns CDATA #IMPLIED>\n"
                "<!ELEMENT e EMPTY>\n");
        XML_SetUserData(parser, (void *)(intptr_t)1);
        for (i = 0; i < 20; i++) {
            allocation_count = i;
            new_parser = XML_ExternalEntityParserCreate(parser,
                                                        context,
                                                        NULL);
            if (new_parser == NULL)
                continue;
            if (_XML_Parse_SINGLE_BYTES(new_parser, text, strlen(text),
                                        XML_TRUE) != XML_STATUS_ERROR)
                break;
            XML_ParserFree(new_parser);
        }

        /* Ensure future allocations will be well */
        allocation_count = 10000;
        if (i == 0) {
            fail("first external parser unexpectedly created");
            XML_ParserFree(new_parser);
            return 0;
        }
        else if (i == 20) {
            fail("first external parser not allocated with count 20");
            return 0;
        }
    } else {
        /* Just run through once */
        text = ("<?xml version='1.0' encoding='us-ascii'?>"
                "<e/>");
        allocation_count = 10000;
        new_parser = XML_ExternalEntityParserCreate(parser, context, NULL);
        if (new_parser == NULL) {
            fail("Unable to create second external parser");
            return 0;
        }
        if (_XML_Parse_SINGLE_BYTES(new_parser, text, strlen(text),
                                    XML_TRUE) == XML_STATUS_ERROR) {
            xml_failure(new_parser);
            XML_ParserFree(new_parser);
            return 0;
        }
    }
    XML_ParserFree(new_parser);
    return 1;
}

/* Test more external entity allocation failure paths */
START_TEST(test_alloc_external_entity)
{
    const char *text =
        "<?xml version='1.0'?>\n"
        "<!DOCTYPE doc SYSTEM 'http://xml.libexpat.org/doc.dtd' [\n"
        "  <!ENTITY en SYSTEM 'http://xml.libexpat.org/entity.ent'>\n"
        "]>\n"
        "<doc xmlns='http://xml.libexpat.org/ns1'>\n"
        "&en;\n"
        "</doc>";

    XML_SetParamEntityParsing(parser, XML_PARAM_ENTITY_PARSING_ALWAYS);
    XML_SetExternalEntityRefHandler(parser,
                                    external_entity_dbl_handler_2);
    XML_SetUserData(parser, NULL);
    if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text),
                                XML_TRUE) == XML_STATUS_ERROR)
        xml_failure(parser);
}
END_TEST


static int XMLCALL
unknown_released_encoding_handler(void *UNUSED_P(data),
                                  const XML_Char *encoding,
                                  XML_Encoding *info)
{
    if (!strcmp(encoding, "unsupported-encoding")) {
        int i;

        for (i = 0; i < 256; i++)
            info->map[i] = i;
        info->data = NULL;
        info->convert = NULL;
        info->release = dummy_release;
        return XML_STATUS_OK;
    }
    return XML_STATUS_ERROR;
}

/* Test the effects of allocation failure in internal entities.
 * Based on test_unknown_encoding_internal_entity
 */
START_TEST(test_alloc_internal_entity)
{
    const char *text =
        "<?xml version='1.0' encoding='unsupported-encoding'?>\n"
        "<!DOCTYPE test [<!ENTITY foo 'bar'>]>\n"
        "<test a='&foo;'/>";
    unsigned int i;
    int repeated = 0;

    for (i = 0; i < 10; i++) {
        /* Again, repeat some counts to account for caching */
        if (repeated < 2 && i == 2) {
            i--;
            repeated++;
        }
        XML_SetUnknownEncodingHandler(parser,
                                      unknown_released_encoding_handler,
                                      NULL);
        allocation_count = i;
        if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text), XML_TRUE) != XML_STATUS_ERROR)
            break;
        XML_ParserReset(parser, NULL);
    }
    if (i == 0)
        fail("Internal entity worked despite failing allocations");
    else if (i == 10)
        fail("Internal entity failed at allocation count 10");
}
END_TEST


/* Test the robustness against allocation failure of element handling
 * Based on test_dtd_default_handling().
 */
START_TEST(test_alloc_dtd_default_handling)
{
    const char *text =
        "<!DOCTYPE doc [\n"
        "<!ENTITY e SYSTEM 'http://xml.libexpat.org/e'>\n"
        "<!NOTATION n SYSTEM 'http://xml.libexpat.org/n'>\n"
        "<!ELEMENT doc EMPTY>\n"
        "<!ATTLIST doc a CDATA #IMPLIED>\n"
        "<?pi in dtd?>\n"
        "<!--comment in dtd-->\n"
        "]><doc/>";
    const char *expected = "\n\n\n\n\n\n\n<doc/>";
    CharData storage;
    int i;
    int repeat = 0;

    for (i = 0; i < 10; i++) {
        /* Repeat some counts to catch cached allocations */
        if ((repeat < 4 && i == 2) ||
            (repeat == 4 && i == 3)) {
            i--;
            repeat++;
        }
        allocation_count = i;
        XML_SetDefaultHandler(parser, accumulate_characters);
        XML_SetDoctypeDeclHandler(parser,
                                  dummy_start_doctype_handler,
                                  dummy_end_doctype_handler);
        XML_SetEntityDeclHandler(parser, dummy_entity_decl_handler);
        XML_SetNotationDeclHandler(parser, dummy_notation_decl_handler);
        XML_SetElementDeclHandler(parser, dummy_element_decl_handler);
        XML_SetAttlistDeclHandler(parser, dummy_attlist_decl_handler);
        XML_SetProcessingInstructionHandler(parser, dummy_pi_handler);
        XML_SetCommentHandler(parser, dummy_comment_handler);
        XML_SetCdataSectionHandler(parser,
                                   dummy_start_cdata_handler,
                                   dummy_end_cdata_handler);
        XML_SetUnparsedEntityDeclHandler(
            parser,
            dummy_unparsed_entity_decl_handler);
        CharData_Init(&storage);
        XML_SetUserData(parser, &storage);
        XML_SetCharacterDataHandler(parser, accumulate_characters);
        if (_XML_Parse_SINGLE_BYTES(parser, text, strlen(text),
                                    XML_TRUE) != XML_STATUS_ERROR)
            break;
        XML_ParserReset(parser, NULL);
    }
    if (i == 0) {
        fail("Default DTD parsed despite allocation failures");
    } else if (i == 10) {
        fail("Default DTD not parsed with alloc count 10");
    } else {
        CharData_CheckXMLChars(&storage, expected);
    }
}
END_TEST

/* Test robustness of XML_SetEncoding() with a failing allocator */
START_TEST(test_alloc_explicit_encoding)
{
    int i;

    for (i = 0; i < 5; i++) {
        allocation_count = i;
        if (XML_SetEncoding(parser, "us-ascii") == XML_STATUS_OK)
            break;
    }
    if (i == 0)
        fail("Encoding set despite failing allocator");
    else if (i == 5)
        fail("Encoding not set at allocation count 5");
}
END_TEST

/* Test robustness of XML_SetBase against a failing allocator */
START_TEST(test_alloc_set_base)
{
    const XML_Char *new_base = "/local/file/name.xml";
    int i;

    for (i = 0; i < 5; i++) {
        allocation_count = i;
        if (XML_SetBase(parser, new_base) == XML_STATUS_OK)
            break;
    }
    if (i == 0)
        fail("Base set despite failing allocator");
    else if (i == 5)
        fail("Base not set with allocation count 5");
}
END_TEST


static Suite *
make_suite(void)
{
    Suite *s = suite_create("basic");
    TCase *tc_basic = tcase_create("basic tests");
    TCase *tc_namespace = tcase_create("XML namespaces");
    TCase *tc_misc = tcase_create("miscellaneous tests");
    TCase *tc_alloc = tcase_create("allocation tests");

    suite_add_tcase(s, tc_basic);
    tcase_add_checked_fixture(tc_basic, basic_setup, basic_teardown);
    tcase_add_test(tc_basic, test_nul_byte);
    tcase_add_test(tc_basic, test_u0000_char);
    tcase_add_test(tc_basic, test_siphash_self);
    tcase_add_test(tc_basic, test_siphash_spec);
    tcase_add_test(tc_basic, test_bom_utf8);
    tcase_add_test(tc_basic, test_bom_utf16_be);
    tcase_add_test(tc_basic, test_bom_utf16_le);
    tcase_add_test(tc_basic, test_illegal_utf8);
    tcase_add_test(tc_basic, test_utf8_auto_align);
    tcase_add_test(tc_basic, test_utf16);
    tcase_add_test(tc_basic, test_utf16_le_epilog_newline);
    tcase_add_test(tc_basic, test_latin1_umlauts);
    /* Regression test for SF bug #491986. */
    tcase_add_test(tc_basic, test_danish_latin1);
    /* Regression test for SF bug #514281. */
    tcase_add_test(tc_basic, test_french_charref_hexidecimal);
    tcase_add_test(tc_basic, test_french_charref_decimal);
    tcase_add_test(tc_basic, test_french_latin1);
    tcase_add_test(tc_basic, test_french_utf8);
    tcase_add_test(tc_basic, test_utf8_false_rejection);
    tcase_add_test(tc_basic, test_line_number_after_parse);
    tcase_add_test(tc_basic, test_column_number_after_parse);
    tcase_add_test(tc_basic, test_line_and_column_numbers_inside_handlers);
    tcase_add_test(tc_basic, test_line_number_after_error);
    tcase_add_test(tc_basic, test_column_number_after_error);
    tcase_add_test(tc_basic, test_really_long_lines);
    tcase_add_test(tc_basic, test_end_element_events);
    tcase_add_test(tc_basic, test_attr_whitespace_normalization);
    tcase_add_test(tc_basic, test_xmldecl_misplaced);
    tcase_add_test(tc_basic, test_unknown_encoding_internal_entity);
    tcase_add_test(tc_basic, test_unrecognised_encoding_internal_entity);
    tcase_add_test(tc_basic,
                   test_wfc_undeclared_entity_unread_external_subset);
    tcase_add_test(tc_basic, test_wfc_undeclared_entity_no_external_subset);
    tcase_add_test(tc_basic, test_wfc_undeclared_entity_standalone);
    tcase_add_test(tc_basic, test_wfc_undeclared_entity_with_external_subset);
    tcase_add_test(tc_basic, test_not_standalone_handler_reject);
    tcase_add_test(tc_basic, test_not_standalone_handler_accept);
    tcase_add_test(tc_basic,
                   test_wfc_undeclared_entity_with_external_subset_standalone);
    tcase_add_test(tc_basic, test_wfc_no_recursive_entity_refs);
    tcase_add_test(tc_basic, test_ext_entity_set_encoding);
    tcase_add_test(tc_basic, test_dtd_default_handling);
    tcase_add_test(tc_basic, test_empty_ns_without_namespaces);
    tcase_add_test(tc_basic, test_ns_in_attribute_default_without_namespaces);
    tcase_add_test(tc_basic, test_stop_parser_between_char_data_calls);
    tcase_add_test(tc_basic, test_suspend_parser_between_char_data_calls);
    tcase_add_test(tc_basic, test_repeated_stop_parser_between_char_data_calls);
    tcase_add_test(tc_basic, test_good_cdata_ascii);
    tcase_add_test(tc_basic, test_good_cdata_utf16);
    tcase_add_test(tc_basic, test_bad_cdata);
    tcase_add_test(tc_basic, test_memory_allocation);
    tcase_add_test(tc_basic, test_default_current);
    tcase_add_test(tc_basic, test_dtd_elements);
    tcase_add_test(tc_basic, test_set_foreign_dtd);
    tcase_add_test(tc_basic, test_set_base);
    tcase_add_test(tc_basic, test_attributes);
    tcase_add_test(tc_basic, test_reset_in_entity);
    tcase_add_test(tc_basic, test_resume_invalid_parse);
    tcase_add_test(tc_basic, test_resume_resuspended);
    tcase_add_test(tc_basic, test_subordinate_reset);
    tcase_add_test(tc_basic, test_subordinate_suspend);
    tcase_add_test(tc_basic, test_explicit_encoding);
    tcase_add_test(tc_basic, test_user_parameters);
    tcase_add_test(tc_basic, test_ext_entity_ref_parameter);
    tcase_add_test(tc_basic, test_empty_parse);
    tcase_add_test(tc_basic, test_get_buffer_1);
    tcase_add_test(tc_basic, test_get_buffer_2);
    tcase_add_test(tc_basic, test_byte_info_at_end);
    tcase_add_test(tc_basic, test_byte_info_at_error);
    tcase_add_test(tc_basic, test_byte_info_at_cdata);
    tcase_add_test(tc_basic, test_invalid_tag_in_dtd);

    suite_add_tcase(s, tc_namespace);
    tcase_add_checked_fixture(tc_namespace,
                              namespace_setup, namespace_teardown);
    tcase_add_test(tc_namespace, test_return_ns_triplet);
    tcase_add_test(tc_namespace, test_ns_tagname_overwrite);
    tcase_add_test(tc_namespace, test_ns_tagname_overwrite_triplet);
    tcase_add_test(tc_namespace, test_start_ns_clears_start_element);
    tcase_add_test(tc_namespace, test_default_ns_from_ext_subset_and_ext_ge);
    tcase_add_test(tc_namespace, test_ns_prefix_with_empty_uri_1);
    tcase_add_test(tc_namespace, test_ns_prefix_with_empty_uri_2);
    tcase_add_test(tc_namespace, test_ns_prefix_with_empty_uri_3);
    tcase_add_test(tc_namespace, test_ns_prefix_with_empty_uri_4);
    tcase_add_test(tc_namespace, test_ns_default_with_empty_uri);
    tcase_add_test(tc_namespace, test_ns_duplicate_attrs_diff_prefixes);
    tcase_add_test(tc_namespace, test_ns_unbound_prefix_on_attribute);
    tcase_add_test(tc_namespace, test_ns_unbound_prefix_on_element);
    tcase_add_test(tc_namespace, test_ns_parser_reset);

    suite_add_tcase(s, tc_misc);
    tcase_add_checked_fixture(tc_misc, NULL, basic_teardown);
    tcase_add_test(tc_misc, test_misc_alloc_create_parser);
    tcase_add_test(tc_misc, test_misc_alloc_create_parser_with_encoding);
    tcase_add_test(tc_misc, test_misc_alloc_ns);
    tcase_add_test(tc_misc, test_misc_null_parser);
    tcase_add_test(tc_misc, test_misc_alloc_ns_parse_buffer);
    tcase_add_test(tc_misc, test_misc_error_string);
    tcase_add_test(tc_misc, test_misc_version);
    tcase_add_test(tc_misc, test_misc_attribute_leak);

    suite_add_tcase(s, tc_alloc);
    tcase_add_checked_fixture(tc_alloc, alloc_setup, alloc_teardown);
    tcase_add_test(tc_alloc, test_alloc_create_external_parser);
    tcase_add_test(tc_alloc, test_alloc_run_external_parser);
    tcase_add_test(tc_alloc, test_alloc_dtd_copy_default_atts);
    tcase_add_test(tc_alloc, test_alloc_external_entity);
    tcase_add_test(tc_alloc, test_alloc_internal_entity);
    tcase_add_test(tc_alloc, test_alloc_dtd_default_handling);
    tcase_add_test(tc_alloc, test_alloc_explicit_encoding);
    tcase_add_test(tc_alloc, test_alloc_set_base);

    return s;
}


int
main(int argc, char *argv[])
{
    int i, nf;
    int verbosity = CK_NORMAL;
    Suite *s = make_suite();
    SRunner *sr = srunner_create(s);

    /* run the tests for internal helper functions */
    testhelper_is_whitespace_normalized();

    for (i = 1; i < argc; ++i) {
        char *opt = argv[i];
        if (strcmp(opt, "-v") == 0 || strcmp(opt, "--verbose") == 0)
            verbosity = CK_VERBOSE;
        else if (strcmp(opt, "-q") == 0 || strcmp(opt, "--quiet") == 0)
            verbosity = CK_SILENT;
        else {
            fprintf(stderr, "runtests: unknown option '%s'\n", opt);
            return 2;
        }
    }
    if (verbosity != CK_SILENT)
        printf("Expat version: %s\n", XML_ExpatVersion());
    srunner_run_all(sr, verbosity);
    nf = srunner_ntests_failed(sr);
    srunner_free(sr);

    return (nf == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}
