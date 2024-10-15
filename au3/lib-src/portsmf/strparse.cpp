#include <string>
#include <cstring>
// #include <iostream>  -- for debugging (cout)
#include "ctype.h"
using namespace std;
#include "strparse.h"

void String_parse::skip_space()
{
    while ((*str)[pos] && isspace((*str)[pos])) {
        pos = pos + 1;
    }
}


char String_parse::peek()
{
    return (*str)[pos];
}


void String_parse::get_nonspace_quoted(string &field)
{
    field.clear();
    skip_space();
    bool quoted = false;
    if ((*str)[pos] == '"') {
        quoted = true;
        field.append(1, '"');
        pos = pos + 1;
    }
    while ((*str)[pos] && (quoted || !isspace((*str)[pos]))) {
        if ((*str)[pos] == '"') {
            if (quoted) {
                field.append(1, '"');
                pos = pos + 1;
            }
            return;
        }
        if ((*str)[pos] == '\\') {
            pos = pos + 1;
        }
        if ((*str)[pos]) {
            field.append(1, (*str)[pos]);
            pos = pos + 1;    
        }
    }
}


static const char *const escape_chars[] = {"\\n", "\\t", "\\\\", "\\r", "\\\""};


void string_escape(string &result, const char *str, const char *quote)
{
    int length = (int) strlen(str);
    if (quote[0]) {
        result.append(1, quote[0]);
    }
    for (int i = 0; i < length; i++) {
        if (!isalnum((unsigned char) str[i])) {
            const char *const chars = "\n\t\\\r\"";
            const char *const special = strchr(chars, str[i]);
            if (special) {
                result.append(escape_chars[special - chars]);
            } else {
                result.append(1, str[i]);
            }
        } else {
            result.append(1, str[i]);
        }
    }
    result.append(1, quote[0]);
}

void String_parse::get_remainder(std::string &field)
{
    field.clear();
    skip_space();
    int len = str->length() - pos;
    if ((len > 0) && ((*str)[len - 1] == '\n')) { // if str ends in newline, 
        len--; // reduce length to ignore newline
    }
    field.insert(0, *str, pos, len);
}

    
