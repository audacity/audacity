# UTF8-CPP: UTF-8 with C++ in a Portable Way


## Introduction

C++ developers miss an easy and portable way of handling Unicode encoded strings. The original C++ Standard (known as C++98 or C++03) is Unicode agnostic. C++11 provides some support for Unicode on core language and library level: u8, u, and U character and string literals, char16_t and char32_t character types, u16string and u32string library classes, and codecvt support for conversions between Unicode encoding forms. In the meantime, developers use third party libraries like ICU, OS specific capabilities, or simply roll out their own solutions.

In order to easily handle UTF-8 encoded Unicode strings, I came up with a small, C++98 compatible generic library. For anybody used to work with STL algorithms and iterators, it should be easy and natural to use. The code is freely available for any purpose - check out the [license](./LICENSE). The library has been used a lot in the past ten years both in commercial and open-source projects and is considered feature-complete now. If you run into bugs or performance issues, please let me know and I'll do my best to address them.

The purpose of this article is not to offer an introduction to Unicode in general, and UTF-8 in particular. If you are not familiar with Unicode, be sure to check out [Unicode Home Page](http://www.unicode.org/) or some other source of information for Unicode. Also, it is not my aim to advocate the use of UTF-8 encoded strings in C++ programs; if you want to handle UTF-8 encoded strings from C++, I am sure you have good reasons for it.

## Examples of use

### Introductionary Sample

To illustrate the use of the library, let's start with a small but complete program that opens a file containing UTF-8 encoded text, reads it line by line, checks each line for invalid UTF-8 byte sequences, and converts it to UTF-16 encoding and back to UTF-8:

```cpp
#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include "utf8.h"
using namespace std;
int main(int argc, char** argv)
{
    if (argc != 2) {
        cout << "\nUsage: docsample filename\n";
        return 0;
    }
    const char* test_file_path = argv[1];
    // Open the test file (must be UTF-8 encoded)
    ifstream fs8(test_file_path);
    if (!fs8.is_open()) {
        cout << "Could not open " << test_file_path << endl;
        return 0;
    }

    unsigned line_count = 1;
    string line;
    // Play with all the lines in the file
    while (getline(fs8, line)) {
        // check for invalid utf-8 (for a simple yes/no check, there is also utf8::is_valid function)
#if __cplusplus >= 201103L // C++ 11 or later
        auto end_it = utf8::find_invalid(line.begin(), line.end());
#else
        string::iterator end_it = utf8::find_invalid(line.begin(), line.end());
#endif // C++ 11
        if (end_it != line.end()) {
            cout << "Invalid UTF-8 encoding detected at line " << line_count << "\n";
            cout << "This part is fine: " << string(line.begin(), end_it) << "\n";
        }
        // Get the line length (at least for the valid part)
        int length = utf8::distance(line.begin(), end_it);
        cout << "Length of line " << line_count << " is " << length <<  "\n";

        // Convert it to utf-16
#if __cplusplus >= 201103L // C++ 11 or later
        u16string utf16line = utf8::utf8to16(line);
#else
        vector<unsigned short> utf16line;
        utf8::utf8to16(line.begin(), end_it, back_inserter(utf16line));
#endif // C++ 11
        // And back to utf-8;
#if __cplusplus >= 201103L // C++ 11 or later
        string utf8line = utf8::utf16to8(utf16line);
#else
        string utf8line; 
        utf8::utf16to8(utf16line.begin(), utf16line.end(), back_inserter(utf8line));
#endif // C++ 11
        // Confirm that the conversion went OK:
        if (utf8line != string(line.begin(), end_it))
            cout << "Error in UTF-16 conversion at line: " << line_count << "\n";        

        line_count++;
    } 

    return 0;
}
```

In the previous code sample, for each line we performed a detection of invalid UTF-8 sequences with `find_invalid`; the number of characters (more precisely - the number of Unicode code points, including the end of line and even BOM if there is one) in each line was determined with a use of `utf8::distance`; finally, we have converted each line to UTF-16 encoding with `utf8to16` and back to UTF-8 with `utf16to8`.

Note a different pattern of usage for old compilers. For instance, this is how we convert
a UTF-8 encoded string to a UTF-16 encoded one with a pre - C++11 compiler:
```cpp
    vector<unsigned short> utf16line;
    utf8::utf8to16(line.begin(), end_it, back_inserter(utf16line));
```

With a more modern compiler, the same operation would look like:
```cpp
    u16string utf16line = utf8::utf8to16(line);
```
If `__cplusplus` macro points to a C++ 11 or later, the library exposes API that takes into 
account C++ standard Unicode strings and move semantics. With an older compiler, it is still
possible to use the same functionality, just in a little less convenient way

In case you do not trust the `__cplusplus` macro or, for instance, do not want to include
the C++ 11 helper functions even with a modern compiler, define `UTF_CPP_CPLUSPLUS` macro
before including `utf8.h` and assign it a value for the standard you want to use - the values are the same as for the `__cplusplus` macro. This can be also useful with compilers that are conservative in setting the `__cplusplus` macro even if they have a good support for a recent standard edition - Microsoft's Visual C++ is one example.

### Checking if a file contains valid UTF-8 text

Here is a function that checks whether the content of a file is valid UTF-8 encoded text without reading the content into the memory:

```cpp
bool valid_utf8_file(const char* file_name)
{
    ifstream ifs(file_name);
    if (!ifs)
        return false; // even better, throw here

    istreambuf_iterator<char> it(ifs.rdbuf());
    istreambuf_iterator<char> eos;

    return utf8::is_valid(it, eos);
}
```

Because the function `utf8::is_valid()` works with input iterators, we were able to pass an `istreambuf_iterator` to `it` and read the content of the file directly without loading it to the memory first.

Note that other functions that take input iterator arguments can be used in a similar way. For instance, to read the content of a UTF-8 encoded text file and convert the text to UTF-16, just do something like:

```cpp
    utf8::utf8to16(it, eos, back_inserter(u16string));
```

### Ensure that a string contains valid UTF-8 text

If we have some text that "probably" contains UTF-8 encoded text and we want to replace any invalid UTF-8 sequence with a replacement character, something like the following function may be used:

```cpp
void fix_utf8_string(std::string& str)
{
    std::string temp;
    utf8::replace_invalid(str.begin(), str.end(), back_inserter(temp));
    str = temp;
}
```

The function will replace any invalid UTF-8 sequence with a Unicode replacement character. There is an overloaded function that enables the caller to supply their own replacement character.


## Points of interest

#### Design goals and decisions

The library was designed to be:

1.  Generic: for better or worse, there are many C++ string classes out there, and the library should work with as many of them as possible.
2.  Portable: the library should be portable both accross different platforms and compilers. The only non-portable code is a small section that declares unsigned integers of different sizes: three typedefs. They can be changed by the users of the library if they don't match their platform. The default setting should work for Windows (both 32 and 64 bit), and most 32 bit and 64 bit Unix derivatives. Support for post C++03 language features is included for modern compilers at API level only, so the library should work even with pretty old compilers.
3.  Lightweight: follow the "pay only for what you use" guideline.
4.  Unintrusive: avoid forcing any particular design or even programming style on the user. This is a library, not a framework.

#### Alternatives

In case you want to look into other means of working with UTF-8 strings from C++, here is the list of solutions I am aware of:

1.  [ICU Library](http://icu.sourceforge.net/). It is very powerful, complete, feature-rich, mature, and widely used. Also big, intrusive, non-generic, and doesn't play well with the Standard Library. I definitelly recommend looking at ICU even if you don't plan to use it.
2.  C++11 language and library features. Still far from complete, and not easy to use.
3.  [Glib::ustring](http://www.gtkmm.org/gtkmm2/docs/tutorial/html/ch03s04.html). A class specifically made to work with UTF-8 strings, and also feel like `std::string`. If you prefer to have yet another string class in your code, it may be worth a look. Be aware of the licensing issues, though.
4.  Platform dependent solutions: Windows and POSIX have functions to convert strings from one encoding to another. That is only a subset of what my library offers, but if that is all you need it may be good enough.


## Reference

### Functions From utf8 Namespace

#### utf8::append

Available in version 3.0 and later. Requires a C++ 11 compliant compiler.

Encodes a 32 bit code point as a UTF-8 sequence of octets and appends the sequence to a UTF-8 string.

```cpp
void append(char32_t cp, std::string& s);
```

`cp`: a code point to append to the string.  
`s`: a utf-8 encoded string to append the code point to.  

Example of use:

```cpp
std::string u;
append(0x0448, u);
assert (u[0] == char(0xd1) && u[1] == char(0x88) && u.length() == 2);
```

In case of an invalid code point, a `utf8::invalid_code_point` exception is thrown.


#### utf8::append

Available in version 1.0 and later.

Encodes a 32 bit code point as a UTF-8 sequence of octets and appends the sequence to a UTF-8 string.

```cpp
template <typename octet_iterator>
octet_iterator append(uint32_t cp, octet_iterator result);
```

`octet_iterator`: an output iterator.  
`cp`: a 32 bit integer representing a code point to append to the sequence.  
`result`: an output iterator to the place in the sequence where to append the code point.  
Return value: an iterator pointing to the place after the newly appended sequence.

Example of use:

```cpp
unsigned char u[5] = {0,0,0,0,0};
unsigned char* end = append(0x0448, u);
assert (u[0] == 0xd1 && u[1] == 0x88 && u[2] == 0 && u[3] == 0 && u[4] == 0);
```

Note that `append` does not allocate any memory - it is the burden of the caller to make sure there is enough memory allocated for the operation. To make things more interesting, `append` can add anywhere between 1 and 4 octets to the sequence. In practice, you would most often want to use `std::back_inserter` to ensure that the necessary memory is allocated.

In case of an invalid code point, a `utf8::invalid_code_point` exception is thrown.

#### utf8::next

Available in version 1.0 and later.

Given the iterator to the beginning of the UTF-8 sequence, it returns the code point and moves the iterator to the next position.

```cpp
template <typename octet_iterator> 
uint32_t next(octet_iterator& it, octet_iterator end);
```

`octet_iterator`: an input iterator.  
`it`: a reference to an iterator pointing to the beginning of an UTF-8 encoded code point. After the function returns, it is incremented to point to the beginning of the next code point.  
`end`: end of the UTF-8 sequence to be processed. If `it` gets equal to `end` during the extraction of a code point, an `utf8::not_enough_room` exception is thrown.  
Return value: the 32 bit representation of the processed UTF-8 code point.

Example of use:

```cpp
char* twochars = "\xe6\x97\xa5\xd1\x88";
char* w = twochars;
int cp = next(w, twochars + 6);
assert (cp == 0x65e5);
assert (w == twochars + 3);
```

This function is typically used to iterate through a UTF-8 encoded string.

In case of an invalid UTF-8 seqence, a `utf8::invalid_utf8` exception is thrown.

#### utf8::peek_next

Available in version 2.1 and later.

Given the iterator to the beginning of the UTF-8 sequence, it returns the code point for the following sequence without changing the value of the iterator.

```cpp
template <typename octet_iterator> 
uint32_t peek_next(octet_iterator it, octet_iterator end);
```


`octet_iterator`: an input iterator.  
`it`: an iterator pointing to the beginning of an UTF-8 encoded code point.  
`end`: end of the UTF-8 sequence to be processed. If `it` gets equal to `end` during the extraction of a code point, an `utf8::not_enough_room` exception is thrown.  
Return value: the 32 bit representation of the processed UTF-8 code point.

Example of use:

```cpp
char* twochars = "\xe6\x97\xa5\xd1\x88";
char* w = twochars;
int cp = peek_next(w, twochars + 6);
assert (cp == 0x65e5);
assert (w == twochars);
```

In case of an invalid UTF-8 seqence, a `utf8::invalid_utf8` exception is thrown.

#### utf8::prior

Available in version 1.02 and later.

Given a reference to an iterator pointing to an octet in a UTF-8 sequence, it decreases the iterator until it hits the beginning of the previous UTF-8 encoded code point and returns the 32 bits representation of the code point.

```cpp
template <typename octet_iterator> 
uint32_t prior(octet_iterator& it, octet_iterator start);
```

`octet_iterator`: a bidirectional iterator.  
`it`: a reference pointing to an octet within a UTF-8 encoded string. After the function returns, it is decremented to point to the beginning of the previous code point.  
`start`: an iterator to the beginning of the sequence where the search for the beginning of a code point is performed. It is a safety measure to prevent passing the beginning of the string in the search for a UTF-8 lead octet.  
 Return value: the 32 bit representation of the previous code point.

Example of use:

```cpp
char* twochars = "\xe6\x97\xa5\xd1\x88";
unsigned char* w = twochars + 3;
int cp = prior (w, twochars);
assert (cp == 0x65e5);
assert (w == twochars);
```

This function has two purposes: one is two iterate backwards through a UTF-8 encoded string. Note that it is usually a better idea to iterate forward instead, since `utf8::next` is faster. The second purpose is to find a beginning of a UTF-8 sequence if we have a random position within a string. Note that in that case `utf8::prior` may not detect an invalid UTF-8 sequence in some scenarios: for instance if there are superfluous trail octets, it will just skip them.

`it` will typically point to the beginning of a code point, and `start` will point to the beginning of the string to ensure we don't go backwards too far. `it` is decreased until it points to a lead UTF-8 octet, and then the UTF-8 sequence beginning with that octet is decoded to a 32 bit representation and returned.

In case `start` is reached before a UTF-8 lead octet is hit, or if an invalid UTF-8 sequence is started by the lead octet, an `invalid_utf8` exception is thrown.

In case `start` equals `it`, a `not_enough_room` exception is thrown.

#### utf8::advance
Available in version 1.0 and later.

Advances an iterator by the specified number of code points within an UTF-8 sequence.

```cpp
template <typename octet_iterator, typename distance_type> 
void advance (octet_iterator& it, distance_type n, octet_iterator end);
```

`octet_iterator`: an input iterator.  
`distance_type`: an integral type convertible to `octet_iterator`'s difference type.  
`it`: a reference to an iterator pointing to the beginning of an UTF-8 encoded code point. After the function returns, it is incremented to point to the nth following code point.  
`n`: number of code points `it` should be advanced. A negative value means decrement.  
`end`: limit of the UTF-8 sequence to be processed. If `n` is positive and `it` gets equal to `end` during the extraction of a code point, an `utf8::not_enough_room` exception is thrown. If `n` is negative and `it` reaches `end` while `it` points t a trail byte of a UTF-8 sequence, a `utf8::invalid_code_point` exception is thrown.

Example of use:

```cpp
char* twochars = "\xe6\x97\xa5\xd1\x88";
unsigned char* w = twochars;
advance (w, 2, twochars + 6);
assert (w == twochars + 5);
advance (w, -2, twochars);
assert (w == twochars);
```

In case of an invalid code point, a `utf8::invalid_code_point` exception is thrown.

#### utf8::distance

Available in version 1.0 and later.

Given the iterators to two UTF-8 encoded code points in a seqence, returns the number of code points between them.

```cpp
template <typename octet_iterator> 
typename std::iterator_traits<octet_iterator>::difference_type distance (octet_iterator first, octet_iterator last);
```

`octet_iterator`: an input iterator.  
`first`: an iterator to a beginning of a UTF-8 encoded code point.  
`last`: an iterator to a "post-end" of the last UTF-8 encoded code point in the sequence we are trying to determine the length. It can be the beginning of a new code point, or not.  
 Return value the distance between the iterators, in code points.

Example of use:

```cpp
char* twochars = "\xe6\x97\xa5\xd1\x88";
size_t dist = utf8::distance(twochars, twochars + 5);
assert (dist == 2);
```

This function is used to find the length (in code points) of a UTF-8 encoded string. The reason it is called _distance_, rather than, say, _length_ is mainly because developers are used that _length_ is an O(1) function. Computing the length of an UTF-8 string is a linear operation, and it looked better to model it after `std::distance` algorithm.

In case of an invalid UTF-8 seqence, a `utf8::invalid_utf8` exception is thrown. If `last` does not point to the past-of-end of a UTF-8 seqence, a `utf8::not_enough_room` exception is thrown.

#### utf8::utf16to8

Available in version 3.0 and later. Requires a C++ 11 compliant compiler.

Converts a UTF-16 encoded string to UTF-8.

```cpp
std::string utf16to8(const std::u16string& s);
```

`s`: a UTF-16 encoded string.
Return value: A UTF-8 encoded string.

Example of use:

```cpp
    u16string utf16string = {0x41, 0x0448, 0x65e5, 0xd834, 0xdd1e};
    string u = utf16to8(utf16string);
    assert (u.size() == 10);
```

In case of invalid UTF-16 sequence, a `utf8::invalid_utf16` exception is thrown.

#### utf8::utf16to8

Available in version 3.2 and later. Requires a C++ 17 compliant compiler.

Converts a UTF-16 encoded string to UTF-8.

```cpp
std::string utf16to8(std::u16string_view s);
```

`s`: a UTF-16 encoded string.
Return value: A UTF-8 encoded string.

Example of use:

```cpp
    u16string utf16string = {0x41, 0x0448, 0x65e5, 0xd834, 0xdd1e};
    u16string_view utf16stringview(u16string);
    string u = utf16to8(utf16string);
    assert (u.size() == 10);
```

In case of invalid UTF-16 sequence, a `utf8::invalid_utf16` exception is thrown.


#### utf8::utf16to8

Available in version 1.0 and later.

Converts a UTF-16 encoded string to UTF-8.

```cpp
template <typename u16bit_iterator, typename octet_iterator>
octet_iterator utf16to8 (u16bit_iterator start, u16bit_iterator end, octet_iterator result);
```

`u16bit_iterator`: an input iterator.  
`octet_iterator`: an output iterator.  
`start`: an iterator pointing to the beginning of the UTF-16 encoded string to convert.  
`end`: an iterator pointing to pass-the-end of the UTF-16 encoded string to convert.  
`result`: an output iterator to the place in the UTF-8 string where to append the result of conversion.  
Return value: An iterator pointing to the place after the appended UTF-8 string.

Example of use:

```cpp
unsigned short utf16string[] = {0x41, 0x0448, 0x65e5, 0xd834, 0xdd1e};
vector<unsigned char> utf8result;
utf16to8(utf16string, utf16string + 5, back_inserter(utf8result));
assert (utf8result.size() == 10);    
```

In case of invalid UTF-16 sequence, a `utf8::invalid_utf16` exception is thrown.

#### utf8::utf8to16

Available in version 3.0 and later. Requires a C++ 11 compliant compiler.

Converts an UTF-8 encoded string to UTF-16.

```cpp
std::u16string utf8to16(const std::string& s);
```

`s`: an UTF-8 encoded string to convert.  
Return value: A UTF-16 encoded string

Example of use:

```cpp
string utf8_with_surrogates = "\xe6\x97\xa5\xd1\x88\xf0\x9d\x84\x9e";
u16string utf16result = utf8to16(utf8_with_surrogates);
assert (utf16result.length() == 4);
assert (utf16result[2] == 0xd834);
assert (utf16result[3] == 0xdd1e);
```

In case of an invalid UTF-8 seqence, a `utf8::invalid_utf8` exception is thrown.

#### utf8::utf8to16

Available in version 3.2 and later. Requires a C++ 17 compliant compiler.

Converts an UTF-8 encoded string to UTF-16.

```cpp
std::u16string utf8to16(std::string_view s);
```

`s`: an UTF-8 encoded string to convert.  
Return value: A UTF-16 encoded string

Example of use:

```cpp
string_view utf8_with_surrogates = "\xe6\x97\xa5\xd1\x88\xf0\x9d\x84\x9e";
u16string utf16result = utf8to16(utf8_with_surrogates);
assert (utf16result.length() == 4);
assert (utf16result[2] == 0xd834);
assert (utf16result[3] == 0xdd1e);
```

In case of an invalid UTF-8 seqence, a `utf8::invalid_utf8` exception is thrown.


#### utf8::utf8to16

Available in version 1.0 and later.

Converts an UTF-8 encoded string to UTF-16

```cpp
template <typename u16bit_iterator, typename octet_iterator>
u16bit_iterator utf8to16 (octet_iterator start, octet_iterator end, u16bit_iterator result);
```

`octet_iterator`: an input iterator.  
`u16bit_iterator`: an output iterator.  
`start`: an iterator pointing to the beginning of the UTF-8 encoded string to convert. < br /> `end`: an iterator pointing to pass-the-end of the UTF-8 encoded string to convert.  
`result`: an output iterator to the place in the UTF-16 string where to append the result of conversion.  
Return value: An iterator pointing to the place after the appended UTF-16 string.

Example of use:

```cpp
char utf8_with_surrogates[] = "\xe6\x97\xa5\xd1\x88\xf0\x9d\x84\x9e";
vector <unsigned short> utf16result;
utf8to16(utf8_with_surrogates, utf8_with_surrogates + 9, back_inserter(utf16result));
assert (utf16result.size() == 4);
assert (utf16result[2] == 0xd834);
assert (utf16result[3] == 0xdd1e);
```

In case of an invalid UTF-8 seqence, a `utf8::invalid_utf8` exception is thrown. If `end` does not point to the past-of-end of a UTF-8 seqence, a `utf8::not_enough_room` exception is thrown.

#### utf8::utf32to8

Available in version 3.0 and later. Requires a C++ 11 compliant compiler.

Converts a UTF-32 encoded string to UTF-8.

```cpp
std::string utf32to8(const std::u32string& s);
```

`s`: a UTF-32 encoded string.  
Return value: a UTF-8 encoded string.

Example of use:

```cpp
u32string utf32string = {0x448, 0x65E5, 0x10346};
string utf8result = utf32to8(utf32string);
assert (utf8result.size() == 9);
```

In case of invalid UTF-32 string, a `utf8::invalid_code_point` exception is thrown.

#### utf8::utf32to8

Available in version 3.2 and later. Requires a C++ 17 compliant compiler.

Converts a UTF-32 encoded string to UTF-8.

```cpp
std::string utf32to8(std::u32string_view s);
```

`s`: a UTF-32 encoded string.  
Return value: a UTF-8 encoded string.

Example of use:

```cpp
u32string utf32string = {0x448, 0x65E5, 0x10346};
u32string_view utf32stringview(utf32string);
string utf8result = utf32to8(utf32stringview);
assert (utf8result.size() == 9);
```

In case of invalid UTF-32 string, a `utf8::invalid_code_point` exception is thrown.


#### utf8::utf32to8

Available in version 1.0 and later.

Converts a UTF-32 encoded string to UTF-8.

```cpp
template <typename octet_iterator, typename u32bit_iterator>
octet_iterator utf32to8 (u32bit_iterator start, u32bit_iterator end, octet_iterator result);
```

`octet_iterator`: an output iterator.  
`u32bit_iterator`: an input iterator.  
`start`: an iterator pointing to the beginning of the UTF-32 encoded string to convert.  
`end`: an iterator pointing to pass-the-end of the UTF-32 encoded string to convert.  
`result`: an output iterator to the place in the UTF-8 string where to append the result of conversion.  
Return value: An iterator pointing to the place after the appended UTF-8 string.

Example of use:

```cpp
int utf32string[] = {0x448, 0x65E5, 0x10346, 0};
vector<unsigned char> utf8result;
utf32to8(utf32string, utf32string + 3, back_inserter(utf8result));
assert (utf8result.size() == 9);
```

In case of invalid UTF-32 string, a `utf8::invalid_code_point` exception is thrown.

#### utf8::utf8to32

Available in version 3.0 and later. Requires a C++ 11 compliant compiler.

Converts a UTF-8 encoded string to UTF-32.

```cpp
std::u32string utf8to32(const std::string& s);
```

`s`: a UTF-8 encoded string.
Return value: a UTF-32 encoded string.

Example of use:

```cpp
const char* twochars = "\xe6\x97\xa5\xd1\x88";
u32string utf32result = utf8to32(twochars);
assert (utf32result.size() == 2);
```

In case of an invalid UTF-8 seqence, a `utf8::invalid_utf8` exception is thrown.

#### utf8::utf8to32

Available in version 3.2 and later. Requires a C++ 17 compliant compiler.

Converts a UTF-8 encoded string to UTF-32.

```cpp
std::u32string utf8to32(std::string_view s);
```

`s`: a UTF-8 encoded string.
Return value: a UTF-32 encoded string.

Example of use:

```cpp
string_view twochars = "\xe6\x97\xa5\xd1\x88";
u32string utf32result = utf8to32(twochars);
assert (utf32result.size() == 2);
```

In case of an invalid UTF-8 seqence, a `utf8::invalid_utf8` exception is thrown.


#### utf8::utf8to32

Available in version 1.0 and later.

Converts a UTF-8 encoded string to UTF-32.

```cpp
template <typename octet_iterator, typename u32bit_iterator>
u32bit_iterator utf8to32 (octet_iterator start, octet_iterator end, u32bit_iterator result);
```

`octet_iterator`: an input iterator.  
`u32bit_iterator`: an output iterator.  
`start`: an iterator pointing to the beginning of the UTF-8 encoded string to convert.  
`end`: an iterator pointing to pass-the-end of the UTF-8 encoded string to convert.  
`result`: an output iterator to the place in the UTF-32 string where to append the result of conversion.  
Return value: An iterator pointing to the place after the appended UTF-32 string.

Example of use:

```cpp
char* twochars = "\xe6\x97\xa5\xd1\x88";
vector<int> utf32result;
utf8to32(twochars, twochars + 5, back_inserter(utf32result));
assert (utf32result.size() == 2);
```

In case of an invalid UTF-8 seqence, a `utf8::invalid_utf8` exception is thrown. If `end` does not point to the past-of-end of a UTF-8 seqence, a `utf8::not_enough_room` exception is thrown.

#### utf8::find_invalid

Available in version 3.0 and later. Requires a C++ 11 compliant compiler.

Detects an invalid sequence within a UTF-8 string.

```cpp
std::size_t find_invalid(const std::string& s);
```

`s`: a UTF-8 encoded string.
Return value: the index of the first invalid octet in the UTF-8 string. In case none were found, equals `std::string::npos`.

Example of use:

```cpp
string utf_invalid = "\xe6\x97\xa5\xd1\x88\xfa";
auto invalid = find_invalid(utf_invalid);
assert (invalid == 5);
```

This function is typically used to make sure a UTF-8 string is valid before processing it with other functions. It is especially important to call it if before doing any of the _unchecked_ operations on it.

#### utf8::find_invalid

Available in version 3.2 and later. Requires a C++ 17 compliant compiler.

Detects an invalid sequence within a UTF-8 string.

```cpp
std::size_t find_invalid(std::string_view s);
```

`s`: a UTF-8 encoded string.
Return value: the index of the first invalid octet in the UTF-8 string. In case none were found, equals `std::string_view::npos`.

Example of use:

```cpp
string_view utf_invalid = "\xe6\x97\xa5\xd1\x88\xfa";
auto invalid = find_invalid(utf_invalid);
assert (invalid == 5);
```

This function is typically used to make sure a UTF-8 string is valid before processing it with other functions. It is especially important to call it if before doing any of the _unchecked_ operations on it.


#### utf8::find_invalid

Available in version 1.0 and later.

Detects an invalid sequence within a UTF-8 string.

```cpp
template <typename octet_iterator> 
octet_iterator find_invalid(octet_iterator start, octet_iterator end);
```

`octet_iterator`: an input iterator.  
`start`: an iterator pointing to the beginning of the UTF-8 string to test for validity.  
`end`: an iterator pointing to pass-the-end of the UTF-8 string to test for validity.  
Return value: an iterator pointing to the first invalid octet in the UTF-8 string. In case none were found, equals `end`.

Example of use:

```cpp
char utf_invalid[] = "\xe6\x97\xa5\xd1\x88\xfa";
char* invalid = find_invalid(utf_invalid, utf_invalid + 6);
assert (invalid == utf_invalid + 5);
```

This function is typically used to make sure a UTF-8 string is valid before processing it with other functions. It is especially important to call it if before doing any of the _unchecked_ operations on it.

#### utf8::is_valid

Available in version 3.0 and later. Requires a C++ 11 compliant compiler.

Checks whether a string object contains valid UTF-8 encoded text.

```cpp
bool is_valid(const std::string& s);
```

`s`: a UTF-8 encoded string.  
Return value: `true` if the string contains valid UTF-8 encoded text; `false` if not.

Example of use:

```cpp
char utf_invalid[] = "\xe6\x97\xa5\xd1\x88\xfa";
bool bvalid = is_valid(utf_invalid);
assert (bvalid == false);
```

You may want to use `is_valid` to make sure that a string contains valid UTF-8 text without the need to know where it fails if it is not valid.

#### utf8::is_valid

Available in version 3.2 and later. Requires a C++ 17 compliant compiler.

Checks whether a string object contains valid UTF-8 encoded text.

```cpp
bool is_valid(std::string_view s);
```

`s`: a UTF-8 encoded string.  
Return value: `true` if the string contains valid UTF-8 encoded text; `false` if not.

Example of use:

```cpp
string_view utf_invalid = "\xe6\x97\xa5\xd1\x88\xfa";
bool bvalid = is_valid(utf_invalid);
assert (bvalid == false);
```

You may want to use `is_valid` to make sure that a string contains valid UTF-8 text without the need to know where it fails if it is not valid.


#### utf8::is_valid

Available in version 1.0 and later.

Checks whether a sequence of octets is a valid UTF-8 string.

```cpp
template <typename octet_iterator> 
bool is_valid(octet_iterator start, octet_iterator end);
```

`octet_iterator`: an input iterator.  
`start`: an iterator pointing to the beginning of the UTF-8 string to test for validity.  
`end`: an iterator pointing to pass-the-end of the UTF-8 string to test for validity.  
Return value: `true` if the sequence is a valid UTF-8 string; `false` if not.

Example of use:

```cpp
char utf_invalid[] = "\xe6\x97\xa5\xd1\x88\xfa";
bool bvalid = is_valid(utf_invalid, utf_invalid + 6);
assert (bvalid == false);
```

`is_valid` is a shorthand for `find_invalid(start, end) == end;`. You may want to use it to make sure that a byte seqence is a valid UTF-8 string without the need to know where it fails if it is not valid.

#### utf8::replace_invalid

Available in version 3.0 and later. Requires a C++ 11 compliant compiler.

Replaces all invalid UTF-8 sequences within a string with a replacement marker.

```cpp
std::string replace_invalid(const std::string& s, char32_t replacement);
std::string replace_invalid(const std::string& s);
```

`s`: a UTF-8 encoded string.  
`replacement`: A Unicode code point for the replacement marker. The version without this parameter assumes the value `0xfffd`  
Return value: A UTF-8 encoded string with replaced invalid sequences.

Example of use:

```cpp
string invalid_sequence = "a\x80\xe0\xa0\xc0\xaf\xed\xa0\x80z";
string replace_invalid_result = replace_invalid(invalid_sequence, '?');
bvalid = is_valid(replace_invalid_result);
assert (bvalid);
const string fixed_invalid_sequence = "a????z";
assert (fixed_invalid_sequence == replace_invalid_result);
```

#### utf8::replace_invalid

Available in version 3.2 and later. Requires a C++ 17 compliant compiler.

Replaces all invalid UTF-8 sequences within a string with a replacement marker.

```cpp
std::string replace_invalid(std::string_view s, char32_t replacement);
std::string replace_invalid(std::string_view s);
```

`s`: a UTF-8 encoded string.  
`replacement`: A Unicode code point for the replacement marker. The version without this parameter assumes the value `0xfffd`  
Return value: A UTF-8 encoded string with replaced invalid sequences.

Example of use:

```cpp
string_view invalid_sequence = "a\x80\xe0\xa0\xc0\xaf\xed\xa0\x80z";
string replace_invalid_result = replace_invalid(invalid_sequence, '?');
bool bvalid = is_valid(replace_invalid_result);
assert (bvalid);
const string fixed_invalid_sequence = "a????z";
assert(fixed_invalid_sequence, replace_invalid_result);
```


#### utf8::replace_invalid

Available in version 2.0 and later.

Replaces all invalid UTF-8 sequences within a string with a replacement marker.

```cpp
template <typename octet_iterator, typename output_iterator>
output_iterator replace_invalid(octet_iterator start, octet_iterator end, output_iterator out, uint32_t replacement);
template <typename octet_iterator, typename output_iterator>
output_iterator replace_invalid(octet_iterator start, octet_iterator end, output_iterator out);
```

`octet_iterator`: an input iterator.  
`output_iterator`: an output iterator.  
`start`: an iterator pointing to the beginning of the UTF-8 string to look for invalid UTF-8 sequences.  
`end`: an iterator pointing to pass-the-end of the UTF-8 string to look for invalid UTF-8 sequences.  
`out`: An output iterator to the range where the result of replacement is stored.  
`replacement`: A Unicode code point for the replacement marker. The version without this parameter assumes the value `0xfffd`  
Return value: An iterator pointing to the place after the UTF-8 string with replaced invalid sequences.

Example of use:

```cpp
char invalid_sequence[] = "a\x80\xe0\xa0\xc0\xaf\xed\xa0\x80z";
vector<char> replace_invalid_result;
replace_invalid (invalid_sequence, invalid_sequence + sizeof(invalid_sequence), back_inserter(replace_invalid_result), '?');
bvalid = is_valid(replace_invalid_result.begin(), replace_invalid_result.end());
assert (bvalid);
char* fixed_invalid_sequence = "a????z";
assert (std::equal(replace_invalid_result.begin(), replace_invalid_result.end(), fixed_invalid_sequence));
```

`replace_invalid` does not perform in-place replacement of invalid sequences. Rather, it produces a copy of the original string with the invalid sequences replaced with a replacement marker. Therefore, `out` must not be in the `[start, end]` range.

#### utf8::starts_with_bom

Available in version 3.0 and later. Requires a C++ 11 compliant compiler.

Checks whether a string starts with a UTF-8 byte order mark (BOM)

```cpp
bool starts_with_bom(const std::string& s);
```

`s`: a UTF-8 encoded string.
Return value: `true` if the string starts with a UTF-8 byte order mark; `false` if not.

Example of use:

```cpp
string byte_order_mark = {char(0xef), char(0xbb), char(0xbf)};
bool bbom = starts_with_bom(byte_order_mark);
assert (bbom == true);
string threechars = "\xf0\x90\x8d\x86\xe6\x97\xa5\xd1\x88";
bool no_bbom = starts_with_bom(threechars);
assert (no_bbom == false);
 ```

The typical use of this function is to check the first three bytes of a file. If they form the UTF-8 BOM, we want to skip them before processing the actual UTF-8 encoded text.


#### utf8::starts_with_bom

Available in version 3.2 and later. Requires a C++ 17 compliant compiler.

Checks whether a string starts with a UTF-8 byte order mark (BOM)

```cpp
bool starts_with_bom(std::string_view s);
```

`s`: a UTF-8 encoded string.
Return value: `true` if the string starts with a UTF-8 byte order mark; `false` if not.

Example of use:

```cpp
string byte_order_mark = {char(0xef), char(0xbb), char(0xbf)};
string_view byte_order_mark_view(byte_order_mark);
bool bbom = starts_with_bom(byte_order_mark_view);
assert (bbom);
string_view threechars = "\xf0\x90\x8d\x86\xe6\x97\xa5\xd1\x88";
bool no_bbom = starts_with_bom(threechars);
assert (!no_bbom);
 ```

The typical use of this function is to check the first three bytes of a file. If they form the UTF-8 BOM, we want to skip them before processing the actual UTF-8 encoded text.


#### utf8::starts_with_bom

Available in version 2.3 and later.

Checks whether an octet sequence starts with a UTF-8 byte order mark (BOM)

```cpp
template <typename octet_iterator> 
bool starts_with_bom (octet_iterator it, octet_iterator end);
```

`octet_iterator`: an input iterator.  
`it`: beginning of the octet sequence to check  
`end`: pass-end of the sequence to check  
Return value: `true` if the sequence starts with a UTF-8 byte order mark; `false` if not.

Example of use:

```cpp
unsigned char byte_order_mark[] = {0xef, 0xbb, 0xbf};
bool bbom = starts_with_bom(byte_order_mark, byte_order_mark + sizeof(byte_order_mark));
assert (bbom == true);
```

The typical use of this function is to check the first three bytes of a file. If they form the UTF-8 BOM, we want to skip them before processing the actual UTF-8 encoded text.

### Types From utf8 Namespace

#### utf8::exception

Available in version 2.3 and later.

Base class for the exceptions thrown by UTF CPP library functions.

```cpp
class exception : public std::exception {};
```

Example of use:

```cpp
try {
  code_that_uses_utf_cpp_library();
}
catch(const utf8::exception& utfcpp_ex) {
  cerr << utfcpp_ex.what();
}
```

#### utf8::invalid_code_point

Available in version 1.0 and later.

Thrown by UTF8 CPP functions such as `advance` and `next` if an UTF-8 sequence represents and invalid code point.

```cpp
class invalid_code_point : public exception {
public: 
    uint32_t code_point() const;
};
```

Member function `code_point()` can be used to determine the invalid code point that caused the exception to be thrown.

#### utf8::invalid_utf8

Available in version 1.0 and later.

Thrown by UTF8 CPP functions such as `next` and `prior` if an invalid UTF-8 sequence is detected during decoding.

```cpp
class invalid_utf8 : public exception {
public: 
    uint8_t utf8_octet() const;
};
```

Member function `utf8_octet()` can be used to determine the beginning of the byte sequence that caused the exception to be thrown.

#### utf8::invalid_utf16

Available in version 1.0 and later.

Thrown by UTF8 CPP function `utf16to8` if an invalid UTF-16 sequence is detected during decoding.

```cpp
class invalid_utf16 : public exception {
public: 
    uint16_t utf16_word() const;
};
```

Member function `utf16_word()` can be used to determine the UTF-16 code unit that caused the exception to be thrown.

#### utf8::not_enough_room

Available in version 1.0 and later.

Thrown by UTF8 CPP functions such as `next` if the end of the decoded UTF-8 sequence was reached before the code point was decoded.

```cpp
class not_enough_room : public exception {};
```

#### utf8::iterator

Available in version 2.0 and later.

Adapts the underlying octet iterator to iterate over the sequence of code points, rather than raw octets.

```cpp
template <typename octet_iterator>
class iterator;
```

##### Member functions

`iterator();` the deafult constructor; the underlying octet_iterator is constructed with its default constructor.

`explicit iterator (const octet_iterator& octet_it, const octet_iterator& range_start, const octet_iterator& range_end);` a constructor that initializes the underlying octet_iterator with octet_it and sets the range in which the iterator is considered valid.

`octet_iterator base () const;` returns the underlying octet_iterator.

`uint32_t operator * () const;` decodes the utf-8 sequence the underlying octet_iterator is pointing to and returns the code point.

`bool operator == (const iterator& rhs) const;` returns `true` if the two underlaying iterators are equal.

`bool operator != (const iterator& rhs) const;` returns `true` if the two underlaying iterators are not equal.

`iterator& operator ++ ();` the prefix increment - moves the iterator to the next UTF-8 encoded code point.

`iterator operator ++ (int);` the postfix increment - moves the iterator to the next UTF-8 encoded code point and returns the current one.

`iterator& operator -- ();` the prefix decrement - moves the iterator to the previous UTF-8 encoded code point.

`iterator operator -- (int);` the postfix decrement - moves the iterator to the previous UTF-8 encoded code point and returns the current one.

Example of use:

```cpp
char* threechars = "\xf0\x90\x8d\x86\xe6\x97\xa5\xd1\x88";
utf8::iterator<char*> it(threechars, threechars, threechars + 9);
utf8::iterator<char*> it2 = it;
assert (it2 == it);
assert (*it == 0x10346);
assert (*(++it) == 0x65e5);
assert ((*it++) == 0x65e5);
assert (*it == 0x0448);
assert (it != it2);
utf8::iterator<char*> endit (threechars + 9, threechars, threechars + 9);  
assert (++it == endit);
assert (*(--it) == 0x0448);
assert ((*it--) == 0x0448);
assert (*it == 0x65e5);
assert (--it == utf8::iterator<char*>(threechars, threechars, threechars + 9));
assert (*it == 0x10346);
```

The purpose of `utf8::iterator` adapter is to enable easy iteration as well as the use of STL algorithms with UTF-8 encoded strings. Increment and decrement operators are implemented in terms of `utf8::next()` and `utf8::prior()` functions.

Note that `utf8::iterator` adapter is a checked iterator. It operates on the range specified in the constructor; any attempt to go out of that range will result in an exception. Even the comparison operators require both iterator object to be constructed against the same range - otherwise an exception is thrown. Typically, the range will be determined by sequence container functions `begin` and `end`, i.e.:

```cpp
std::string s = "example";
utf8::iterator i (s.begin(), s.begin(), s.end());
```

### Functions From utf8::unchecked Namespace

#### utf8::unchecked::append

Available in version 1.0 and later.

Encodes a 32 bit code point as a UTF-8 sequence of octets and appends the sequence to a UTF-8 string.

```cpp
template <typename octet_iterator>
octet_iterator append(uint32_t cp, octet_iterator result);
```

`cp`: A 32 bit integer representing a code point to append to the sequence.  
`result`: An output iterator to the place in the sequence where to append the code point.  
Return value: An iterator pointing to the place after the newly appended sequence.

Example of use:

```cpp
unsigned char u[5] = {0,0,0,0,0};
unsigned char* end = unchecked::append(0x0448, u);
assert (u[0] == 0xd1 && u[1] == 0x88 && u[2] == 0 && u[3] == 0 && u[4] == 0);
```

This is a faster but less safe version of `utf8::append`. It does not check for validity of the supplied code point, and may produce an invalid UTF-8 sequence.

#### utf8::unchecked::next

Available in version 1.0 and later.

Given the iterator to the beginning of a UTF-8 sequence, it returns the code point and moves the iterator to the next position.

```cpp
template <typename octet_iterator>
uint32_t next(octet_iterator& it);
```

`it`: a reference to an iterator pointing to the beginning of an UTF-8 encoded code point. After the function returns, it is incremented to point to the beginning of the next code point.  
 Return value: the 32 bit representation of the processed UTF-8 code point.

Example of use:

```cpp
char* twochars = "\xe6\x97\xa5\xd1\x88";
char* w = twochars;
int cp = unchecked::next(w);
assert (cp == 0x65e5);
assert (w == twochars + 3);
```

This is a faster but less safe version of `utf8::next`. It does not check for validity of the supplied UTF-8 sequence.

#### utf8::unchecked::peek_next

Available in version 2.1 and later.

Given the iterator to the beginning of a UTF-8 sequence, it returns the code point.

```cpp
template <typename octet_iterator>
uint32_t peek_next(octet_iterator it);
```

`it`: an iterator pointing to the beginning of an UTF-8 encoded code point.  
Return value: the 32 bit representation of the processed UTF-8 code point.

Example of use:

```cpp
char* twochars = "\xe6\x97\xa5\xd1\x88";
char* w = twochars;
int cp = unchecked::peek_next(w);
assert (cp == 0x65e5);
assert (w == twochars);
```

This is a faster but less safe version of `utf8::peek_next`. It does not check for validity of the supplied UTF-8 sequence.

#### utf8::unchecked::prior

Available in version 1.02 and later.

Given a reference to an iterator pointing to an octet in a UTF-8 seqence, it decreases the iterator until it hits the beginning of the previous UTF-8 encoded code point and returns the 32 bits representation of the code point.

```cpp
template <typename octet_iterator>
uint32_t prior(octet_iterator& it);
```

`it`: a reference pointing to an octet within a UTF-8 encoded string. After the function returns, it is decremented to point to the beginning of the previous code point.  
 Return value: the 32 bit representation of the previous code point.

Example of use:

```cpp
char* twochars = "\xe6\x97\xa5\xd1\x88";
char* w = twochars + 3;
int cp = unchecked::prior (w);
assert (cp == 0x65e5);
assert (w == twochars);
```

This is a faster but less safe version of `utf8::prior`. It does not check for validity of the supplied UTF-8 sequence and offers no boundary checking.

#### utf8::unchecked::advance

Available in version 1.0 and later.

Advances an iterator by the specified number of code points within an UTF-8 sequence.

```cpp
template <typename octet_iterator, typename distance_type>
void advance (octet_iterator& it, distance_type n);
```

`it`: a reference to an iterator pointing to the beginning of an UTF-8 encoded code point. After the function returns, it is incremented to point to the nth following code point.
`n`: number of code points `it` should be advanced. A negative value means decrement.

Example of use:

```cpp
char* twochars = "\xe6\x97\xa5\xd1\x88";
char* w = twochars;
unchecked::advance (w, 2);
assert (w == twochars + 5);
```

This is a faster but less safe version of `utf8::advance`. It does not check for validity of the supplied UTF-8 sequence and offers no boundary checking.

#### utf8::unchecked::distance

Available in version 1.0 and later.

Given the iterators to two UTF-8 encoded code points in a seqence, returns the number of code points between them.

```cpp
template <typename octet_iterator>
typename std::iterator_traits<octet_iterator>::difference_type distance (octet_iterator first, octet_iterator last);
```

`first`: an iterator to a beginning of a UTF-8 encoded code point.  
`last`: an iterator to a "post-end" of the last UTF-8 encoded code point in the sequence we are trying to determine the length. It can be the beginning of a new code point, or not.  
Return value: the distance between the iterators, in code points.

Example of use:

```cpp
char* twochars = "\xe6\x97\xa5\xd1\x88";
size_t dist = utf8::unchecked::distance(twochars, twochars + 5);
assert (dist == 2);
```

This is a faster but less safe version of `utf8::distance`. It does not check for validity of the supplied UTF-8 sequence.

#### utf8::unchecked::utf16to8

Available in version 1.0 and later.

Converts a UTF-16 encoded string to UTF-8.

```cpp
template <typename u16bit_iterator, typename octet_iterator>
octet_iterator utf16to8 (u16bit_iterator start, u16bit_iterator end, octet_iterator result);
```

`start`: an iterator pointing to the beginning of the UTF-16 encoded string to convert.  
`end`: an iterator pointing to pass-the-end of the UTF-16 encoded string to convert.  
`result`: an output iterator to the place in the UTF-8 string where to append the result of conversion.  
Return value: An iterator pointing to the place after the appended UTF-8 string.

Example of use:

```cpp
unsigned short utf16string[] = {0x41, 0x0448, 0x65e5, 0xd834, 0xdd1e};
vector<unsigned char> utf8result;
unchecked::utf16to8(utf16string, utf16string + 5, back_inserter(utf8result));
assert (utf8result.size() == 10);    
```

This is a faster but less safe version of `utf8::utf16to8`. It does not check for validity of the supplied UTF-16 sequence.

#### utf8::unchecked::utf8to16

Available in version 1.0 and later.

Converts an UTF-8 encoded string to UTF-16

```cpp
template <typename u16bit_iterator, typename octet_iterator>
u16bit_iterator utf8to16 (octet_iterator start, octet_iterator end, u16bit_iterator result);
```

`start`: an iterator pointing to the beginning of the UTF-8 encoded string to convert. < br /> `end`: an iterator pointing to pass-the-end of the UTF-8 encoded string to convert.  
`result`: an output iterator to the place in the UTF-16 string where to append the result of conversion.  
Return value: An iterator pointing to the place after the appended UTF-16 string.

Example of use:

```cpp
char utf8_with_surrogates[] = "\xe6\x97\xa5\xd1\x88\xf0\x9d\x84\x9e";
vector <unsigned short> utf16result;
unchecked::utf8to16(utf8_with_surrogates, utf8_with_surrogates + 9, back_inserter(utf16result));
assert (utf16result.size() == 4);
assert (utf16result[2] == 0xd834);
assert (utf16result[3] == 0xdd1e);
```

This is a faster but less safe version of `utf8::utf8to16`. It does not check for validity of the supplied UTF-8 sequence.

#### utf8::unchecked::utf32to8

Available in version 1.0 and later.

Converts a UTF-32 encoded string to UTF-8.

```cpp
template <typename octet_iterator, typename u32bit_iterator>
octet_iterator utf32to8 (u32bit_iterator start, u32bit_iterator end, octet_iterator result);
```

`start`: an iterator pointing to the beginning of the UTF-32 encoded string to convert.  
`end`: an iterator pointing to pass-the-end of the UTF-32 encoded string to convert.  
`result`: an output iterator to the place in the UTF-8 string where to append the result of conversion.  
Return value: An iterator pointing to the place after the appended UTF-8 string.

Example of use:

```cpp
int utf32string[] = {0x448, 0x65e5, 0x10346, 0};
vector<unsigned char> utf8result;
utf32to8(utf32string, utf32string + 3, back_inserter(utf8result));
assert (utf8result.size() == 9);
```

This is a faster but less safe version of `utf8::utf32to8`. It does not check for validity of the supplied UTF-32 sequence.

#### utf8::unchecked::utf8to32

Available in version 1.0 and later.

Converts a UTF-8 encoded string to UTF-32.

```cpp
template <typename octet_iterator, typename u32bit_iterator>
u32bit_iterator utf8to32 (octet_iterator start, octet_iterator end, u32bit_iterator result);
```

`start`: an iterator pointing to the beginning of the UTF-8 encoded string to convert.  
`end`: an iterator pointing to pass-the-end of the UTF-8 encoded string to convert.  
`result`: an output iterator to the place in the UTF-32 string where to append the result of conversion.  
Return value: An iterator pointing to the place after the appended UTF-32 string.

Example of use:

```cpp
char* twochars = "\xe6\x97\xa5\xd1\x88";
vector<int> utf32result;
unchecked::utf8to32(twochars, twochars + 5, back_inserter(utf32result));
assert (utf32result.size() == 2);
```

This is a faster but less safe version of `utf8::utf8to32`. It does not check for validity of the supplied UTF-8 sequence.

#### utf8::unchecked::replace_invalid

Available in version 3.1 and later.

Replaces all invalid UTF-8 sequences within a string with a replacement marker.

```cpp
template <typename octet_iterator, typename output_iterator>
output_iterator replace_invalid(octet_iterator start, octet_iterator end, output_iterator out, uint32_t replacement);
template <typename octet_iterator, typename output_iterator>
output_iterator replace_invalid(octet_iterator start, octet_iterator end, output_iterator out);
```

`octet_iterator`: an input iterator.  
`output_iterator`: an output iterator.  
`start`: an iterator pointing to the beginning of the UTF-8 string to look for invalid UTF-8 sequences.  
`end`: an iterator pointing to pass-the-end of the UTF-8 string to look for invalid UTF-8 sequences.  
`out`: An output iterator to the range where the result of replacement is stored.  
`replacement`: A Unicode code point for the replacement marker. The version without this parameter assumes the value `0xfffd`  
Return value: An iterator pointing to the place after the UTF-8 string with replaced invalid sequences.

Example of use:

```cpp
char invalid_sequence[] = "a\x80\xe0\xa0\xc0\xaf\xed\xa0\x80z";
vector<char> replace_invalid_result;
unchecked::replace_invalid (invalid_sequence, invalid_sequence + sizeof(invalid_sequence), back_inserter(replace_invalid_result), '?');
bvalid = utf8::is_valid(replace_invalid_result.begin(), replace_invalid_result.end());
assert (bvalid);
char* fixed_invalid_sequence = "a????z";
assert (std::equal(replace_invalid_result.begin(), replace_invalid_result.end(), fixed_invalid_sequence));
```

`replace_invalid` does not perform in-place replacement of invalid sequences. Rather, it produces a copy of the original string with the invalid sequences replaced with a replacement marker. Therefore, `out` must not be in the `[start, end]` range.

Unlike `utf8::replace_invalid`, this function does not verify validity of the replacement marker.

### Types From utf8::unchecked Namespace

#### utf8::iterator

Available in version 2.0 and later.

Adapts the underlying octet iterator to iterate over the sequence of code points, rather than raw octets.

```cpp
template <typename octet_iterator>
class iterator;
```

##### Member functions

`iterator();` the deafult constructor; the underlying octet_iterator is constructed with its default constructor.

`explicit iterator (const octet_iterator& octet_it);` a constructor that initializes the underlying octet_iterator with `octet_it`.

`octet_iterator base () const;` returns the underlying octet_iterator.

`uint32_t operator * () const;` decodes the utf-8 sequence the underlying octet_iterator is pointing to and returns the code point.

`bool operator == (const iterator& rhs) const;` returns `true` if the two underlaying iterators are equal.

`bool operator != (const iterator& rhs) const;` returns `true` if the two underlaying iterators are not equal.

`iterator& operator ++ ();` the prefix increment - moves the iterator to the next UTF-8 encoded code point.

`iterator operator ++ (int);` the postfix increment - moves the iterator to the next UTF-8 encoded code point and returns the current one.

`iterator& operator -- ();` the prefix decrement - moves the iterator to the previous UTF-8 encoded code point.

`iterator operator -- (int);` the postfix decrement - moves the iterator to the previous UTF-8 encoded code point and returns the current one.

Example of use:

```cpp
char* threechars = "\xf0\x90\x8d\x86\xe6\x97\xa5\xd1\x88";
utf8::unchecked::iterator<char*> un_it(threechars);
utf8::unchecked::iterator<char*> un_it2 = un_it;
assert (un_it2 == un_it);
assert (*un_it == 0x10346);
assert (*(++un_it) == 0x65e5);
assert ((*un_it++) == 0x65e5);
assert (*un_it == 0x0448);
assert (un_it != un_it2);
utf8::::unchecked::iterator<char*> un_endit (threechars + 9);  
assert (++un_it == un_endit);
assert (*(--un_it) == 0x0448);
assert ((*un_it--) == 0x0448);
assert (*un_it == 0x65e5);
assert (--un_it == utf8::unchecked::iterator<char*>(threechars));
assert (*un_it == 0x10346);
```

This is an unchecked version of `utf8::iterator`. It is faster in many cases, but offers no validity or range checks.

## Links

1.  [The Unicode Consortium](http://www.unicode.org/).
2.  [ICU Library](http://icu.sourceforge.net/).
3.  [UTF-8 at Wikipedia](http://en.wikipedia.org/wiki/UTF-8)
4.  [UTF-8 and Unicode FAQ for Unix/Linux](http://www.cl.cam.ac.uk/~mgk25/unicode.html)
