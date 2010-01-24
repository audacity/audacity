// strparse.h -- header for String_parse class

class String_parse {
public:
    int pos;
    std::string *str;
    void init(std::string *s) {
        str = s;
        pos = 0;
    }
    void skip_space();
    char peek();
    void get_nonspace_quoted(std::string &field);
    // get the remaining characters, skipping initial spaces and final return
    void get_remainder(std::string &field);
};

void string_escape(std::string &result, const char *s, const char *quote);
