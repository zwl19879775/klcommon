/**
  Convert string encode.
  Kevin lynx
  1.31.2011
  Tested in Ubuntu only.
*/
#ifndef ___stringencoder_h
#define ___stringencoder_h

/**these functions in_len is better to include '\0' */
/** i.e: dbcs2wchar("GBK", ...*/
int dbcs2wchar(const char* code_page, const char* in,int in_len, wchar_t*
        out,int out_max);

int wchar2dbcs(const char* code_page, const wchar_t* in,int in_len, char*
        out,int out_max);

int wchar2utf8(const wchar_t* in,int in_len, char* out,int out_max);

int utf82wchar(const char* in,int in_len, wchar_t* out,int out_max);

/** utility functions */
int utf82dbcs(const char* code_page, const char* in, int in_len, 
        char* out, int out_max);

int dbcs2utf8(const char* code_page, const char* in, int in_len, 
        char* out, int out_max);
#endif

