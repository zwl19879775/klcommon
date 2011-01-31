/**
  Convert string encode.
  Kevin lynx
  1.31.2011
  Tested in Ubuntu only.
*/
#ifndef WIN32
#include <stdlib.h>
#include <wchar.h>
#include <iconv.h>
#include <string.h>
#else
#include <windows.h>
#endif
#include "stringencoder.h"

int dbcs2wchar(const char* code_page, const char* in,int in_len, wchar_t* out,int out_max)
{
#ifndef WIN32
    size_t result;
    iconv_t env;
    env = iconv_open("WCHAR_T",code_page);
    result = iconv(env,(char**)&in,(size_t*)&in_len,(char**)&out,(size_t*)&out_max);
    iconv_close(env);
    return (int) result;
#else
    return ::MultiByteToWideChar(codepage(code_page),0,in,in_len,out,out_max);
#endif
}

int wchar2dbcs(const char* code_page, const wchar_t* in,int in_len, char* out,int out_max)
{
#ifndef WIN32
    size_t result;
    iconv_t env;
    env = iconv_open(code_page,"WCHAR_T");
    result = iconv(env,(char**)&in,(size_t*)&in_len,(char**)&out,(size_t*)&out_max);
    iconv_close(env);
    return (int) result;
#else
    BOOL use_def_char;
    use_def_char = FALSE;
    return ::WideCharToMultiByte(codepage(code_page),0,in,in_len/sizeof(wchar_t),out,out_max,"?",&use_def_char);
#endif  
}

int wchar2utf8(const wchar_t* in,int in_len, char* out,int out_max)
{
#ifndef WIN32
    size_t result;
    iconv_t env;

    env = iconv_open("UTF8","WCHAR_T");
    result = iconv(env,(char**)&in,(size_t*)&in_len,(char**)&out,(size_t*)&out_max);    
    iconv_close(env);
    return (int) result;
#else
    BOOL use_def_char;
    use_def_char = FALSE;
    return ::WideCharToMultiByte(CP_UTF8,0,in,in_len/sizeof(wchar_t),out,out_max,NULL,NULL);
#endif
}

int utf82wchar(const char* in,int in_len, wchar_t* out,int out_max)
{
#ifndef WIN32
    size_t result;
    iconv_t env;
    env = iconv_open("WCHAR_T","UTF8");
    result = iconv(env,(char**)&in,(size_t*)&in_len,(char**)&out,(size_t*)&out_max);
    iconv_close(env);
    return (int) result;
#else
    return ::MultiByteToWideChar(CP_UTF8,0,in,in_len,out,out_max);
#endif
}

int dbcs2utf8(const char* code_page, const char* in, int in_len, 
        char* out, int out_max)
{
    wchar_t *tmps = (wchar_t*) malloc(in_len*sizeof(wchar_t));
    int ret;
    memset(tmps, 0, sizeof(wchar_t)*in_len);
    memset(out, 0, out_max);
    dbcs2wchar(code_page, in, in_len, tmps, in_len*sizeof(wchar_t));
    ret = wchar2utf8(tmps, wcslen(tmps)*sizeof(wchar_t), out, out_max);
    free(tmps);
    return ret;
}

int utf82dbcs(const char* code_page, const char* in, int in_len, 
        char* out, int out_max)
{
    wchar_t *tmps = (wchar_t*) malloc(in_len*sizeof(wchar_t));
    int ret;
    memset(tmps, 0, sizeof(wchar_t)*in_len);
    memset(out, 0, out_max);
    utf82wchar(in, in_len, tmps, in_len*sizeof(wchar_t));
    ret = wchar2dbcs(code_page, tmps, wcslen(tmps)*sizeof(wchar_t), out, out_max);
    free(tmps);
    return ret;
}

#ifdef _TEST
#include <stdio.h>

int main()
{
    unsigned char b[] = {
        0x41, 0xD6, 0xD0, 0xCE, 0xC4, 0xB2, 0xE2, 0xCA, 0xD4, 0x00
        /*0x41, 0x42, 0x43, 0x00*/
    };
    /* utf8 will spent more space */
    char out[sizeof(b)*2];
    dbcs2utf8("GBK", (char*) b, sizeof(b), out, sizeof(out));
    printf("utf8:%s, len:%d\n", out, strlen(out));

    char dbout[sizeof(out)];
    utf82dbcs("GBK", out, strlen(out)+1, dbout, sizeof(dbout));
    size_t dblen = strlen(dbout);
    size_t i = 0;
    for(; i < dblen; ++ i )
    {
        printf("0x%x, ", (unsigned char) dbout[i]);
    }
    printf("\n");
    return 0;
}

#endif

