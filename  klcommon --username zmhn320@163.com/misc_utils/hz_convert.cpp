///
/// @file hz_convert.h
/// @brief Convert between GBK,GB2312,BIG5.
///
#include "hz_convert.h"
#include <string.h>
#include <windows.h>

void BIG52GBK(char *szBuf)
{
	int nStrLen = (int)strlen(szBuf);
	if( nStrLen == 0 )
		return;
	wchar_t *pws = new wchar_t[nStrLen + 1];
	int nReturn = MultiByteToWideChar(950, 0, szBuf, nStrLen, pws, nStrLen + 1);
	BOOL bValue = false;
	nReturn = WideCharToMultiByte(936, 0, pws, nReturn, szBuf, nStrLen + 1, "?", &bValue);
	szBuf[nReturn] = 0;
	delete [] pws;
}

void GBK2BIG5(char *szBuf)
{
	int nStrLen = (int)strlen(szBuf);
	if( nStrLen == 0 )
		return;
	wchar_t *pws = new wchar_t[nStrLen + 1];
	MultiByteToWideChar(936, 0, szBuf, nStrLen, pws, nStrLen + 1);
	BOOL bValue = false;
	WideCharToMultiByte(950, 0, pws, nStrLen, szBuf, nStrLen + 1, "?", &bValue);
	szBuf[nStrLen] = 0;
	delete [] pws;
}

void GB2GBK(char *szBuf)
{
	int nStrLen = (int)strlen(szBuf);
	if( nStrLen == 0 )
		return;
	DWORD wLCID = MAKELCID(MAKELANGID(LANG_CHINESE, SUBLANG_CHINESE_SIMPLIFIED), SORT_CHINESE_PRC);
	int nReturn = LCMapString(wLCID, LCMAP_TRADITIONAL_CHINESE, szBuf, nStrLen, NULL, 0);
	if(!nReturn)
		return;
	char *pcBuf = new char[nReturn + 1];
	wLCID = MAKELCID(MAKELANGID(LANG_CHINESE, SUBLANG_CHINESE_SIMPLIFIED), SORT_CHINESE_PRC);
	LCMapString(wLCID, LCMAP_TRADITIONAL_CHINESE, szBuf, nReturn, pcBuf, nReturn + 1);
	strncpy(szBuf, pcBuf, nReturn);
	delete [] pcBuf;
}

void GBK2GB(char *szBuf)
{
	int nStrLen = (int)strlen(szBuf);
	if( nStrLen == 0 )
		return;
	WORD wLCID = MAKELCID(MAKELANGID(LANG_CHINESE, SUBLANG_CHINESE_SIMPLIFIED), SORT_CHINESE_BIG5);
	int nReturn = LCMapString(wLCID, LCMAP_SIMPLIFIED_CHINESE, szBuf, nStrLen, NULL, 0);
	if(!nReturn)
		return;
	char *pcBuf = new char[nReturn + 1];
	wLCID = MAKELCID(MAKELANGID(LANG_CHINESE, SUBLANG_CHINESE_SIMPLIFIED), SORT_CHINESE_BIG5);
	LCMapString(wLCID, LCMAP_SIMPLIFIED_CHINESE, szBuf, nReturn, pcBuf, nReturn + 1);
	strncpy(szBuf, pcBuf, nReturn);
	delete [] pcBuf;
}