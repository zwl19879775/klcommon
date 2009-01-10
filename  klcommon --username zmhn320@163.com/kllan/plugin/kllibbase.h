/**
 * @file kllibbase.h
 * @author Kevin Lynx
 * @brief to provide some basic functions in the script
 */
#ifndef ___KLLIB_BASE_H_
#define ___KLLIB_BASE_H_

struct klState;

/**
 * open the library.
 * this library provides these functions list below:
 * - print : to print a number or a string
 * - putc : to put a character to the screen
 * - input : get a input string/char/number from stdin [%s, %d, %c]
 */
int kllib_open_base( struct klState *kl );

#endif 
