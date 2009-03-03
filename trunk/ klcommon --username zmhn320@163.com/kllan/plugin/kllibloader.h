/**
 * @file kllibloader.h
 * @author Kevin Lynx
 * @brief to load dynamical library and import these functions in script 
 */
#ifndef ___KLLIB_LOADER_H_
#define ___KLLIB_LOADER_H_

struct klState;

/**
 * open the library.
 * this library provides these functions list below:
 * - import : import a dynamical library
 */
int kllib_open_loader( struct klState *kl );

#endif 

