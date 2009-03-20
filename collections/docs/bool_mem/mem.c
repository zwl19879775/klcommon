/* mem.c - Memory routines.
   Copyright (C) 2001 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.  */

/* Written by Marc Tardif <intmktg@cam.org>.  */

/* The memory allocation algorithm in this program can be summarized
   with the following illustration:

    contents      next                                           lim
       v            v                                             v
      +-----------------------------------------------------------+
      *
   pointer

   The pointer variable contains the address of the previously allocated
   object.  The first pointer is NULL to identify the object as the last
   element in the list.  This creates a singly linked list which can be
   traversed backwards.

   The memory block starts immediately after the pointer variable at
   the address of contents.  For the current active block of memory
   (the last allocated), two other pointers are maintained:
   - next, the address of the next available character.
   - lim, the end of the current memory block.

   Both the pointer and memory block are allocated in a single call to
   malloc.  This is accomplished by requesting memory past the contents
   variable and manipulating this area with the pointers mentioned above.
*/

#include <stdlib.h>
#include <unistd.h>

#include "err.h"
#include "mem.h"

/* Block of memory distributed in segments.                              */
struct block
{
  struct block *prev;         /* Address of prior block or NULL.         */
  char contents[4];           /* Objects begin here.                     */
};

/* Control current object in current block.                              */
struct mem
{
  void *block;                /* Address of current struct block.        */
  char *lim;                  /* Address of char after current block.    */
  char *next;                 /* Next char to current block.             */
  size_t size;                /* Size of memory block.                   */
};

static struct mem *mem;

void
mem_init (void)
{
  register struct block *block;
  size_t page_size;

  page_size = getpagesize ();
  mem = malloc (page_size);
  if (!mem)
    err_fatal ("memory exhausted");

  block = mem->block = (struct block *)
      ((char *)mem + ALIGN (sizeof (struct mem)));
  mem->next = block->contents;
  mem->lim = (char *) mem + page_size;
  mem->size = page_size;
  block->prev = NULL;
}

void *
mem_alloc (int length)
{
  register void *ptr;

  if (mem->next + length > mem->lim)
    {
      register struct block *block;
      register long new_size;

      mem->size *= 2;
      new_size = length + sizeof (struct block *);
      new_size += mem->size - (new_size % mem->size);

      block = malloc (new_size);
      if (!block)
	err_fatal ("memory exhausted");

      block->prev = mem->block;
      mem->block = block;
      mem->next = block->contents;
      mem->lim = (char *) block + new_size;
    }
  ptr = mem->next;
  mem->next = (char *)ALIGN (mem->next + length);
  return ptr;
}

void
mem_free (void)
{
  register struct block *lp, *plp;

  lp = mem->block;
  while (lp->prev != 0)
    {
      plp = lp->prev;
      free(lp);
      lp = plp;
    }
  free(mem);
}

