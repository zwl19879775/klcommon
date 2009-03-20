/* mem.h - interface to mem.c
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

#ifndef MEM_H
#define MEM_H

#define ALIGNBYTES      (sizeof (int) - 1)
#define ALIGN(p)        (((unsigned)(p) + ALIGNBYTES) & ~ALIGNBYTES)

extern void  mem_init  (void);
extern void *mem_alloc (int);
extern void  mem_free  (void);

#endif
