// -*- C++ -*-
#if !defined( DEBUG_H )
#define DEBUG_H

/*
 * debug.h
 *
 * by Joseph Heled, 2002
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#if !defined(NDEBUG)

// GCC 3 supports macro variadic args in ISO C 1999 style.
// Assume same support for other compilers.

#if !defined(__GNUC__) || __GNUC__ == 3
#define ICC(...)  __VA_ARGS__
#else

// GCC Old style macro variadic args.

#define ICC(args...)  args
#endif  

#else

#ifdef WIN32

#define ICC(args) 

#else

#if !defined(__GNUC__) || __GNUC__ == 3
#define ICC(...)
#else

#define ICC(args...)

#endif

#endif

#endif

#endif //WIN32