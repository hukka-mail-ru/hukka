// -*- C++ -*-
#if !defined( STDUTIL_H )
#define STDUTIL_H

/*
 * stdutil.h
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

#include <cassert>
#include <string.h>

/// Compares @arg{s1} and @arg{s2}.

inline bool
streq(const char* s1, const char* s2)
{
  {                                                       assert( s1 && s2 ); }
  return strcmp(s1, s2) == 0;
}

/// Compares first @arg{n} characters of @arg{s1} and @arg{s2}.

inline bool
strneq(const char* s1, const char* s2, int n)
{
  {                                                       assert( s1 && s2 ); }
  return strncmp(s1, s2, n) == 0;
}

/// Compares @arg{s1} and @arg{s2}, ignoring case.

inline bool
strcaseeq(const char* s1, const char* s2)
{
  {                                                       assert( s1 && s2 ); }
#ifdef WIN32
  return (_stricoll(s1, s2) == 0);
#else
  return (strcasecmp(s1, s2) == 0);
#endif
}

/// Compares first @arg{n} characters of @arg{s1} and @arg{s2}, ignoring case.

inline bool
strncaseeq(const char* s1, const char* s2, int n)
{
  {                                                       assert( s1 && s2 ); }
#ifdef WIN32
  return (_strnicoll(s1, s2, n) == 0);
#else
  return (strncasecmp(s1, s2, n) == 0);
#endif
}

#endif
