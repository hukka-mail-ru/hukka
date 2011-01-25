#if defined( __GNUG__ )
#pragma implementation
#endif

/*
 * rand.cc
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


#include <stdlib.h>
#include <time.h>

#if defined( USE_MT19937INT )
#include "mt19937int.h"
#endif

#include "rand.h"

namespace Random {
static bool seeded = false;
}

void
Random::setSeed(unsigned long const seed)
{
#ifdef WIN32
srand(seed);
#else
#if defined( USE_MT19937INT )
  sgenrand(seed);
#else
  srandom(seed);
#endif

#endif //WIN32
}

unsigned long
Random::get(void)
{
  if( ! seeded ) {
    setSeed(time(0));
    seeded = true;
  }
#ifdef WIN32
return rand();
#else

#if defined( USE_MT19937INT )
  return genrand();
#else
  return random();
#endif

#endif //WIN32
};
