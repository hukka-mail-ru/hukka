// -*- C++ -*-
#if !defined( SYSTYPES_H )
#define SYSTYPES_H

///
// Typically defined in @file{sys/types.h} for Unix-like systems. However,
// on other systems it is missing, and I try to avoid ifdef's if a reasonable
// alternative exists.
//
// If identical, multiple declaration are legal C++ @footnote{not 100% sure},
// so no harm is done if sys/types.h is included elsewhere.

typedef unsigned int uint;

#endif
