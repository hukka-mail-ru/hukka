// -*- C++ -*-
#if !defined( COMPILER_H )
#define COMPILER_H

#if defined( __GNUC__ )

// GNU extension - local variable sized arrays.

#define LOCAL_ARRAY(type, name, n) type name [ n ]

#else

namespace Hacks {
  // A small class that allocates a simple array and releses it when going
  // out of scope.
  //
  // auto_ptr is not applicable since it does a delete and not 'delete []'

  template <typename T> class ArrayAutoPtr {
  public:
    ArrayAutoPtr(unsigned int n) : ptr(new T [n]) {}
    
    ~ArrayAutoPtr() { delete [] ptr; }

    operator T*(void) const { return ptr; }
    
  private:
    T*	const ptr;
  };
}

#define LOCAL_ARRAY(type, name, n) Hacks::ArrayAutoPtr<type> name(n);

#endif


#endif
