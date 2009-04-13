#ifndef __SINGLETON__

#define __SINGLETON__

/*

* Usage: Inherit from singleton class like this:

* class Foo : public singleton<Foo> { ... };

*/

template<typename T> class singleton

{

public:

static T& inst()

{

static T object;

return object;

}

static T* ptr()

{

return &inst();

}

protected:

singleton() {};

virtual ~singleton() {};

};

#endif // __SINGLETON__

// End of file.
