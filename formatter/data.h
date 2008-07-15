#ifndef DATA_H
#define DATA_H

#include <boost/shared_ptr.hpp>
#include <vector>


//-------------------------------------------------

struct Class;
struct Attribute;

typedef std::vector<boost::shared_ptr<Class> >       VectorClass;
typedef std::vector<boost::shared_ptr<Attribute> >   VectorAttr;
typedef std::vector<boost::shared_ptr<Variable> >    VectorVar;
typedef std::vector<boost::shared_ptr<std::string> > VectorString;

struct Class
{
    std::string name;
    VectorClass parents;
    VectorAttr members;
};

//-------------------------------------------------

struct File
{
    VectorString includes;
    VectorString namespace;
    std::string namespace;
    Class class; 
};


//-------------------------------------------------

enum Storage
{
    ST_PRIVATE,
    ST_PROTECTED,
    ST_PUBLIC
};

enum Essence
{
    ES_VARIABLE,
    ES_FUNCTION
};


//-------------------------------------------------
struct Attribute
{
    Attribute():
        essence(Essence),
        type(""),
        name(""),
        comment(""),
        storage(ST_PUBLIC),
        constant(false),
        ptr(false),
        ref(false)
    {}

    virtual ~Attribute() = 0;           

    std::string type;
    std::string name;
    std::string comment;
    Storage storage;
    bool ptr;
    bool ref;
};

Attribute::~Attribute() {}

//-------------------------------------------------
struct Variable: public Attribute
{
    Variable():
        Attribute(),
        value("")
    {}

    std::string value;
};

//-------------------------------------------------
struct Function: public Attribute
{
    Function():
        Attribute(),
        virt(false)
    {}

    VectorVar args;
    VectorString impl;
    bool virt;

};



#endif
