#ifndef PARSER_H
#define PARSER_H

#include <boost/shared_ptr.hpp>
#include <vector>

//-------------------------------------------------
struct Attribute
{
    Attribute():
        type(""),
        name(""),
        comment(""),
        constant(false),
        ptr(false),
        ref(false)
    {}

    virtual ~Attribute() = 0;           

    std::string type;
    std::string name;
    std::string comment;
    bool constant;
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

typedef std::vector<boost::shared_ptr<Variable> > VectorVar;
typedef std::vector<boost::shared_ptr<std::string> > VectorString;

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
