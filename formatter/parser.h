#ifndef PARSER_H
#define PARSER_H

#include <string>
#include "data.h"

class Parser
{ 
    friend class TestParser;

 public:
    
    Parser() 
    {}

    ~Parser()
    {}

    bool parseFile(const std::string& fileName, File& file);

 private:

    Parser(const Parser&);

    Parser& operator=(const Parser&);

    bool preg_match(const std::string& pattern,
                    const std::string& str,
                    std::vector<std::string>& matches);

    bool defineEssence(const std::string& line, Essence essence);
    

    bool parseVar(const std::string& line, Variable& var)
    {
        return false;
    }

    bool parseFunc(const std::string& line, Function& var)
    {
        return false;
    }


    bool getVarName(const std::string& line, std::string& name);


    bool getVarType(const std::string& line, std::string& type)
    {
        return false;
    }

    bool getVarValue(const std::string& line, std::string& val)
    {
        return false;
    }


    bool getFuncName(const std::string& line, std::string& name)
    {
        return false;
    }

    bool getFuncType(const std::string& line, std::string& type)
    {
        return false;
    }

    bool getFuncArgs(const std::string& line, VectorVar& args)    
    {
        return false;
    }

    bool getFuncImpl(const std::string& line, VectorString& impl)
    {
        return false;
    }

};

#endif
