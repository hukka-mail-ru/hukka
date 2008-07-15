#ifndef PARSER_H
#define PARSER_H

#include <string>
#include <data.h>

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

    operator=(const Parser&);


    bool defineEssence(const std::string& line, Essence essence);
    

    bool parseVar(const std::string& line, Variable& var);

    bool parseFunc(const std::string& line, Function& var);


    bool getVarName(const std::string& line, std::string& name);

    bool getVarType(const std::string& line, std::string& type);

    bool getVarValue(const std::string& line, std::string& val);


    bool getFuncName(const std::string& line, std::string& name);

    bool getFuncType(const std::string& line, std::string& type);

    bool getFuncArgs(const std::string& line, VectorVar& args);

    bool getFuncImpl(const std::string& line, VectorString& impl);

};

#endif
