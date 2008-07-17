#include <iostream>
#include <fstream>
#include <vector>
//#include <algorithm>
//#include <ctype.h>

#include <pcreposix.h>
#include "parser.h"
#include "data.h"

using namespace std;
//using namespace boost;



bool Parser::preg_match(const string& pattern,
                        const string& str,
                        vector<string>& matches)
{
    matches.clear();
    const int MAX_TOKENS = 100;
    regex_t parsingRule;

    int res = pcreposix_regcomp(&parsingRule, pattern.c_str(), REG_EXTENDED);
    if (res != 0) 
    {
        cout << "Err " << res << endl;
        return false;
    }


    regmatch_t match[MAX_TOKENS];// array for results of searching. 

    if(pcreposix_regexec(&parsingRule, str.c_str(), MAX_TOKENS, match, 0))
    {
        cerr << "ERROR pcreposix_regexec" << endl;
        return false;
    }


    int start_ = -1;
    int end_ = -1;

    // Positions of the first and the last symbols of the found substring
    // are in the first element off the array 'match'. 
    // We don't need the match[0] because it contains
    // result of matching of the whole regexp, so start with match[1].
    for(int i = 1; i < MAX_TOKENS; i++)
    {
        const int start = match[i].rm_so; // beginning of the found substring
        const int end = match[i].rm_eo;   // end of the found substring

        /* last token */
        if(start == -1 && end == -1)
            break;

        /* inbound token */
        if (start >= start_ && end <= end_ && start != end_ ) 
             continue;

        start_ = start;
        end_ = end;

        if(end - start == 0)
        {
            matches.push_back("");
        }
        else
        {
            string res(str);
            res = res.substr(start, end - start);
            matches.push_back(res);
        }

    }


     if(matches.empty())
     {
         cerr << "No matches found" << endl;
         return false;
     }

     //    for(unsigned i = 0; i<matches.size(); ++i)
     //     cout << "'" << matches[i]  << "'" << endl;


    regfree(&parsingRule);

    return true;
}


bool Parser::preg_match(const string& pattern,
                        const string& str,
                        string& match)
{
    vector<string> res;
    if(!preg_match(pattern, str, res))
    {
        match = "";
        return false;
    }

    match = res[0];
        
    return true;    

}



bool Parser::parseVar(const std::string& line, Variable& var)
{
    return preg_match(" *([a-zA-Z]+) *[;,=]", line, var.name);

}




