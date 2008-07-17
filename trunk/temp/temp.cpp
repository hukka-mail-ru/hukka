#include <iostream>
#include <algorithm>
#include <vector>
#include <iterator>

#include <pcreposix.h>

using namespace std;



int main()
{
    vector<string> result;
    const int MAX_TOKENS = 10;

    regex_t parsingRule;

    const char* pattern = " *([a-zA-Z]+) *[;]";

    int res = pcreposix_regcomp(&parsingRule, pattern, REG_EXTENDED);
    if (res != 0) 
    {
        cout << "Err " << res << endl;
        return -1;
    }


    regmatch_t match[MAX_TOKENS];// array for results of searching. 

    const char* str = "   const   int var ;";
    if(pcreposix_regexec(&parsingRule, str, MAX_TOKENS, match, 0))
    {
        cerr << "Err pcreposix_regexec" << endl;
        return -1;
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
            result.push_back("");
        }
        else
        {
            string res(str);
            res = res.substr(start, end - start);
            result.push_back(res);
        }

    }


    if(result.empty())
        cout << "No matches found" << endl;

    for(unsigned i = 0; i<result.size(); ++i)
        cout << "'" << result[i]  << "'" << endl;


    regfree(&parsingRule);


}
