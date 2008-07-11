#include <iostream>
#include <fstream>
#include <vector>
#include <boost/shared_ptr.hpp>

using namespace std;
using namespace boost;

//-------------------------------------------------
struct Line
{
    Line(string t): text(t), check(false) 
    {}

    string text;
    bool check;   	
};
//-------------------------------------------------

typedef vector<shared_ptr<Line> > LinesVector;

//-------------------------------------------------

void CheckPublicVariables(LinesVector& lines)
{
    bool public_found = false;
 
    for(unsigned i = 0; i<lines.size(); ++i)
    {
        string& text = lines[i]->text;
        
        // Move from "public:"
        if(!public_found)
        {
           if(text.find("public:") != string::npos)
           {
               public_found = true;
           }
           continue;
        }

        // To "private:" or end-of-class
        if(text.find("private:")  != string::npos ||
           text.find("};") != string::npos )
        {
            break;
        }

        // No comments
        size_t first = text.find_first_not_of(" ");
        if(first == string::npos ||
           text[first] == '/' ||
           text[first] == '*' ||
           text[first] == '#')
        {
            continue;
        }
        
        // No functions
        if(text.find("(") != string::npos)
        {
            continue;
        }
        
        // Then, it's a variable!
        lines[i]->check = true;

        // Move up and find its comment
        for(unsigned j = i-1; ; --j)
        {
            string& text = lines[j]->text;

            size_t first = text.find_first_not_of(" ");
            if(first == string::npos)
            {
                continue;
            }

            // is it a comment ?
            if(text.find("/*") != string::npos ||
               text[first] == '*')               
            {
                lines[j]->check = true;
            }
            else
            {
                break;
            }
        }
    }
}
//-------------------------------------------------

void MoveToPrivate(LinesVector& lines)
{
    LinesVector temp;
    bool private_found = false;

    for(unsigned i = 0; i<lines.size(); ++i)
    {
        string& text = lines[i]->text;


        // To "private:" or end-of-class
        if(text.find("private") != string::npos)
        {
            private_found = true;
        }

        // Insert checked lines before the end of class
        if(text.find("};") != string::npos)
        {
            if(!private_found) // also insert "private:" if it's missing
            {
                shared_ptr<Line> line( new Line("private:") );
                temp.push_back(line);
            }

            // now the checked lines...
            for(unsigned j = 0; j<lines.size(); ++j)
            {
                if(lines[j]->check == true)
                {
                    temp.push_back(lines[j]);
                }
            }

            // Add };
            shared_ptr<Line> line( new Line("};") );
            temp.push_back(line);
        }
        else // if it's still not the end of class
        {
            if(lines[i]->check == false)
            {
                temp.push_back(lines[i]);
            }
        }
    }

    lines = temp; // write result to main vector

}


//-------------------------------------------------

void MakePrivate(LinesVector& lines)
{
    CheckPublicVariables(lines);
    MoveToPrivate(lines);

    for(unsigned i = 0; i<lines.size(); ++i)
    {
        cout << lines[i]->text << endl;
    }

}

//-------------------------------------------------

int main()
{
    LinesVector lines;

    // Read form file
    ifstream file("header.h");
    string str;
    while(getline(file, str))
    {
        shared_ptr<Line> line( new Line(str) ); 
        lines.push_back(line);

    }


    for(unsigned i = 0; i<lines.size(); i++)
        cout << lines[i]->text.c_str() << endl;

    MakePrivate(lines);

    return 0;
}
