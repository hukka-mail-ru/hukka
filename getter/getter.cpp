#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
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

void Print(const LinesVector& lines)
{
    for(unsigned i = 0; i<lines.size(); i++)
    {
        cout << lines[i]->text.c_str() << endl;
    }
    cout << " ==========================================" << endl;
}
//-------------------------------------------------

void CheckVariables(LinesVector& lines)
{
    
    for(unsigned i = 0; i<lines.size(); ++i)
    {
        string& text = lines[i]->text;
        
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
        
        // Then, it's a variable! Check it.
        lines[i]->check = true;
       
        // Can't move up
        if(i == 0)
            continue;

         // Move up and find its comment
        for(unsigned j = i-1; j>0; --j)
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
                lines[j]->check = true; // Check the comment.
            }
            else
            {
                break;
            }
        }
    }
}
//-------------------------------------------------

bool checked(shared_ptr<Line> line)
{
   return line->check; 
}

void MoveChecked(LinesVector& src, LinesVector& dst)
{

    for(unsigned i=0; i<src.size(); ++i) // change to algorithm!
    {
        if(src[i]->check)
        {
            dst.push_back(src[i]);
        }
    }

    src.erase ( remove_if(src.begin(), src.end(), checked),
                src.end() );
}


//-------------------------------------------------
enum Section
{
    HEAD,
    PUBLIC,
    PRIVATE,
    TAIL
};


//-------------------------------------------------
void Divide(const LinesVector& lines,
            LinesVector& head,
            LinesVector& pub,
            LinesVector& prv,
            LinesVector& tail)
{
 
    Section section = HEAD;
    for(unsigned i = 0; i<lines.size(); ++i)
    {
        string& text = lines[i]->text;

        switch(section)
        {
        case HEAD:
            if(text.find("public:") != string::npos)
            {
                section = PUBLIC;
            }
            else // still in head
            {
                head.push_back(lines[i]);
            }
            break;

        case PUBLIC:
            if(text.find("private:") != string::npos)
            {
                section = PRIVATE;
            }
            else if(text.find("};") != string::npos) // no private at all
            {
                section = TAIL;
            }
            else // still in public
            {
                pub.push_back(lines[i]);
            }        
            break;

        case PRIVATE:
            if(text.find("};") != string::npos) // no private at all
            {
                section = TAIL;
            }
            else // still in private
            {
                prv.push_back(lines[i]);
            }       
            break;

        case TAIL:
            tail.push_back(lines[i]);
            break;
        }
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

    LinesVector head;
    LinesVector prv;
    LinesVector pub;
    LinesVector tail;

    Divide(lines, head, pub, prv, tail);

    CheckVariables(pub);
    MoveChecked(pub, prv);


    // OUTPUT
    Print(head);
    cout << "public:" << endl;
    Print(pub);
    cout << "private:" << endl;
    Print(prv);
    cout << "};" << endl;
    Print(tail);


    return 0;
}
