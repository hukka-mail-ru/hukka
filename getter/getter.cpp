#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <boost/shared_ptr.hpp>
#include <ctype.h>

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
    //   cout << " ==========================================" << endl;
}

//-------------------------------------------------

void Push(LinesVector& lines, string str)
{
     shared_ptr<Line> line( new Line(str) ); 
     lines.push_back(line);
}


//-------------------------------------------------

void CheckComments(LinesVector& lines, int start)
{

    for(unsigned j = start; j>0; --j)
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

//-------------------------------------------------

bool CheckFunction(LinesVector& lines, const string& name)
{
    bool res = false;

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
        
        // Functions
        if(text.find("(") != string::npos && text.find(")") != string::npos)
        {
            if(text.find(name) != string::npos)
            {
                // The desired function found!
                res = true;

                lines[i]->check = true;
                if(i > 0)
                {
                    CheckComments(lines, i-1);
                }
            }
            else
            {
                continue;
            }
        }
        
    }

    return res;

}
//-------------------------------------------------


void CheckVariables(LinesVector& lines, bool with_comments = true, bool replace_ptr = false)
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
        
        // Replace * by Ptr
        if(replace_ptr && text.find("*") != string::npos)
        {
            const string ptr = "Ptr";

            text.replace(text.find("*"), size_t(1), ptr);
        }
       
        // Check comments
        if(i > 0 && with_comments)
        {
            CheckComments(lines, i-1);
        }

    }
}
//-------------------------------------------------


bool checked(const shared_ptr<Line>& line)
{
   return line->check; 
}


bool unchecked(const shared_ptr<Line>& line)
{
   return !line->check; 
}

void check(shared_ptr<Line>& line)
{
   line->check = true; 
}

void uncheck(shared_ptr<Line>& line)
{
   line->check = false; 
}

void MoveChecked(LinesVector& src, LinesVector& dst)
{
    remove_copy_if(src.begin(), src.end(), back_inserter(dst),
                   unchecked);

    src.erase ( remove_if(src.begin(), src.end(),
                               checked),  src.end() ); 

    for_each(dst.begin(), dst.end(), uncheck);
}

void CopyChecked(LinesVector& src, LinesVector& dst)
{
    for(unsigned i = 0; i<src.size(); ++i)
    {
        if(src[i]->check)
        {
            Push(dst, src[i]->text);
        }
    }

    for_each(dst.begin(), dst.end(), uncheck);
}

void CheckAll(LinesVector& lines)
{     
    for_each(lines.begin(), lines.end(), check);  
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


void MakeGetSet(const LinesVector& vars, LinesVector& pub,
                LinesVector& getters, LinesVector& setters)
{
    Push(getters, " ");
    Push(getters, "// =======  GETTERS ========= ");
    Push(getters, " ");

    Push(setters, " ");
    Push(setters, "// =======  SETTERS ========= ");
    Push(setters, " ");

    for(unsigned i = 0; i<vars.size(); ++i)
    { 
        string& var = vars[i]->text;

        // Trim
        size_t begin = var.find_first_not_of(" ");
        var = var.substr(begin, var.length() - begin); 

        // Get first word as "type", secord word as name (without ';')
        size_t end = var.find_first_of(" ");
        string type = var.substr(0, end); 
        string name = var.substr(end + 1, var.length()); // end + 1 is dangerous. Need to trim
        name.erase(remove(name.begin(), name.end(), ';'), name.end());

        
        string func_name = name; 
        func_name[0]= toupper(func_name[0]);
        string getter_name = "get" + func_name;
        string setter_name = "set" + func_name;

//        cout << "type: '" << type << "' name: '" 
//            << name <<  "' setter '" << setter_name << "'" << endl;

        // Make getter if it doesn't exist
        if(CheckFunction(pub, getter_name))
        {
            MoveChecked(pub, getters);
        }
        else
        {
            Push(getters, "    /*");
            Push(getters, "     * @brief Get value of " + name);
            Push(getters, "     * @return " + type);
            Push(getters, "     */");
            Push(getters, "    " + type + " " + getter_name + "() const");
            Push(getters, "    {"); 
            Push(getters, "        return " + name+ ";");
            Push(getters, "    }");  
            Push(getters, " "); 
        }
        
         // Make setter if it doesn't exist
        if(CheckFunction(pub, setter_name))
        {
            MoveChecked(pub, setters);
        }
        else
        {             
            Push(setters, "    /*");
            Push(setters, "     * @brief Set value of " + name);
            Push(setters, "     * @param[in] " + name);
            Push(setters, "     */");
            Push(setters, "    void " + setter_name + "(const " + type + " " + name + ")");
            Push(setters, "    {"); 
            Push(setters, "        this->" + name + " = " + name + ";");
            Push(setters, "    }");  
            Push(setters, " ");   
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
        Push(lines, str);
    }

    LinesVector head;
    LinesVector prv;
    LinesVector pub;
    LinesVector tail;

    Divide(lines, head, pub, prv, tail);

    // Move public vars to private
    CheckVariables(pub);
    MoveChecked(pub, prv);


    // Collect variables without comments and 'Ptr' instead of '*'
    LinesVector vars; 
    CheckVariables(prv, false, true); 
    CopyChecked(prv, vars);
    
    // Form getters and setters on the base of the "var"
    LinesVector setters;
    LinesVector getters;
    MakeGetSet(vars, pub, getters, setters); // different comments!

    // Move getters/setters to public
    CheckAll(setters);
    CheckAll(getters);

    MoveChecked(getters, pub);
    MoveChecked(setters, pub);



    Print(head);
    cout << "public:" << endl;
    Print(pub);
    cout << "private:" << endl;
    Print(prv);
    cout << "};" << endl;
    Print(tail);


    return 0;
}
