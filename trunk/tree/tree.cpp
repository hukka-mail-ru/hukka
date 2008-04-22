//#define _GNU_SOURCE 1
#include <sys/types.h>
#include <dirent.h>
#include <errno.h>
#include <sys/stat.h>
#include <unistd.h>
#include <vector>
#include <algorithm>
#include <string>
#include <iostream>

 



#include "color.hpp"
using namespace std;


#define SHOW_ERROR(X) \
        cerr << X << endl; \
        cerr << "Reason: " << strerror(errno) << endl << \
        "Source: " << __FILE__ << ", line " << __LINE__ << "(" << __func__ << ")" <<endl;

static int offset = 0;
vector<string> tail;
vector<string> files;



int process(char *local_dir, const char* desired_file)
{

    DIR *dp;
    dirent *ent;

    tail.push_back(local_dir);

    string dir = "";

    for(unsigned i = 0; i< tail.size(); i++)
        dir += tail[i] + "/";
///////////////////////////////


   if((dp = opendir(dir.c_str())) == NULL)
    {
        offset--;
        tail.pop_back();
        return 1;
    }

    errno = 0;
    while((ent = readdir(dp)) != NULL)
    {       
        string filename = ent->d_name;
        string fullname = dir + ent->d_name;

        struct stat st;
        stat(fullname.c_str(), &st);


        if(!S_ISDIR(st.st_mode) && filename != "." && filename != "..")
            files.push_back(ent->d_name);
    }

    if(errno != 0)
    {
        offset--;
        tail.pop_back();
        return 1;
    }
          

    if(closedir(dp) == -1)
        return 1;
    




/////////////////////////////////
    if((dp = opendir(dir.c_str())) == NULL)
    {
        offset--;
        tail.pop_back();
        return 1;
    }

    errno = 0;
    while((ent = readdir(dp)) != NULL)
    {       
        string filename = ent->d_name;
        string fullname = dir + ent->d_name;

        struct stat st;
        stat(fullname.c_str(), &st);

        if(files.size())
        {
            for(unsigned i=0; i < files.size(); i++)
            {
                if(desired_file != 0 && (
                   files[i] == desired_file || 
                   (files[i].find(desired_file) != files[i].npos)))
                    cout << color(YELLOW) << files[i] << "  ";   
                else
                    cout << color(WHITE) << files[i] << "  ";   
                cout << color() ;
            }

            files.erase(files.begin(),files.end());

        }

        string start = filename.substr(0,1);
        if(S_ISDIR(st.st_mode) && filename != "." && filename != ".." && filename != "." && start != ".")
        {
 
            cout << endl;

            for(int i=0; i < offset; i++)
                cout << "  ";
            cout << color(BLUE) << ent->d_name << color() << " " ;
                  

            offset++;
            process(ent->d_name, desired_file);
        }


    }

    if(errno != 0)
    {
        offset--;
        tail.pop_back();
        return 1;
    }
          

    if(closedir(dp) == -1)
        return 1;


    offset--;
    tail.pop_back();
    return 0;

}


int main(int argc, char* argv[])
{
    int errs = 0;

    if(argc < 2)
    {
        cerr << "Usage: tree DIR [FIND]";
        return -1;
    }


    cout << endl;

    if(argc == 2)
        process(argv[1], 0);
    else 
        process(argv[1], argv[2]);
   
    cout << endl << endl;

    return (errs != 0);
}
