#ifndef __color_hpp__
#define __color_hpp__

extern bool EnableColorLog;


class Fucker
{
 public:

/*
 * The FunctionWithComment comment
 */
    int FunctionWithComment(int h);

    int myVariableWithoutComment;
 
/*
 * The myVariableWithComment comment
 */
    int myVariableWithComment;


    int FunctionWithoutComment(int h);

/*
 * Original myVariableWithComment getter
 */
    int getMyVariableWithComment();

/*
 * Original myVariableWithoutComment setter
 */
    void setMyVariableWithoutComment(int i);

};


#endif
