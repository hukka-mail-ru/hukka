#ifndef __window_h__
#define __window_h__

enum EDR_EventType
{
    EVENT_LEFTMOUSEBUTTONDOWN,
    EVENT_RIGHTMOUSEBUTTONDOWN,
    EVENT_QUIT
};

struct EDR_Button
{
    int x;
    int y;
};

struct EDR_Event
{
    EDR_EventType type;
    EDR_Button button;
};

void EDR_CreateWindow(int width, int height, const char *name);
void EDR_SwapBuffers();
bool EDR_PollEvent(EDR_Event& event);



#endif
