#pragma once
#include "resourceppc.h"

#ifndef _TUTORIAL1_H
#define _TUTORIAL1_H

#include <windows.h> //needed include for window system calls
#include <GLES/gl.h>
#include <GLES/egl.h>


#define PRECISION 16	
#define ONE	(1 << PRECISION)
#define ZERO 0
inline GLfixed FixedFromInt(int value) {return value << PRECISION;};

int WINAPI WinMain(HINSTANCE hInst, HINSTANCE hPrevInst, LPTSTR lpCmdLine,int nCmdShow);
LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);
BOOL InitOGLES();// Our GL initialization function
void Render();  // Our Render function
void Clean();   //Our clean function. It will clean all used resources

#endif

