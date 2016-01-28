#pragma once

#define FSRASTERCALLCONV __cdecl

#ifdef _DLL
#define FSRASTEREXPORT __declspec(dllexport) 
#else
#define FSRASTEREXPORT __declspec(dllimport)  
#endif