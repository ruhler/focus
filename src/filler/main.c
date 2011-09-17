
// Copyright (C) 2011 Richard Uhler <ruhler@member.fsf.org>
//
// This file is part of Focus.
//
// Focus is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Focus is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Focus.  If not, see <http://www.gnu.org/licenses/>.

#include <stdio.h>
#include <tcl.h>

#include "filler.h"

bool done;
Filler filler;
int width;
int height;

int quit_cmd(ClientData cd, Tcl_Interp* interp, int objc, Tcl_Obj* const objv[])
{
    if (objc != 1) {
        Tcl_WrongNumArgs(interp, 1, objv, "");
        return TCL_ERROR;
    }

    done = true;
    return TCL_OK;
}

int color_cmd(ClientData cd, Tcl_Interp* interp, int objc, Tcl_Obj* const objv[])
{
    if (objc != 1) {
        Tcl_WrongNumArgs(interp, 1, objv, "");
        return TCL_ERROR;
    }

    CNSL_Color* color = (CNSL_Color*)cd;
    Filler_FillWith(filler, *color);
    return TCL_OK;
}

int zoom_cmd(ClientData cd, Tcl_Interp* interp, int objc, Tcl_Obj* const objv[])
{
    if (objc != 3) {
        Tcl_WrongNumArgs(interp, 1, objv, "xfactor yfactor");
        return TCL_ERROR;
    }

    double xf, yf;
    Tcl_GetDoubleFromObj(interp, objv[1], &xf);
    Tcl_GetDoubleFromObj(interp, objv[2], &yf);

    width = (int)(width * xf);
    height = (int)(height * yf);
    Filler_Resize(filler, width, height);
    return TCL_OK;
}
    

int main(int argc, char* argv[])
{
    if (argc > 1 && strcmp(argv[1], "--version") == 0) {
        printf("filler %s\n", FOCUS_VERSION_STRING);
        return 0;
    }

    if (argc > 1 && strcmp(argv[1], "--help") == 0) {
        printf("Usage: filler\n");
        printf("An application to color the screen\n");
        printf("\n");
        printf("Options\n");
        printf("  --help       output this help message and exit\n");
        printf("  --version    output version information and exit\n");
        printf("\n");
        return 0;
    }

    CNSL_Event event = CNSL_RecvEvent(stdcon);
    if (!CNSL_IsResize(event, &width, &height)) {
        fprintf(stderr, "filler: expected resize event. Got %i\n", event.type);
        return 1;
    }

    CNSL_Color black = CNSL_MakeColor(0, 0, 0);
    CNSL_Color red = CNSL_MakeColor(255, 0, 0);
    CNSL_Color green = CNSL_MakeColor(0, 255, 0);
    CNSL_Color blue = CNSL_MakeColor(0, 0, 255);
    CNSL_Color cyan = CNSL_MakeColor(0, 255, 255);
    CNSL_Color yellow = CNSL_MakeColor(255, 255, 0);
    CNSL_Color purple = CNSL_MakeColor(255, 0, 255);
    CNSL_Color white = CNSL_MakeColor(255, 255, 255);

    CNSL_Keysym sym;
    done = false;
    filler = Filler_Create(width, height, black, stdcon);

    Tcl_Interp* interp = Tcl_CreateInterp();
    Tcl_CreateObjCommand(interp, "quit", quit_cmd, NULL, NULL);
    Tcl_CreateObjCommand(interp, "black", color_cmd, &black, NULL);
    Tcl_CreateObjCommand(interp, "red", color_cmd, &red, NULL);
    Tcl_CreateObjCommand(interp, "green", color_cmd, &green, NULL);
    Tcl_CreateObjCommand(interp, "blue", color_cmd, &blue, NULL);
    Tcl_CreateObjCommand(interp, "cyan", color_cmd, &cyan, NULL);
    Tcl_CreateObjCommand(interp, "yellow", color_cmd, &yellow, NULL);
    Tcl_CreateObjCommand(interp, "purple", color_cmd, &purple, NULL);
    Tcl_CreateObjCommand(interp, "white", color_cmd, &white, NULL);
    Tcl_CreateObjCommand(interp, "zoom", zoom_cmd, NULL, NULL);

    while (!done) {
        event = CNSL_RecvEvent(stdcon);
        if (CNSL_IsQuit(event)) {
            fprintf(stderr, "filler: quit\n");
            done = true;
        } else if (CNSL_IsResize(event, &width, &height)) {
            fprintf(stderr, "filler: resize %i, %i\n", width, height);
            Filler_Resize(filler, width, height);
        } else if (CNSL_IsKeypress(event, &sym)) {
            fprintf(stderr, "filler: keypress: %c(%i)\n", sym, sym);
            switch (sym) {
                case CNSLK_r: Tcl_Eval(interp, "red"); break;
                case CNSLK_g: Tcl_Eval(interp, "green"); break;
                case CNSLK_b: Tcl_Eval(interp, "blue"); break;
                case CNSLK_c: Tcl_Eval(interp, "cyan"); break;
                case CNSLK_y: Tcl_Eval(interp, "yellow"); break;
                case CNSLK_p: Tcl_Eval(interp, "purple"); break;
                case CNSLK_w: Tcl_Eval(interp, "white"); break;
                case CNSLK_n: Tcl_Eval(interp, "black"); break;
                case CNSLK_q: Tcl_Eval(interp, "quit"); break;
                case CNSLK_d: Tcl_Eval(interp, "zoom 2 2"); break;
                case CNSLK_h: Tcl_Eval(interp, "zoom 0.5 0.5"); break;
            }
        }
    }

    Tcl_DeleteInterp(interp);
    Filler_Free(filler);

    return 0;
}
