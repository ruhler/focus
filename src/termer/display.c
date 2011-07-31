
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

#include <assert.h>
#include <stdlib.h>

#include "display.h"

DISPLAY_Display DISPLAY_Alloc()
{
    int width;
    int height;
    CNSL_Event event = CNSL_RecvEvent(stdcon);
    if (!CNSL_IsResize(event, &width, &height)) {
        fprintf(stderr, "termer: expected resize event. Got %i\n", event.type);
        return NULL;
    }

    DISPLAY_Display display = malloc(sizeof(DISPLAY_Display_));
    if (display == NULL) {
        return NULL;
    }

    display->display = CNSL_AllocDisplay(width, height);

    const char* font = getenv("TERMERFONT");
    if (!font) {
        font = "Monospace";
    }

    display->fonter = FNTR_Create(font);
    display->cell_width = FNTR_MaxWidth(display->fonter);
    display->cell_height = FNTR_Height(display->fonter);

    display->columns = width / display->cell_width;
    display->lines = height / display->cell_height;

    display->maxcol = -1;
    display->mincol = -1;
    display->maxrow = -1;
    display->minrow = -1;

    return display;
}

void DISPLAY_Free(DISPLAY_Display display)
{
    FNTR_Free(display->fonter);
    CNSL_FreeDisplay(display->display);
    free(display);
}

int DISPLAY_Lines(DISPLAY_Display display)
{
    return display->lines;
}

int DISPLAY_Columns(DISPLAY_Display display)
{
    return display->columns;
}

// convert a screen color to truecolor.
CNSL_Color colorof(int color, bool bold)
{
    int red = color & SCREEN_COLOR_RED ? (bold ? 0xFF : 0xAF) : 0;
    int green = color & SCREEN_COLOR_GREEN ? (bold ? 0xFF : 0xAF) : 0;
    int blue = color & SCREEN_COLOR_BLUE ? (bold ? 0xFF : 0xAF) : 0;
    return CNSL_MakeColor(red, green, blue);
}

void DISPLAY_DrawCell(DISPLAY_Display display,
        SCREEN_Position pos, const SCREEN_Cell* cell)
{
    int col = pos.column;
    int row = pos.line;

    if (col < 0 || col >= display->columns
            || row < 0 || row >= display->lines) {
        return;
    }

    // expand the display update rectangle if needed.
    if (display->mincol == -1 || display->mincol > col) {
        display->mincol = col;
    }
    if (display->maxcol == -1 || display->maxcol < col) {
        display->maxcol = col;
    }
    if (display->minrow == -1 || display->minrow > row) {
        display->minrow = row;
    }
    if (display->maxrow == -1 || display->maxrow < row) {
        display->maxrow = row;
    }

    int fgc = cell->cattrs.style.reverse ? cell->cattrs.bgcolor : cell->cattrs.fgcolor;
    int bgc = cell->cattrs.style.reverse ? cell->cattrs.fgcolor : cell->cattrs.bgcolor;
    CNSL_Color fg = colorof(fgc, cell->cattrs.style.bold);
    CNSL_Color bg = colorof(bgc, cell->cattrs.style.bold);

    int x = col * display->cell_width;
    int y = row * display->cell_height;

    FNTR_LoadGlyph(display->fonter, cell->character);
    FNTR_DrawGlyph(display->fonter, display->display, fg, bg, x, y);
}

void DISPLAY_Show(DISPLAY_Display display)
{
    int x = 0;
    int y = 0;
    int w = 0;
    int h = 0;

    if (display->mincol != -1) {
        assert(display->maxcol >= 0);
        assert(display->minrow >= 0);
        assert(display->maxrow >= 0);
        assert(display->maxcol >= display->mincol);
        assert(display->maxrow >= display->minrow);

        x = display->cell_width * display->mincol;
        y = display->cell_height * display->minrow;
        w = display->cell_width * (display->maxcol - display->mincol + 1);
        h = display->cell_height * (display->maxrow - display->minrow + 1);

        CNSL_SendDisplay(stdcon, display->display, x, y, x, y, w, h);
    }

    display->mincol = -1;
    display->maxcol = -1;
    display->minrow = -1;
    display->maxrow = -1;
}

void DISPLAY_Resize(DISPLAY_Display display, int width, int height)
{
    CNSL_Display ndisplay = CNSL_AllocDisplay(width, height);

    int oldwidth = display->display.width;
    int oldheight = display->display.height;
    int w = width < oldwidth ? width : oldwidth;
    int h = height < oldheight ? height : oldheight;

    int x, y;
    for (y = 0; y < h; y++) {
        for (x = 0; x < w; x++) {
            CNSL_SetPixel(ndisplay, x, y, CNSL_GetPixel(display->display, x, y));
        }
    }

    CNSL_FreeDisplay(display->display);
    display->display = ndisplay;

    display->columns = width / display->cell_width;
    display->lines = height / display->cell_height;

    if (display->maxrow >= display->lines) {
        display->maxrow = display->lines-1;
    }

    if (display->maxcol >= display->columns) {
        display->maxcol = display->columns-1;
    }
}

