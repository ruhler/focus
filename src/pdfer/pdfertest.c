
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
#include <stdio.h>

#include "consoler.h"

// Call this with first command line argument the path to the pdfer
// application to test, and second command line argument the path to the
// colors.pdf file 
// 
// The colors.pdf file has 8 pages, each 8.5" by 11" with a solid color:
//  black, blue, green, cyan, red, purple, yellow, white
int main(int argc, char* argv[])
{
    CNSL_Display display = CNSL_AllocDisplay(640, 480);
    CNSL_Client pdfer = CNSL_LaunchClient(argv[1], argv+1);
    CNSL_Color color;

    CNSL_SendEvent(pdfer, CNSL_MakeResize(640, 480));
    CNSL_RecvDisplay(pdfer, display, NULL, NULL, NULL, NULL);

    CNSL_SendEvent(pdfer, CNSL_MakeKeypress(CNSLK_w));
    CNSL_SendEvent(pdfer, CNSL_MakeKeyrelease(CNSLK_w));
    CNSL_RecvDisplay(pdfer, display, NULL, NULL, NULL, NULL);
    color = 0xFFFFFF & CNSL_GetPixel(display, 20, 30);
    fprintf(stderr, "%08x\n", color);
    assert(color == CNSL_MakeColor(0, 0, 0));

    CNSL_SendEvent(pdfer, CNSL_MakeKeypress(CNSLK_n));
    CNSL_SendEvent(pdfer, CNSL_MakeKeyrelease(CNSLK_n));
    CNSL_RecvDisplay(pdfer, display, NULL, NULL, NULL, NULL);
    color = 0xFFFFFF & CNSL_GetPixel(display, 20, 30);
    fprintf(stderr, "%08x\n", color);
    assert(color == CNSL_MakeColor(0, 0, 0xff));

    CNSL_SendEvent(pdfer, CNSL_MakeKeypress(CNSLK_n));
    CNSL_SendEvent(pdfer, CNSL_MakeKeyrelease(CNSLK_n));
    CNSL_RecvDisplay(pdfer, display, NULL, NULL, NULL, NULL);
    color = 0xFFFFFF & CNSL_GetPixel(display, 20, 30);
    fprintf(stderr, "%08x\n", color);
    assert(color == CNSL_MakeColor(0, 0xff, 0));

    // Test that 'j' vertical scroll is 10%
    CNSL_SendEvent(pdfer, CNSL_MakeKeypress(CNSLK_a));
    CNSL_SendEvent(pdfer, CNSL_MakeKeyrelease(CNSLK_a));
    CNSL_RecvDisplay(pdfer, display, NULL, NULL, NULL, NULL);
    CNSL_SendEvent(pdfer, CNSL_MakeKeypress(CNSLK_j));
    CNSL_SendEvent(pdfer, CNSL_MakeKeyrelease(CNSLK_j));
    CNSL_RecvDisplay(pdfer, display, NULL, NULL, NULL, NULL);
    color = 0xFFFFFF & CNSL_GetPixel(display, 320, 430);
    fprintf(stderr, "%08x\n", color);
    assert(color == CNSL_MakeColor(0, 0xff, 0));
    color = 0xFFFFFF & CNSL_GetPixel(display, 320, 434);
    fprintf(stderr, "%08x\n", color);
    assert(color != CNSL_MakeColor(0, 0xff, 0));

    // Test that 'd' vertical scroll is 50%
    CNSL_SendEvent(pdfer, CNSL_MakeKeypress(CNSLK_a));
    CNSL_SendEvent(pdfer, CNSL_MakeKeyrelease(CNSLK_a));
    CNSL_RecvDisplay(pdfer, display, NULL, NULL, NULL, NULL);
    CNSL_SendEvent(pdfer, CNSL_MakeKeypress(CNSLK_d));
    CNSL_SendEvent(pdfer, CNSL_MakeKeyrelease(CNSLK_d));
    CNSL_RecvDisplay(pdfer, display, NULL, NULL, NULL, NULL);
    color = 0xFFFFFF & CNSL_GetPixel(display, 320, 238);
    fprintf(stderr, "%08x\n", color);
    assert(color == CNSL_MakeColor(0, 0xff, 0));
    color = 0xFFFFFF & CNSL_GetPixel(display, 320, 242);
    fprintf(stderr, "%08x\n", color);
    assert(color != CNSL_MakeColor(0, 0xff, 0));

    // Test that 'u' vertical scroll is 50%
    CNSL_SendEvent(pdfer, CNSL_MakeKeypress(CNSLK_a));
    CNSL_SendEvent(pdfer, CNSL_MakeKeyrelease(CNSLK_a));
    CNSL_RecvDisplay(pdfer, display, NULL, NULL, NULL, NULL);
    CNSL_SendEvent(pdfer, CNSL_MakeKeypress(CNSLK_u));
    CNSL_SendEvent(pdfer, CNSL_MakeKeyrelease(CNSLK_u));
    CNSL_RecvDisplay(pdfer, display, NULL, NULL, NULL, NULL);
    color = 0xFFFFFF & CNSL_GetPixel(display, 320, 242);
    fprintf(stderr, "%08x\n", color);
    assert(color == CNSL_MakeColor(0, 0xff, 0));
    color = 0xFFFFFF & CNSL_GetPixel(display, 320, 238);
    fprintf(stderr, "%08x\n", color);
    assert(color != CNSL_MakeColor(0, 0xff, 0));

    // Test that 'f' vertical scroll is 90%
    CNSL_SendEvent(pdfer, CNSL_MakeKeypress(CNSLK_a));
    CNSL_SendEvent(pdfer, CNSL_MakeKeyrelease(CNSLK_a));
    CNSL_RecvDisplay(pdfer, display, NULL, NULL, NULL, NULL);
    CNSL_SendEvent(pdfer, CNSL_MakeKeypress(CNSLK_f));
    CNSL_SendEvent(pdfer, CNSL_MakeKeyrelease(CNSLK_f));
    CNSL_RecvDisplay(pdfer, display, NULL, NULL, NULL, NULL);
    color = 0xFFFFFF & CNSL_GetPixel(display, 320, 46);
    fprintf(stderr, "%08x\n", color);
    assert(color == CNSL_MakeColor(0, 0xff, 0));
    color = 0xFFFFFF & CNSL_GetPixel(display, 320, 50);
    fprintf(stderr, "%08x\n", color);
    assert(color != CNSL_MakeColor(0, 0xff, 0));

    // Test that 'b' vertical scroll is 90%
    CNSL_SendEvent(pdfer, CNSL_MakeKeypress(CNSLK_a));
    CNSL_SendEvent(pdfer, CNSL_MakeKeyrelease(CNSLK_a));
    CNSL_RecvDisplay(pdfer, display, NULL, NULL, NULL, NULL);
    CNSL_SendEvent(pdfer, CNSL_MakeKeypress(CNSLK_b));
    CNSL_SendEvent(pdfer, CNSL_MakeKeyrelease(CNSLK_b));
    CNSL_RecvDisplay(pdfer, display, NULL, NULL, NULL, NULL);
    color = 0xFFFFFF & CNSL_GetPixel(display, 320, 434);
    fprintf(stderr, "%08x\n", color);
    assert(color == CNSL_MakeColor(0, 0xff, 0));
    color = 0xFFFFFF & CNSL_GetPixel(display, 320, 430);
    fprintf(stderr, "%08x\n", color);
    assert(color != CNSL_MakeColor(0, 0xff, 0));

    // Test that 't' goes to top
    CNSL_SendEvent(pdfer, CNSL_MakeKeypress(CNSLK_w));
    CNSL_SendEvent(pdfer, CNSL_MakeKeyrelease(CNSLK_w));
    CNSL_RecvDisplay(pdfer, display, NULL, NULL, NULL, NULL);
    CNSL_SendEvent(pdfer, CNSL_MakeKeypress(CNSLK_t));
    CNSL_SendEvent(pdfer, CNSL_MakeKeyrelease(CNSLK_t));
    CNSL_RecvDisplay(pdfer, display, NULL, NULL, NULL, NULL);
    CNSL_SendEvent(pdfer, CNSL_MakeKeypress(CNSLK_k));
    CNSL_SendEvent(pdfer, CNSL_MakeKeyrelease(CNSLK_k));
    CNSL_RecvDisplay(pdfer, display, NULL, NULL, NULL, NULL);
    color = 0xFFFFFF & CNSL_GetPixel(display, 320, 50);
    fprintf(stderr, "%08x\n", color);
    assert(color == CNSL_MakeColor(0, 0xff, 0));
    color = 0xFFFFFF & CNSL_GetPixel(display, 320, 46);
    fprintf(stderr, "%08x\n", color);
    assert(color != CNSL_MakeColor(0, 0xff, 0));

    // Test that 'e' goes to bottom ("end")
    CNSL_SendEvent(pdfer, CNSL_MakeKeypress(CNSLK_w));
    CNSL_SendEvent(pdfer, CNSL_MakeKeyrelease(CNSLK_w));
    CNSL_RecvDisplay(pdfer, display, NULL, NULL, NULL, NULL);
    CNSL_SendEvent(pdfer, CNSL_MakeKeypress(CNSLK_e));
    CNSL_SendEvent(pdfer, CNSL_MakeKeyrelease(CNSLK_e));
    CNSL_RecvDisplay(pdfer, display, NULL, NULL, NULL, NULL);
    CNSL_SendEvent(pdfer, CNSL_MakeKeypress(CNSLK_j));
    CNSL_SendEvent(pdfer, CNSL_MakeKeyrelease(CNSLK_j));
    CNSL_RecvDisplay(pdfer, display, NULL, NULL, NULL, NULL);
    color = 0xFFFFFF & CNSL_GetPixel(display, 320, 330);
    fprintf(stderr, "%08x\n", color);
    assert(color == CNSL_MakeColor(0, 0xff, 0));
    color = 0xFFFFFF & CNSL_GetPixel(display, 320, 334);
    fprintf(stderr, "%08x\n", color);
    assert(color != CNSL_MakeColor(0, 0xff, 0));

    CNSL_SendEvent(pdfer, CNSL_MakeKeypress(CNSLK_q));
    CNSL_CloseClient(pdfer);
    return 0;
}

