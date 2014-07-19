
#include <curses.h>

#define FILLCOLOR 1

int main(int argc, char* argv[])
{
    initscr();
    start_color();
    cbreak();
    noecho();

    init_pair(FILLCOLOR, COLOR_WHITE, COLOR_BLACK);

    attrset(A_BOLD | COLOR_PAIR(FILLCOLOR));

    int maxy = 0;
    int maxx = 0;
    getmaxyx(stdscr, maxy, maxx);
    int x, y;
    for (y = 0; y < maxy; y++) {
        for (x = 0; x < maxx; x++) {
            mvaddch(y, x, ' ');
        }
    }
    refresh();

    bool done = false;
    while (!done) {
        int c = getch();
        switch (c) {
            case 'q': done = true; break;
            case 'r':
                init_pair(FILLCOLOR, COLOR_BLACK, COLOR_RED);
                refresh();
                break;

            case 'b':
                init_pair(FILLCOLOR, COLOR_BLACK, COLOR_BLUE);
                refresh();
                break;

            case 'g':
                init_pair(FILLCOLOR, COLOR_BLACK, COLOR_GREEN);
                refresh();
                break;

            case 'w':
                init_pair(FILLCOLOR, COLOR_BLACK, COLOR_WHITE);
                refresh();
                break;

            case 'n':
                init_pair(FILLCOLOR, COLOR_WHITE, COLOR_BLACK);
                refresh();
                break;
        }

    }

    endwin();
    return 0;
}

