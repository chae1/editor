#include "font.h"

using namespace font;

int main() {
    FontInfo font_info("/home/chaewon/Desktop/chae1/github/editor/client/src/font/UbuntuMono-R/");
    font_info.generate_font_buffers();
    font_info.print_font_buffers();
    
    return 0;
}
