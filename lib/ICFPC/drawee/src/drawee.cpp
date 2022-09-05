#define _CRT_SECURE_NO_WARNINGS
#define DATA_PATH "data/"

// defer is from https://gist.github.com/andrewrk/ffb272748448174e6cdb4958dae9f3d8
#define CONCAT_INTERNAL(x,y) x##y
#define CONCAT(x,y) CONCAT_INTERNAL(x,y)

template<typename T>
struct ExitScope {
    T lambda;
    ExitScope(T lambda):lambda(lambda){}
    ~ExitScope(){lambda();}
    ExitScope(const ExitScope&);
  private:
    ExitScope& operator =(const ExitScope&);
};

class ExitScopeHelp {
  public:
    template<typename T>
        ExitScope<T> operator+(T t){ return t;}
};

#define defer const auto& CONCAT(defer__, __LINE__) = ExitScopeHelp() + [&]()

#ifdef _MSC_VER
#define FUNCTION_NAME __FUNCTION__
#else
#define FUNCTION_NAME ""
#endif

#define den_warning(Expr) if (!(Expr)) SDL_Log("den_warning! %s " #Expr "\n", FUNCTION_NAME);
#define log_warning(Expr, Format, ...) if (!(Expr)) SDL_Log("den_warning! %s " #Expr " " Format "\n", FUNCTION_NAME, __VA_ARGS__);

#include <assert.h>

#ifdef _MSC_VER
#include "SDL.h"
#include "SDL_ttf.h"
#else
#include <SDL2\SDL.h>
#include <SDL2\SDL_ttf.h>
#endif
#include <chrono>
#include <string>
#include <cmath>
#include <vector>
#include <map>
#include <string>
#include <sstream>
#define INLUDE_JSON 1
#if INLUDE_JSON
#include "json.hpp"
#endif

using u8 = uint8_t;
using u16 = uint16_t;
using u32 = uint32_t;
using u64 = uint64_t;

using u16 = uint16_t;
using s16 = int16_t;

SDL_Window   *g_window;
#define WIN_W 900
#define WIN_H 500
#define PIC_W 400
#define PIC_H 400
#define PS 2
#define X 0
#define Y 1
#define SNAP_GRID_SIZE 10
int g_canvas_zero_lower_left[2] = {(0)*PS, (WIN_H)*PS};
SDL_Renderer *g_renderer;
TTF_Font     *g_font;

void main_loop();
void read_blocks(char const* filename);
void read_commands(char const* filename);

int main(int const n_args, char ** const args) {
    SDL_Log("Usage:\n"
            "    %s <blocks.json> <swaps.isl> \n"
            "    or %s", args[0], args[0]);
    if(SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO) == -1) {SDL_Log("Could not initialize SDL: %s.\n", SDL_GetError()); return -1;}
    defer {SDL_Quit();};

    #if INLUDE_JSON
    if (n_args == 3) {
        read_blocks(args[1]);
        read_commands(args[2]);
    }
    #endif
    main_loop();

    return 0;
}

bool init_sdl() {
    bool success = false;
    g_window  = SDL_CreateWindow("Powder Team icfpc2022", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, WIN_W*PS, WIN_H*PS, SDL_WINDOW_OPENGL); if (!g_window) {SDL_Log("Could not create window: %s\n", SDL_GetError());     return false;}
    defer {if (!success) SDL_DestroyWindow(g_window);};

    g_renderer = SDL_CreateRenderer(g_window, -1, SDL_RENDERER_ACCELERATED);                                                   if (!g_renderer) {SDL_Log("Could not create rendered: %s\n", SDL_GetError()); return false;}
    defer {if (!success) SDL_DestroyRenderer(g_renderer);};
    SDL_SetRenderDrawBlendMode(g_renderer, SDL_BLENDMODE_BLEND);

    int ttf_init_result = TTF_Init();                                                                                                        if (ttf_init_result == -1) {SDL_Log("Could not init TTF: %s\n", SDL_GetError());     return false;}
    defer {if (!success) TTF_Quit();};

    //char const *font_path = DATA_PATH "Inconsolata-Regular.ttf";
    char const *font_path = DATA_PATH "Poppins-Regular.ttf";
    g_font = TTF_OpenFont(font_path, 36);                                                                                            if (!g_font) {SDL_Log("Can't open font %s: %s", font_path, SDL_GetError());    return false;}
    defer {if (!success) TTF_CloseFont(g_font);};

    success = true;
    return success;
}

void close_sdl() {
    TTF_CloseFont(g_font);
    TTF_Quit();
    SDL_DestroyRenderer(g_renderer);
    SDL_DestroyWindow(g_window);
}

void pixel_to_pos(int px, int py, int *rx, int *ry) {
    *rx = (g_canvas_zero_lower_left[X] + px)/PS;
    *ry = (g_canvas_zero_lower_left[Y] - py)/PS;
}

int pixel_x_to_pos(int px) {
    return (g_canvas_zero_lower_left[X] + px)/PS;
}

int pixel_y_to_pos(int py) {
    return (g_canvas_zero_lower_left[Y] - py)/PS;
}

void pos_to_upper_left_pixel(int x, int y, int *rx, int *ry) {
    *rx = g_canvas_zero_lower_left[X] + x*PS;
    *ry = g_canvas_zero_lower_left[Y] - (y+1)*PS + 1;
}

void pos_to_lower_left_pixel(int x, int y, int *rx, int *ry) {
    *rx = g_canvas_zero_lower_left[X] + x*PS;
    *ry = g_canvas_zero_lower_left[Y] - y*PS;
}

void pos_to_upper_right_pixel(int x, int y, int *rx, int *ry) {
    *rx = g_canvas_zero_lower_left[X] + (x+1)*PS - 1;
    *ry = g_canvas_zero_lower_left[Y] - (y+1)*PS + 1;
}

void pos_to_lower_right_pixel(int x, int y, int *rx, int *ry) {
    *rx = g_canvas_zero_lower_left[X] + (x+1)*PS - 1;
    *ry = g_canvas_zero_lower_left[Y] - y*PS;
}

void pos_rect_to_pixel_rect(int x0, int y0, int x1, int y1, SDL_Rect *rect) {
    pos_to_upper_left_pixel(x0, y1, &rect->x, &rect->y);
    int lrx, lry;
    pos_to_lower_right_pixel(x1, y0, &lrx, &lry);
    rect->w = lrx - rect->x + 1;
    rect->h = lry - rect->y + 1;
}

void highlight_rect(u8 R, u8 G, u8 B, u8 A, int x0, int y0, int x1, int y1) {
    SDL_Rect rect;
    pos_rect_to_pixel_rect(x0, y0, x1, y1, &rect);
    SDL_SetRenderDrawColor(g_renderer, R, G, B, A);
    SDL_RenderFillRect(g_renderer, &rect);
}

void draw_text(int x, int y, char const* format, ...) {
    char text[1000];
    {
        va_list args;
        va_start (args, format);
        vsnprintf(text, 1000, format, args);
        perror (text);
        va_end (args);
    }
    SDL_Rect message_rect;
    message_rect.x = x + 2;
    message_rect.y = y + 2;
    TTF_SizeText(g_font, text, &message_rect.w, &message_rect.h);

    {
        SDL_Surface* surfaceMessage = TTF_RenderText_Solid(g_font, text, {128, 128, 128}); if (!surfaceMessage) {SDL_Log("draw_text error: %s", SDL_GetError()); return;}
        defer {SDL_FreeSurface(surfaceMessage);};

        SDL_Texture* texture = SDL_CreateTextureFromSurface(g_renderer, surfaceMessage);   if (!texture) {SDL_Log("SDL_CreateTextureFromSurface: %s", SDL_GetError()); return;}
        defer {SDL_DestroyTexture(texture);};

        SDL_RenderCopy(g_renderer, texture, NULL, &message_rect);
    }

    message_rect.x = x;
    message_rect.y = y;

    {
        SDL_Surface* surfaceMessage = TTF_RenderText_Solid(g_font, text, {255, 255, 255}); if (!surfaceMessage) {SDL_Log("draw_text error: %s", SDL_GetError()); return;}
        defer {SDL_FreeSurface(surfaceMessage);};

        SDL_Texture* texture = SDL_CreateTextureFromSurface(g_renderer, surfaceMessage);   if (!texture) {SDL_Log("SDL_CreateTextureFromSurface: %s", SDL_GetError()); return;}
        defer {SDL_DestroyTexture(texture);};

        SDL_RenderCopy(g_renderer, texture, NULL, &message_rect);
    }
}

enum Axis {
    VERTICAL = X,
    HORIZONTAL = Y
};

enum Command_Type {
    cNONE = 0,
    cCUT_LINE,
    cCUT_POINT,
    cSET_COLOR,
    cSWAP,
    cMERGE
};
Command_Type current_command;
bool command_is_ready = false;
Axis current_axis = VERTICAL;
int       saved_x = 0;
int       saved_y = 0;
SDL_Color saved_color;
bool click = false;
#define MOUSE_DEFAULT (PIC_W + 2)
int mouse_x = MOUSE_DEFAULT;
int mouse_y = MOUSE_DEFAULT;


struct Command {
    Command_Type type = cNONE;
    Axis axis;
    int x = 0, y = 0;
    int block_id = 0;
    int block_id2 = 0;
    u8 R, G, B, A;
};

struct Command_On_Strings {
    Command_Type type = cNONE;
    char axis = 'X';
    int x = 0, y = 0;
    std::string block_id  = "";
    std::string block_id2 = "";
    u8 R, G, B, A;
};

struct Block {
    int xmin, ymin;
    int xmax, ymax;
};

#define MAX_COMMANDS 400*400*2
#define MAX_BLOCKS MAX_COMMANDS

std::map<std::string, int> string_to_id;
std::string id_to_string[MAX_BLOCKS];
Block       blocks[MAX_BLOCKS];
bool        block_deleted[MAX_BLOCKS];
SDL_Color   block_color[MAX_BLOCKS];
SDL_Color   block_show_color[MAX_BLOCKS];
int         n_blocks = 0;
int pos_to_id[400][400];

Command commands[MAX_COMMANDS];
int n_commands = 0;
int n_max_redo = 0;

void block_inherit_id(int block_id, int parent_id, char const* suffix) {
    (id_to_string[block_id] = id_to_string[parent_id]) += suffix;
    string_to_id[id_to_string[block_id]] = block_id;
}

void block_set_id(int block_id, std::string const& string_id) {
    id_to_string[block_id] = string_id;
    string_to_id[string_id] = block_id;
}


Block *block_create_inherit(int parent_id, char const* suffix) {
    Block *p = &blocks[parent_id];
    int new_block_id = n_blocks++;
    Block *b = &blocks[new_block_id];
    *b = *p;
    block_inherit_id(new_block_id, parent_id, suffix);
    return b;
}

void block_fill_with_id(int block_id) {
    Block *b = &blocks[block_id];
    for (int x = b->xmin; x < b->xmax; ++x) {
        for (int y = b->ymin; y < b->ymax; ++y) {
            pos_to_id[x][y] = block_id;
        }
    }
}

void block_swap_filled_ids(int id0, int id1) {
    Block *b0 = &blocks[id0];
    Block *b1 = &blocks[id1];

    den_warning(!block_deleted[id0]);
    den_warning(!block_deleted[id1]);
    int size_x = b0->xmax - b0->xmin;
    int size_y = b0->ymax - b0->ymin;
    den_warning(size_x == b1->xmax - b1->xmin && size_y == b1->ymax - b1->ymin);

    for (int x = 0; x < size_x; ++x) {
        for (int y = 0; y < size_y; ++y) {
            pos_to_id[b0->xmin + x][b0->ymin + y] = id1;
            pos_to_id[b1->xmin + x][b1->ymin + y] = id0;
        }
    }
}

void get_random_color(SDL_Color *c) {
    c->r = rand() & 0xFF;
    c->g = rand() & 0xFF;
    c->b = rand() & 0xFF;
    c->a = 0xFF;
}

void create_cut_line(int x, int y, Axis axis) {
    int parent_id = pos_to_id[x][y];
    Block *p = &blocks[parent_id];
    if (axis == VERTICAL) {
        if (x == p->xmin || x == p->xmax) {
            SDL_Log("bad vertical cut skipping");
            return;
        }
    }
    if (axis == HORIZONTAL) {
        if (y == p->ymin || y == p->ymax) {
            SDL_Log("bad horizontal cut skipping");
            return;
        }
    }
    Command *c = &commands[n_commands++];
    c->type = cCUT_LINE;
    c->block_id = parent_id;
    c->axis = axis;
    if (axis == VERTICAL) {
        c->x                                        = x;
        block_create_inherit(parent_id, ".0")->xmax = x;
        block_create_inherit(parent_id, ".1")->xmin = x;
    }
    if (axis == HORIZONTAL) {
        c->y                                        = y;
        block_create_inherit(parent_id, ".0")->ymax = y;
        block_create_inherit(parent_id, ".1")->ymin = y;
    }
    block_color[n_blocks-2] = block_color[parent_id];
    block_color[n_blocks-1] = block_color[parent_id];
    block_fill_with_id(n_blocks-2);
    block_fill_with_id(n_blocks-1);
    get_random_color(&block_show_color[n_blocks-2]);
    get_random_color(&block_show_color[n_blocks-1]);
    block_deleted[parent_id] = true;
    n_max_redo = n_commands;
}

void undo_cut_line() {
    // todo erase from map?
    int id = commands[--n_commands].block_id;
    block_fill_with_id(id);
    block_deleted[id] = false;
    n_blocks -= 2;
}

void create_color(int id, SDL_Color color) {
    Command *c = &commands[n_commands++];
    c->type = cSET_COLOR;
    c->block_id = id;
    c->R = color.r;
    c->G = color.g;
    c->B = color.b;
    c->A = color.a;
    block_color[id] = color;
}

void create_swap_from_ids(int id0, int id1) {
    if (id0 == id1) return;
    Block *b0 = &blocks[id0];
    Block *b1 = &blocks[id1];
    if (b0->xmax - b0->xmin != b1->xmax - b1->xmin
        ||b0->ymax - b0->ymin != b1->ymax - b1->ymin) return;

    Command *c = &commands[n_commands++];
    c->type = cSWAP;
    c->block_id  = SDL_min(id0, id1);
    c->block_id2 = SDL_max(id0, id1);
    block_swap_filled_ids(id0, id1);
    Block tmp = *b0;
    *b0 = *b1;
    *b1 = tmp;
}

void create_swap(int x0, int y0, int x1, int y1) {
    int id0 = pos_to_id[x0][y0];
    int id1 = pos_to_id[x1][y1];
    if (id0 == id1) return;
    create_swap_from_ids(id0, id1);
}


void undo() {
    if (n_commands == 0) return;
    Command *c = &commands[n_commands-1];
    switch (c->type) {
        case cNONE: return;
        case cCUT_LINE: undo_cut_line(); return;
    }
    SDL_Log("no undo for you");
}

void redo_cut_line() {
    Command *c = &commands[n_commands++];
    block_deleted[c->block_id] = true;
    block_deleted[n_blocks++] = false;
    block_deleted[n_blocks++] = false;
    block_fill_with_id(n_blocks-2);
    block_fill_with_id(n_blocks-1);
}

void redo() {
    if (n_commands == n_max_redo) return;
    Command *c = &commands[n_commands];
    switch (c->type) {
        case cNONE: return;
        case cCUT_LINE: redo_cut_line(); return;
    }
}

void highlight_line_in_pos(u8 R, u8 G, u8 B, u8 A, int x, int y) {
    int id = pos_to_id[x][y];
    int px, py;
    pos_to_upper_right_pixel(x, y, &px, &py);
    Block *b = &blocks[id];
    if (current_axis == VERTICAL) {
        highlight_rect(R, G, B, A, x, b->ymin, x, b->ymax);
        draw_text(px + 10*PS, py - 30*PS, "%s %d", id_to_string[id].c_str(), x);
    }
    if (current_axis == HORIZONTAL) {
        highlight_rect(R, G, B, A, b->xmin, y, b->xmax, y);
        draw_text(px + 10*PS, py - 30*PS, "%s %d", id_to_string[id].c_str(), y);
    }
}

void highlight_block_in_pos(u8 R, u8 G, u8 B, u8 A, int x, int y) {
    int id = pos_to_id[x][y];
    Block *b = &blocks[id];
    highlight_rect(R, G, B, A, b->xmin, b->ymin, b->xmax, b->ymax);
}

void clamp(int *v, int m, int M) {
    if (m >= M) return;
    if (*v < m) *v = m;
    if (*v > M) *v = M;
}

void write_command() {
    if (!command_is_ready && mouse_x != MOUSE_DEFAULT) {
        saved_x = mouse_x;
        saved_y = mouse_y;
        command_is_ready = true;
    }
    if (command_is_ready) {
        // сохраняем команду, меняем стейт
        switch (current_command) {
            case cNONE: break;
            case cCUT_LINE: create_cut_line(saved_x, saved_y, current_axis); break;
            case cCUT_POINT: break;
            case cSET_COLOR: create_color(pos_to_id[saved_x][saved_y], saved_color); break;
            case cSWAP: create_swap(saved_x, saved_y, mouse_x, mouse_y); break;
            case cMERGE: break;
        }
        command_is_ready = false;
    }
}

void main_loop() {
    if (!init_sdl()) return;
    defer {close_sdl();};

    if (n_blocks == 0) {
        string_to_id["0"] = 0;
        id_to_string[0] = "0";
        Block *b = &blocks[0];
        b->xmin = 0;
        b->xmax = PIC_W;
        b->ymin = 0;
        b->ymax = PIC_H;
        block_color[0] = {255, 255, 255, 255};
        n_blocks = 1;
    }

    SDL_Rect canvas;
    pos_to_upper_left_pixel(0, PIC_H-1, &canvas.x, &canvas.y);
    int lrx, lry;
    pos_to_lower_right_pixel(PIC_W-1, 0, &lrx, &lry);
    canvas.w = lrx - canvas.x + 1;
    canvas.h = lry - canvas.y + 1;

    for(;;) {
        SDL_SetRenderDrawColor(g_renderer, 0, 0, 0, 255);
        SDL_RenderClear(g_renderer);
        SDL_SetRenderDrawColor(g_renderer, 255, 255, 255, 255);
        {
            Block *b = &blocks[0];
            SDL_Rect rect;
            pos_rect_to_pixel_rect(b->xmin, b->ymin, b->xmax, b->ymax, &rect);
            SDL_RenderFillRect(g_renderer, &rect);
        }

        highlight_rect(255, 0, 0, 255, mouse_x - 1, mouse_y - 1, mouse_x + 1, mouse_y + 1);

        for (int id = 0; id < n_blocks; ++id) if (!block_deleted[id]) {
            Block *b = &blocks[id];
            SDL_Color s = block_show_color[id];
            SDL_Color c = block_color[id];
            SDL_Rect rect;
            pos_rect_to_pixel_rect(b->xmin, b->ymin, b->xmax, b->ymax, &rect);

            SDL_SetRenderDrawColor(g_renderer, c.r, c.g, c.b, c.a);
            SDL_RenderFillRect(g_renderer, &rect);

            SDL_SetRenderDrawColor(g_renderer, s.r, s.g, s.b, s.a);
            SDL_RenderDrawLine(g_renderer, rect.x, rect.y, rect.x + rect.w, rect.y + rect.h);
            SDL_RenderDrawLine(g_renderer, rect.x + rect.w, rect.y, rect.x, rect.y + rect.h);
            SDL_RenderDrawRect(g_renderer, &rect);
        }

        // Рисуем что за операция сохранится
        switch (current_command) {
            case cCUT_LINE: {
                if (MOUSE_DEFAULT != mouse_x) {
                    highlight_line_in_pos(255, 0, 0, 255, mouse_x, mouse_y);
                }
                if (command_is_ready) {
                    highlight_line_in_pos(255, 0, 0, 255, saved_x, saved_y);
                }
            } break;
            case cSWAP: {
                if (command_is_ready) {
                    highlight_block_in_pos(255, 0, 0, 255, saved_x, saved_y);
                }
            } break;
            case cSET_COLOR: {
                SDL_Color c = saved_color;
                if (command_is_ready) {
                    highlight_block_in_pos(c.r, c.g, c.b, c.a, saved_x, saved_y);
                } else {
                    highlight_block_in_pos(c.r, c.g, c.b, c.a, mouse_x, mouse_y);
                }
            } break;
        }

        for (int id = 0; id < n_blocks; ++id) if (!block_deleted[id]) {
            Block *b = &blocks[id];
            SDL_Color c = block_show_color[id];
            SDL_Rect rect;
            pos_rect_to_pixel_rect(b->xmin, b->ymin, b->xmax, b->ymax, &rect);

            if (b->xmin <= mouse_x && mouse_x < b->xmax && b->ymin <= mouse_y && mouse_y < b->ymax) {
                draw_text(rect.x + 2, rect.y + 2, "%s", id_to_string[id].c_str());
                draw_text(rect.x + 2, rect.y + 2 + 30, "%d %d %d %d", c.r, c.g, c.b, c.a);
            }
        }

        SDL_RenderPresent(g_renderer);

        SDL_Event event;
        while (SDL_PollEvent(&event)) {
            if (event.type == SDL_QUIT) {
                return;
            } else if (event.type & SDL_MOUSEMOTION) {
                pixel_to_pos(event.motion.x, event.motion.y, &mouse_x, &mouse_y);

                if (mouse_x >= 400 || mouse_x < 0 || mouse_y >= 400 || mouse_y < 0) { mouse_x = PIC_W + 2; mouse_y = PIC_H + 2; }
                else {
                    int m = SNAP_GRID_SIZE;
                    mouse_x = mouse_x - (mouse_x % m);
                    mouse_y = mouse_y - (mouse_y % m);
                    if (event.type == SDL_MOUSEBUTTONDOWN) {
                        // сохраняем операцию
                        saved_x = mouse_x;
                        saved_y = mouse_y;
                        command_is_ready = true;
                    }
                }

                if (event.type == SDL_MOUSEBUTTONDOWN) {
                    ;
                }

                if (event.type == SDL_MOUSEBUTTONUP) {
                }

            } else if (event.type & SDL_KEYDOWN) {
                if (event.type == SDL_KEYUP) {
                    #define Other(Val, Option0, Option1) ((Val) == (Option0) ? (Option1) : (Option0))
                    #define Immediate_Color(Key, R, G, B, A) \
                        if (event.key.keysym.sym == SDLK_##Key) { saved_color = {R, G, B, A}; \
                            auto tmp = current_command; current_command = cSET_COLOR; write_command(); current_command = tmp; }
                    Immediate_Color(q,   0,   0,   0, 255);
                    Immediate_Color(w, 255, 255, 255, 255);
                    Immediate_Color(e,   0,   0, 255, 255);
                    //if (event.key.keysym.sym == SDLK_q) { saved_color = {0, 0, 0, 255};
                    //    auto tmp = current_command; current_command = cSET_COLOR; write_command(); current_command = tmp; }
                    //if (event.key.keysym.sym == SDLK_w) { saved_color = {255, 255, 255, 255}; current_command = cSET_COLOR;
                    //    auto tmp = current_command; current_command = cSET_COLOR; write_command(); current_command = tmp; }
                    //if (event.key.keysym.sym == SDLK_e) { saved_color = {0, 0, 255, 255}; current_command = cSET_COLOR;
                    //    auto tmp = current_command; current_command = cSET_COLOR; write_command(); current_command = tmp; }
                    if (event.key.keysym.sym == SDLK_TAB) { command_is_ready = false; current_command = cSWAP; }
                    if (event.key.keysym.sym == SDLK_1) { command_is_ready = false; current_command = cCUT_LINE; current_axis = HORIZONTAL; }
                    if (event.key.keysym.sym == SDLK_2) { command_is_ready = false; current_command = cCUT_LINE; current_axis = VERTICAL; }
                    if (event.key.keysym.sym == SDLK_3) { command_is_ready = false; current_command = cSET_COLOR; }
                    if (event.key.keysym.sym == SDLK_v) { command_is_ready = false; current_axis = Other(current_axis, HORIZONTAL, VERTICAL); }
                    if (event.key.keysym.sym == SDLK_z) { command_is_ready = false; undo(); }
                    if (event.key.keysym.sym == SDLK_y) { command_is_ready = false; redo(); }
                    if (event.key.keysym.sym == SDLK_LEFT)  { saved_x -= 1; clamp(&saved_x, 0, PIC_W); }
                    if (event.key.keysym.sym == SDLK_RIGHT) { saved_x += 1; clamp(&saved_x, 0, PIC_W); }
                    if (event.key.keysym.sym == SDLK_DOWN)  { saved_y -= 1; clamp(&saved_y, 0, PIC_H); }
                    if (event.key.keysym.sym == SDLK_UP)    { saved_y += 1; clamp(&saved_y, 0, PIC_H); }
                    if (event.key.keysym.sym == SDLK_SPACE) { write_command(); }
                    if (event.key.keysym.sym == SDLK_s) {
                        FILE *f = fopen("commands.isl", "w");
                        defer { fclose(f); };
                        for (int i = 0; i < n_commands; ++i) {
                            Command *c = &commands[i];
                            switch (c->type) {
                                case cNONE: {
                                    SDL_Log("");
                                } break;
                                case cCUT_LINE: {
                                    char const* axis = "her_vam";
                                    int v = -1;
                                    if (c->axis == VERTICAL) {
                                        axis = "X";
                                        v = c->x;
                                    }
                                    if (c->axis == HORIZONTAL) {
                                        axis = "Y";
                                        v = c->y;
                                    }
                                    fprintf(f, "cut [%s] [%s] [%d]\n", id_to_string[c->block_id].c_str(), axis, v);
                                } break;
                                case cSWAP: {
                                    fprintf(f, "swap [%s] [%s]\n", id_to_string[c->block_id].c_str(), id_to_string[c->block_id2].c_str());
                                } break;
                                case cSET_COLOR: {
                                    fprintf(f, "color [%s] [%d,%d,%d,%d]\n", id_to_string[c->block_id].c_str(), c->R, c->G, c->B, c->A);
                                } break;
                                default: {
                                    fprintf(f, "git merge origin/master\n");
                                } break;
                            }
                        }
                    }
                }
            }
        }
    }
}

std::string read_entire_file(std::string const& filename) {
    FILE  *f = fopen(filename.c_str(), "rb");

    long size;
    {
        long pos = ftell(f);
        fseek(f, 0, SEEK_END);
        size = ftell(f);
        fseek(f, pos, SEEK_SET);
    }

    std::string ret;
    ret.resize(size);
    size_t n_read = fread((void*)ret.data(), 1, size, f);

    fclose(f);
    return ret;
}

// JSON PART
#if INLUDE_JSON
/*
example:
{"width": 400, "height": 400, "blocks": [{"blockId": "0", "bottomLeft": [0, 0], "topRight": [40, 40], "color": [255, 255, 255, 255]}, {"blockId": "1", "bottomLeft": [0, 40], "topRight": [40, 80], "color": [255, 255, 255, 255]}, {"blockId": "2", "bottomLeft": [0, 80], "topRight": [40, 120], "color": [255, 255, 255, 255]}, {"blockId": "3", "bottomLeft": [0, 120], "topRight": [40, 160], "color": [255, 255, 255, 255]}, {"blockId": "4", "bottomLeft": [0, 160], "topRight": [40, 200], "color": [255, 255, 255, 255]}, {"blockId": "5", "bottomLeft": [0, 200], "topRight": [40, 240], "color": [255, 255, 255, 255]}, {"blockId": "6", "bottomLeft": [0, 240], "topRight": [40, 280], "color": [0, 0, 0, 255]}, {"blockId": "7", "bottomLeft": [0, 280], "topRight": [40, 320], "color": [0, 0, 0, 255]}, {"blockId": "8", "bottomLeft": [0, 320], "topRight": [40, 360], "color": [0, 0, 0, 255]}, {"blockId": "9", "bottomLeft": [0, 360], "topRight": [40, 400], "color": [255, 255, 255, 255]}, {"blockId": "10", "bottomLeft": [40, 0], "topRight": [80, 40], "color": [255, 255, 255, 255]}, {"blockId": "11", "bottomLeft": [40, 40], "topRight": [80, 80], "color": [255, 255, 255, 255]}, {"blockId": "12", "bottomLeft": [40, 80], "topRight": [80, 120], "color": [0, 0, 0, 255]}, {"blockId": "13", "bottomLeft": [40, 120], "topRight": [80, 160], "color": [255, 255, 255, 255]}, {"blockId": "14", "bottomLeft": [40, 160], "topRight": [80, 200], "color": [255, 255, 255, 255]}, {"blockId": "15", "bottomLeft": [40, 200], "topRight": [80, 240], "color": [0, 0, 0, 255]}, {"blockId": "16", "bottomLeft": [40, 240], "topRight": [80, 280], "color": [255, 255, 255, 255]}, {"blockId": "17", "bottomLeft": [40, 280], "topRight": [80, 320], "color": [255, 255, 255, 255]}, {"blockId": "18", "bottomLeft": [40, 320], "topRight": [80, 360], "color": [255, 255, 255, 255]}, {"blockId": "19", "bottomLeft": [40, 360], "topRight": [80, 400], "color": [0, 0, 0, 255]}, {"blockId": "20", "bottomLeft": [80, 0], "topRight": [120, 40], "color": [255, 255, 255, 255]}, {"blockId": "21", "bottomLeft": [80, 40], "topRight": [120, 80], "color": [0, 0, 0, 255]}, {"blockId": "22", "bottomLeft": [80, 80], "topRight": [120, 120], "color": [0, 0, 0, 255]}, {"blockId": "23", "bottomLeft": [80, 120], "topRight": [120, 160], "color": [0, 0, 0, 255]}, {"blockId": "24", "bottomLeft": [80, 160], "topRight": [120, 200], "color": [0, 0, 0, 255]}, {"blockId": "25", "bottomLeft": [80, 200], "topRight": [120, 240], "color": [255, 255, 255, 255]}, {"blockId": "26", "bottomLeft": [80, 240], "topRight": [120, 280], "color": [255, 255, 255, 255]}, {"blockId": "27", "bottomLeft": [80, 280], "topRight": [120, 320], "color": [255, 255, 255, 255]}, {"blockId": "28", "bottomLeft": [80, 320], "topRight": [120, 360], "color": [255, 255, 255, 255]}, {"blockId": "29", "bottomLeft": [80, 360], "topRight": [120, 400], "color": [255, 255, 255, 255]}, {"blockId": "30", "bottomLeft": [120, 0], "topRight": [160, 40], "color": [0, 0, 0, 255]}, {"blockId": "31", "bottomLeft": [120, 40], "topRight": [160, 80], "color": [0, 0, 0, 255]}, {"blockId": "32", "bottomLeft": [120, 80], "topRight": [160, 120], "color": [0, 0, 0, 255]}, {"blockId": "33", "bottomLeft": [120, 120], "topRight": [160, 160], "color": [255, 255, 255, 255]}, {"blockId": "34", "bottomLeft": [120, 160], "topRight": [160, 200], "color": [0, 0, 0, 255]}, {"blockId": "35", "bottomLeft": [120, 200], "topRight": [160, 240], "color": [255, 255, 255, 255]}, {"blockId": "36", "bottomLeft": [120, 240], "topRight": [160, 280], "color": [255, 255, 255, 255]}, {"blockId": "37", "bottomLeft": [120, 280], "topRight": [160, 320], "color": [255, 255, 255, 255]}, {"blockId": "38", "bottomLeft": [120, 320], "topRight": [160, 360], "color": [255, 255, 255, 255]}, {"blockId": "39", "bottomLeft": [120, 360], "topRight": [160, 400], "color": [0, 0, 0, 255]}, {"blockId": "40", "bottomLeft": [160, 0], "topRight": [200, 40], "color": [255, 255, 255, 255]}, {"blockId": "41", "bottomLeft": [160, 40], "topRight": [200, 80], "color": [0, 0, 0, 255]}, {"blockId": "42", "bottomLeft": [160, 80], "topRight": [200, 120], "color": [0, 0, 0, 255]}, {"blockId": "43", "bottomLeft": [160, 120], "topRight": [200, 160], "color": [255, 255, 255, 255]}, {"blockId": "44", "bottomLeft": [160, 160], "topRight": [200, 200], "color": [255, 255, 255, 255]}, {"blockId": "45", "bottomLeft": [160, 200], "topRight": [200, 240], "color": [0, 0, 0, 255]}, {"blockId": "46", "bottomLeft": [160, 240], "topRight": [200, 280], "color": [0, 0, 0, 255]}, {"blockId": "47", "bottomLeft": [160, 280], "topRight": [200, 320], "color": [0, 0, 0, 255]}, {"blockId": "48", "bottomLeft": [160, 320], "topRight": [200, 360], "color": [255, 255, 255, 255]}, {"blockId": "49", "bottomLeft": [160, 360], "topRight": [200, 400], "color": [255, 255, 255, 255]}, {"blockId": "50", "bottomLeft": [200, 0], "topRight": [240, 40], "color": [0, 0, 0, 255]}, {"blockId": "51", "bottomLeft": [200, 40], "topRight": [240, 80], "color": [0, 0, 0, 255]}, {"blockId": "52", "bottomLeft": [200, 80], "topRight": [240, 120], "color": [255, 255, 255, 255]}, {"blockId": "53", "bottomLeft": [200, 120], "topRight": [240, 160], "color": [255, 255, 255, 255]}, {"blockId": "54", "bottomLeft": [200, 160], "topRight": [240, 200], "color": [255, 255, 255, 255]}, {"blockId": "55", "bottomLeft": [200, 200], "topRight": [240, 240], "color": [0, 0, 0, 255]}, {"blockId": "56", "bottomLeft": [200, 240], "topRight": [240, 280], "color": [255, 255, 255, 255]}, {"blockId": "57", "bottomLeft": [200, 280], "topRight": [240, 320], "color": [0, 0, 0, 255]}, {"blockId": "58", "bottomLeft": [200, 320], "topRight": [240, 360], "color": [255, 255, 255, 255]}, {"blockId": "59", "bottomLeft": [200, 360], "topRight": [240, 400], "color": [0, 0, 0, 255]}, {"blockId": "60", "bottomLeft": [240, 0], "topRight": [280, 40], "color": [0, 0, 0, 255]}, {"blockId": "61", "bottomLeft": [240, 40], "topRight": [280, 80], "color": [0, 0, 0, 255]}, {"blockId": "62", "bottomLeft": [240, 80], "topRight": [280, 120], "color": [0, 0, 0, 255]}, {"blockId": "63", "bottomLeft": [240, 120], "topRight": [280, 160], "color": [0, 0, 0, 255]}, {"blockId": "64", "bottomLeft": [240, 160], "topRight": [280, 200], "color": [255, 255, 255, 255]}, {"blockId": "65", "bottomLeft": [240, 200], "topRight": [280, 240], "color": [255, 255, 255, 255]}, {"blockId": "66", "bottomLeft": [240, 240], "topRight": [280, 280], "color": [0, 0, 0, 255]}, {"blockId": "67", "bottomLeft": [240, 280], "topRight": [280, 320], "color": [0, 0, 0, 255]}, {"blockId": "68", "bottomLeft": [240, 320], "topRight": [280, 360], "color": [0, 0, 0, 255]}, {"blockId": "69", "bottomLeft": [240, 360], "topRight": [280, 400], "color": [0, 0, 0, 255]}, {"blockId": "70", "bottomLeft": [280, 0], "topRight": [320, 40], "color": [255, 255, 255, 255]}, {"blockId": "71", "bottomLeft": [280, 40], "topRight": [320, 80], "color": [0, 0, 0, 255]}, {"blockId": "72", "bottomLeft": [280, 80], "topRight": [320, 120], "color": [255, 255, 255, 255]}, {"blockId": "73", "bottomLeft": [280, 120], "topRight": [320, 160], "color": [0, 0, 0, 255]}, {"blockId": "74", "bottomLeft": [280, 160], "topRight": [320, 200], "color": [255, 255, 255, 255]}, {"blockId": "75", "bottomLeft": [280, 200], "topRight": [320, 240], "color": [255, 255, 255, 255]}, {"blockId": "76", "bottomLeft": [280, 240], "topRight": [320, 280], "color": [0, 0, 0, 255]}, {"blockId": "77", "bottomLeft": [280, 280], "topRight": [320, 320], "color": [255, 255, 255, 255]}, {"blockId": "78", "bottomLeft": [280, 320], "topRight": [320, 360], "color": [0, 0, 0, 255]}, {"blockId": "79", "bottomLeft": [280, 360], "topRight": [320, 400], "color": [0, 0, 0, 255]}, {"blockId": "80", "bottomLeft": [320, 0], "topRight": [360, 40], "color": [0, 0, 0, 255]}, {"blockId": "81", "bottomLeft": [320, 40], "topRight": [360, 80], "color": [0, 0, 0, 255]}, {"blockId": "82", "bottomLeft": [320, 80], "topRight": [360, 120], "color": [255, 255, 255, 255]}, {"blockId": "83", "bottomLeft": [320, 120], "topRight": [360, 160], "color": [255, 255, 255, 255]}, {"blockId": "84", "bottomLeft": [320, 160], "topRight": [360, 200], "color": [255, 255, 255, 255]}, {"blockId": "85", "bottomLeft": [320, 200], "topRight": [360, 240], "color": [255, 255, 255, 255]}, {"blockId": "86", "bottomLeft": [320, 240], "topRight": [360, 280], "color": [0, 0, 0, 255]}, {"blockId": "87", "bottomLeft": [320, 280], "topRight": [360, 320], "color": [255, 255, 255, 255]}, {"blockId": "88", "bottomLeft": [320, 320], "topRight": [360, 360], "color": [255, 255, 255, 255]}, {"blockId": "89", "bottomLeft": [320, 360], "topRight": [360, 400], "color": [255, 255, 255, 255]}, {"blockId": "90", "bottomLeft": [360, 0], "topRight": [400, 40], "color": [255, 255, 255, 255]}, {"blockId": "91", "bottomLeft": [360, 40], "topRight": [400, 80], "color": [255, 255, 255, 255]}, {"blockId": "92", "bottomLeft": [360, 80], "topRight": [400, 120], "color": [0, 0, 0, 255]}, {"blockId": "93", "bottomLeft": [360, 120], "topRight": [400, 160], "color": [255, 255, 255, 255]}, {"blockId": "94", "bottomLeft": [360, 160], "topRight": [400, 200], "color": [255, 255, 255, 255]}, {"blockId": "95", "bottomLeft": [360, 200], "topRight": [400, 240], "color": [0, 0, 0, 255]}, {"blockId": "96", "bottomLeft": [360, 240], "topRight": [400, 280], "color": [0, 0, 0, 255]}, {"blockId": "97", "bottomLeft": [360, 280], "topRight": [400, 320], "color": [0, 0, 0, 255]}, {"blockId": "98", "bottomLeft": [360, 320], "topRight": [400, 360], "color": [0, 0, 0, 255]}, {"blockId": "99", "bottomLeft": [360, 360], "topRight": [400, 400], "color": [255, 255, 255, 255]}]}
one block:
{"blockId": "0", "bottomLeft": [0, 0], "topRight": [40, 40], "color": [255, 255, 255, 255]}
*/

using namespace nlohmann;
void read_blocks(char const* filename) {
    std::string text = read_entire_file(filename);
    json j = json::parse(text);

    int w = j["width"].get<int>();
    int h = j["height"].get<int>();

    FILE *f = fopen("log.log", "w");
    den_warning(f);
    fprintf(f, "w = %d, h = %d\n", w, h);

    den_warning(n_blocks == 0);
    for (auto &bj : j["blocks"]) {
        int id = n_blocks++;
        Block *b = &blocks[id];
        block_set_id(id, bj["blockId"].get<std::string>());
        b->xmin = bj["bottomLeft"][0].get<int>();
        b->ymin = bj["bottomLeft"][1].get<int>();
        b->xmax = bj["topRight"][0].get<int>();
        b->ymax = bj["topRight"][1].get<int>();
        u8 R, G, B, A;
        R = block_color[id].r = (u8)bj["color"][0].get<int>();
        G = block_color[id].g = (u8)bj["color"][1].get<int>();
        B = block_color[id].b = (u8)bj["color"][2].get<int>();
        A = block_color[id].a = (u8)bj["color"][3].get<int>();
        block_fill_with_id(id);

        //fprintf(f, "\n{\"blockId\": \"%s\", \"bottomLeft\": [%d, %d], \"topRight\": [%d, %d], \"color\": [%d, %d, %d, %d]}, ",
        //            id_to_string[id].c_str(), b->xmin, b->ymin, b->xmax, b->ymax, R, G, B, A);
        //fflush(f);
    }

    char o[100000];
    int i = 0;
    for (int k = 0; k < text.size(); ++k) {
        char c = text[k];
        if (c == '{') o[i++] = '\n';
        o[i++] = c;
    }
    o[i] = 0;

    fputs(o, f);
    fclose(f);
}
#endif

// PARSING PART


std::vector<std::string> lines_of_the_string(std::string s) {
    std::vector<std::string> ret;
    int line_start = 0;
    for (int i = 0; i <= s.size(); ++i) {
        if (i == s.size() || s[i] == '\n' || s[i] == '\r') {
            ret.push_back("");
            for (int j = line_start; j < i; ++j) {
                ret.back().push_back(s[j]);
            }
            ret.back().push_back('\n');
            line_start = i + 1;
        }
    }
    return ret;
}

struct Iter {
    char const* c;
    int         size;
};

void advance(Iter *s) {
    if (s->size == 0) return;
    s->c++;
    s->size--;
}

static inline char get(Iter *s) {
    return *s->c;
}

static inline char pop_internal(Iter *s, char const* expected, int line) {
    den_warning(s->size);
    char c = get(s);
    if (expected) {
        log_warning(c == *expected, "c=%c e=%s line=%d", c, expected, line);
    }
    advance(s);
    return c;
}

#define pop(S, E) pop_internal(S, E, __LINE__)

void skip(Iter *s, char const* cs) {
    if (!cs[0]) return;
    if (!cs[1]) {
        char c = cs[0];
        while (s->size && get(s) == c) advance(s);
        return;
    }
    for (;;) {
        char const* c = cs;
        while (*c) {
            if (!s->size || get(s) != *c++) return;
            advance(s);
        }
    }
}

std::string read_word(Iter *s) {
    skip(s, " ");
    std::string ret;
    while ('a' <= get(s) && get(s) <= 'z') {
        ret.push_back(get(s));
        advance(s);
    }
    return ret;
}

int read_int(Iter *s) {
    skip(s, " ");
    int ret = 0;
    while ('0' <= get(s) && get(s) <= '9') {
        int digit = get(s) - '0';
        ret *= 10;
        ret += digit;
        advance(s);
    }
    return ret;
}

std::string read_id(Iter *s) {
    skip(s, " ");
    pop(s, "[");
      std::string ret;
      while (get(s) != ']') {
          if (!(get(s) == '.' || ('0' <= get(s) && get(s) <= '9'))) {
            int k = 1324;
          }
          den_warning(get(s) == '.' || ('0' <= get(s) && get(s) <= '9'));
          ret.push_back(get(s));
          advance(s);
      }
    pop(s, "]");
    return ret;
}

void command_from_command_on_strings(Command *o, Command_On_Strings *c) {
    #define C(Field) o->Field = c->Field
    C(type);
    den_warning(c->axis == 'X' || c->axis == 'Y');
    o->axis = c->axis == 'X' ? VERTICAL : HORIZONTAL;
    C(x); C(y);
    log_warning(string_to_id.find(c->block_id)  != string_to_id.end(), "id was '%s'", c->block_id.c_str());
    o->block_id = string_to_id[c->block_id];
    if (c->block_id2 != "") {
        log_warning(string_to_id.find(c->block_id2) != string_to_id.end(), "id2 was '%s'", c->block_id2.c_str());
        o->block_id2 = string_to_id[c->block_id2];
    }
    C(R); C(G); C(B); C(A);
    #undef C
}

Command_On_Strings line_to_command(std::string const& line) {
    Command_On_Strings ret;
    if (line == "") return ret;

    Iter iterator;
    Iter *s = &iterator;
    s->c = line.data();
    s->size = (int)line.size();
    std::string name = read_word(s);

    // block is common for all commands
    ret.block_id = read_id(s);

    if (name == "cut") {
        skip(s, " ");
        pop(s, "[");

        char axis = get(s);
        if (axis == 'X' || axis == 'Y') {
            ret.type = cCUT_LINE;
            ret.axis = pop(s, 0);
            pop(s, "]");
            skip(s, " ");
            pop(s, "[");
                if (axis == 'X') {
                    ret.x = read_int(s);
                } else {
                    ret.y = read_int(s);
                }
            pop(s, "]");
        } else {
            ret.type = cCUT_POINT;
            ret.x = read_int(s);
            pop(s, ","); ret.y = read_int(s);
            pop(s, "]");
        }
        return ret;
    }

    if (name == "color") {
        ret.type = cSET_COLOR;
        skip(s, " ");
        pop(s, "[");
            ret.R = (u8)read_int(s);
            pop(s, ","); ret.G = (u8)read_int(s);
            pop(s, ","); ret.B = (u8)read_int(s);
            pop(s, ","); ret.A = (u8)read_int(s);
        pop(s, "]");
        return ret;
    }

    if (name == "merge") {
        ret.type = cMERGE;
        ret.block_id2 = read_id(s);
        return ret;
    }

    if (name == "swap") {
        ret.type = cSWAP;
        ret.block_id2 = read_id(s);
        return ret;
    }

    return ret;
}

std::string command_on_strings_to_string(Command_On_Strings *c) {
    std::stringstream ss;
    if (c->type == cCUT_LINE) {
        int offset = c->axis == 'X' ? c->x : c->y;
        ss << "cut[" << c->block_id << "][" << c->axis << "][" << offset << "]";
    }
    if (c->type == cCUT_POINT) {
        ss << "cut[" << c->block_id << "][" << c->x << "," << c->y << "]";
    }
    if (c->type == cSET_COLOR) {
        ss << "color[" << c->block_id << "][" << (int)c->R << "," << (int)c->G << "," << (int)c->B << "," << (int)c->A << "]";
    }
    if (c->type == cMERGE) {
        ss << "merge[" << c->block_id << "][" << c->block_id2 << "]";
    }
    std::string ret;
    ss >> ret;
    return ret;
}

void read_commands(char const* filename) {
    std::string text = read_entire_file(filename);
    std::vector<std::string> lines = lines_of_the_string(text);
    std::vector<Command_On_Strings> commands_on_string;
    commands_on_string.reserve(lines.size());
    for (std::string const& line : lines) {
        if (line == "" || line == "\n") continue;
        commands_on_string.emplace_back(line_to_command(line));
    }

    for (int i = 0; i < commands_on_string.size(); ++i) {
        Command_On_Strings *_c = &commands_on_string[i];
        log_warning(_c->type == cSWAP, "type %d is unsupported", (int)_c->type);
        Command *c = &commands[n_commands++];
        command_from_command_on_strings(c, _c);
        create_swap_from_ids(c->block_id, c->block_id2);
    }
}
