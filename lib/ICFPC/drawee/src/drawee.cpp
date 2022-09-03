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

#include <assert.h>
#ifdef _MSC_VER
#include "SDL.h"
#include "SDL_ttf.h"
#else
#include <SDL2\SDL.h>
#include <SDL2\SDL_ttf.h>
#endif
#include <atomic>
#include <chrono>
#include <string>
#include <cmath>
#include <map>
#include <string>

using u8 = uint8_t;
using u16 = uint16_t;
using u32 = uint32_t;
using u64 = uint64_t;

using u16 = uint16_t;
using s16 = int16_t;

SDL_Window   *g_window;
#define WIN_W 500
#define WIN_H 500
#define PIC_W 400
#define PIC_H 400
#define PS 2
#define X 0
#define Y 1
int g_canvas_zero_lower_left[2] = {(0)*PS, (WIN_H)*PS};
SDL_Renderer *g_renderer;
TTF_Font     *g_font;

void main_loop();

int main(int const n_args, char ** const args) {
    if(SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO) == -1) {SDL_Log("Could not initialize SDL: %s.\n", SDL_GetError()); return -1;}
    defer {SDL_Quit();};

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

void draw_vertical_line(u8 R, u8 G, u8 B, u8 A, int posx, int y0 = 0, int y1 = 0) {
    SDL_SetRenderDrawColor(g_renderer, R, G, B, A);
    int screen_pos = posx * PS + g_canvas_zero_lower_left[X];
    SDL_RenderDrawLine(g_renderer, screen_pos - y0, 0, screen_pos + y1, WIN_H * PS);
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
int current_block_id;
int saved_x = 0;
int saved_y = 0;
bool click = false;
#define MOUSE_DEFAULT (PIC_W + 2)
int mouse_x = MOUSE_DEFAULT;
int mouse_y = MOUSE_DEFAULT;


struct Command {
    Command_Type type = cNONE;
    Axis axis;
    int x = 0, y = 0;
    int block_id = 0;
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
int         n_blocks = 0;
int pos_to_id[400][400];

Command commands[MAX_COMMANDS];
int n_commands = 0;
int n_max_redo = 0;

void block_set_id(int block_id, int parent_id, char const* suffix) {
    (id_to_string[block_id] = id_to_string[parent_id]) += suffix;
    string_to_id[id_to_string[block_id]] = block_id;
}

Block *block_create_inherit(int parent_id, char const* suffix) {
    Block *p = &blocks[parent_id];
    int new_block_id = n_blocks++;
    Block *b = &blocks[new_block_id];
    *b = *p;
    block_set_id(new_block_id, parent_id, suffix);
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

void get_random_color(SDL_Color *c) {
    c->r = rand() & 0xFF;
    c->g = rand() & 0xFF;
    c->b = rand() & 0xFF;
    c->a = 0xFF;
}

void create_cut_line(int x, int y, Axis axis) {
    int parent_id = pos_to_id[x][y];
    Command *c = &commands[n_commands++];
    c->type = cCUT_LINE;
    c->block_id = parent_id;
    c->axis = axis;
    Block *p = &blocks[parent_id];
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
    block_fill_with_id(n_blocks-2);
    block_fill_with_id(n_blocks-1);
    get_random_color(&block_color[n_blocks-2]);
    get_random_color(&block_color[n_blocks-1]);
    block_deleted[parent_id] = true;
}

void undo_cut_line() {
    // todo erase from map?
    int id = commands[--n_commands].block_id;
    block_fill_with_id(id);
    block_deleted[id] = false;
    n_blocks -= 2;
}

void undo() {
    if (n_commands == 0) return;
    Command *c = &commands[n_commands-1];
    switch (c->type) {
        case cNONE: return;
        case cCUT_LINE: undo_cut_line(); return;
    }
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

void clamp(int *v, int m, int M) {
    if (m >= M) return;
    if (*v < m) *v = m;
    if (*v > M) *v = M;
}

void main_loop() {
    if (!init_sdl()) return;
    defer {close_sdl();};

    {
        string_to_id["0"] = 0;
        id_to_string[0] = "0";
        Block *b = &blocks[0];
        b->xmin = 0;
        b->xmax = PIC_W;
        b->ymin = 0;
        b->ymax = PIC_H;
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

        highlight_rect(255, 0, 0, 255, mouse_x - 1, mouse_y - 1, mouse_x + 1, mouse_y + 1);

        for (int id = 0; id < n_blocks; ++id) if (!block_deleted[id]) {
            Block *b = &blocks[id];
            SDL_Color c = block_color[id];
            SDL_SetRenderDrawColor(g_renderer, c.r, c.g, c.b, c.a);
            SDL_Rect rect;
            pos_rect_to_pixel_rect(b->xmin, b->ymin, b->xmax, b->ymax, &rect);
            SDL_RenderFillRect(g_renderer, &rect);
            draw_text(rect.x + 2, rect.y + 2, "%s", id_to_string[id].c_str());
            draw_text(rect.x + 2, rect.y + 2 + 30, "%d %d %d %d", c.r, c.g, c.b, c.a);
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
        }

        SDL_RenderPresent(g_renderer);

        SDL_Event event;
        while (SDL_PollEvent(&event)) {
            if (event.type == SDL_QUIT) {
                return;
            } else if (event.type & SDL_MOUSEMOTION) {
                pixel_to_pos(event.motion.x, event.motion.y, &mouse_x, &mouse_y);
                if (mouse_x >= 400 || mouse_x < 0 || mouse_y >= 400 || mouse_y < 0) { mouse_x = PIC_W + 2; mouse_y = PIC_H + 2; }
                else if (event.type == SDL_MOUSEBUTTONDOWN) {
                    // сохраняем операцию
                    saved_x = mouse_x;
                    saved_y = mouse_y;
                    command_is_ready = true;
                }
                if (event.type == SDL_MOUSEBUTTONDOWN) {
                    ;
                }

                if (event.type == SDL_MOUSEBUTTONUP) {
                }

            } else if (event.type & SDL_KEYDOWN) {
                if (event.type == SDL_KEYUP) {
                    #define Other(Val, Option0, Option1) ((Val) == (Option0) ? (Option1) : (Option0))
                    if (event.key.keysym.sym == SDLK_1) { command_is_ready = false; current_command = cCUT_LINE; current_axis = HORIZONTAL; }
                    if (event.key.keysym.sym == SDLK_2) { command_is_ready = false; current_command = cCUT_LINE; current_axis = VERTICAL; }
                    if (event.key.keysym.sym == SDLK_v) { command_is_ready = false; current_axis = Other(current_axis, HORIZONTAL, VERTICAL); }
                    if (event.key.keysym.sym == SDLK_z) { command_is_ready = false; undo(); }
                    if (event.key.keysym.sym == SDLK_y) { command_is_ready = false; redo(); }
                    if (event.key.keysym.sym == SDLK_LEFT)  { saved_x -= 1; clamp(&saved_x, 0, PIC_W); }
                    if (event.key.keysym.sym == SDLK_RIGHT) { saved_x += 1; clamp(&saved_x, 0, PIC_W); }
                    if (event.key.keysym.sym == SDLK_DOWN)  { saved_y -= 1; clamp(&saved_y, 0, PIC_H); }
                    if (event.key.keysym.sym == SDLK_UP)    { saved_y += 1; clamp(&saved_y, 0, PIC_H); }
                    if (event.key.keysym.sym == SDLK_SPACE) {
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
                                case cCUT_POINT:
                                case cSET_COLOR:
                                case cSWAP:
                                case cMERGE:
                                break;
                            }
                            n_max_redo = n_commands;
                            command_is_ready = false;
                        }
                    }
                    if (event.key.keysym.sym == SDLK_RETURN) {
                        for (int i = 0; i < n_commands; ++i) {
                            Command *c = &commands[i];
                            switch (c->type) {
                                case cNONE: {
                                    SDL_Log("");
                                } break;
                                case cCUT_LINE: {
                                    Block *b = &blocks[c->block_id];
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
                                    SDL_Log("cut [%s] [%s] [%d]", id_to_string[c->block_id].c_str(), axis, v);
                                } break;
                                default: {
                                    SDL_Log("swap underwear");
                                } break;
                            }
                        }
                    }
                }
            }
        }
    }
}
