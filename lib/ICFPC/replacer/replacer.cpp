#include <string>
#include <stdio.h>
#include <vector>
#include <sstream>
#include <iostream>

#ifdef _MSC_VER
#define FUNCTION_NAME __FUNCTION__
#else
#define FUNCTION_NAME ""
#endif

typedef uint8_t u8;
#define warning(Expr) if (!(Expr)) fprintf(stderr, "warning! %s " #Expr "\n", FUNCTION_NAME);
#define log_warning(Expr, Format, ...) if (!(Expr)) fprintf(stderr, "warning! %s " #Expr " " Format "\n", FUNCTION_NAME, __VA_ARGS__);

enum Command_Type {
    cNONE = 0,
    cCUT_LINE,
    cCUT_POINT,
    cSET_COLOR,
    cSWAP,
    cMERGE
};

struct Command {
    Command_Type type = cNONE;
    char axis = 'X';
    int x = 0, y = 0;
    std::string block_id  = "";
    std::string block_id2 = "";
    u8 R, G, B, A;
};

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
    warning(s->size);
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
          warning(get(s) == '.' || ('0' <= get(s) && get(s) <= '9'));
          ret.push_back(get(s));
          advance(s);
      }
    pop(s, "]");
    return ret;
}

Command line_to_command(std::string const& line) {
    Command ret;
    if (line == "") return ret;

    Iter iterator;
    Iter *s = &iterator;
    s->c = line.data();
    s->size = line.size();
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
            ret.R = read_int(s);
            pop(s, ","); ret.G = read_int(s);
            pop(s, ","); ret.B = read_int(s);
            pop(s, ","); ret.A = read_int(s);
        pop(s, "]");
        return ret;
    }

    if (name == "merge") {
        ret.type = cMERGE;
        ret.block_id2 = read_id(s);
        return ret;
    }

    return ret;
}

std::string command_to_string(Command *c) {
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

void id_replace_start_if_starts_with_equal_number_and_decrement_if_starts_with_bigger_number(std::string &id, int n, std::string const& replacement) {
    std::string s = std::to_string(n);
    std::string start = "";
    int i = 0;
    for (; i < id.size(); ++i) {
        if (id[i] == '.') break;
        warning('0' <= id[i] && id[i] <= '9');
        start.push_back(id[i]);
    }
    warning(i == id.size() || id[i] == '.');
    int start_n;
    {
        std::stringstream ss;
        ss << start;
        ss >> start_n;
    }
    std::string end = "";
    for (int j = i; j < id.size(); ++j) end.push_back(id[j]);

    if (start_n == n) {
        warning(replacement != "");
        id = replacement;
        id += end;
    } else if (start_n > n) {
        id = std::to_string(start_n - 1);
        id += end;
    }
}

bool string_starts_with(std::string const& s, std::string const& start) {
    for (int i = 0; i < start.size(); ++i) {
        if (i == s.size()) return false;
        if (s[i] != start[i]) return false;
    }
    return true;
}

void id_replace_start_if_starts_with(std::string &id, std::string const& to_replace, std::string const& replacement) {
    int i = 0;
    std::string start = "";
    for (; i < to_replace.size(); ++i) {
        if (i == id.size()) break;
        if (id[i] != to_replace[i]) return;
    }

    std::string end = "";
    for (int j = i; j < id.size(); ++j) end.push_back(id[j]);
    id = replacement;
    id += end;
}

int main(int n_args, char **args) {
    std::string text;
    if (n_args != 2) {
        //printf("Usage %s <input.isl>.\n Make some optimisations to input file and print the result to stdout.\n", args[0]);
        //return 0;
        text = read_entire_file("log2.log");
    } else {
        text = read_entire_file(args[1]);
    }

    std::vector<std::string> lines = lines_of_the_string(text);
    std::vector<Command> commands;
    commands.reserve(lines.size());
    for (std::string const& line : lines) {
        if (line == "" || line == "\n") continue;
        commands.emplace_back(line_to_command(line));
        // std::cout << command_to_string(&commands.back()) << "\n";
    }

    std::cerr << "size = " << commands.size() << "\n";

    int n_optimized = 0;
    for (;;) {
        /*
        std::cout << "#################################################\n";
        for (Command& c : commands) {
            std::cout << command_to_string(&c) << "\n";
        }
        */


        bool found_something = false;
        int merge_new_id = 1;
        for (int i = 0; i + 3 < commands.size(); ++i) {
            //std::cerr << "check from " << i << "\n";
            Command *c0 = &commands[i];
            Command *c1 = &commands[i+1];
            Command *c2 = &commands[i+2];
            Command *c3 = &commands[i+3];
            if (c0->type == cMERGE) {
                merge_new_id += 1;
                continue;
            }

            if (c0->type == cSET_COLOR
                &&c1->type == cCUT_LINE
                &&c2->type == cSET_COLOR
                &&c3->type == cMERGE) {
                u8 R, G, B, A;
                R = c0->R; G = c0->G; B = c0->B; A = c0->A;
                if (c2->R != R || c2->G != G || c2->B != B || c2->A != A) continue;

                std::string const& id = c0->block_id;
                if (c1->block_id != id) continue;
                if (c2->block_id != id + ".0"
                    && c2->block_id != id + ".1") continue;
                if (!(c3->block_id == id + ".0" && c3->block_id2 == id + ".1")) {
                    if (!(c3->block_id == id + ".1" && c3->block_id2 == id + ".0")) continue;
                }

                for (int j = i + 1; j + 3 < commands.size(); ++j) {
                    //if (j <= i + 3) std::cerr << command_to_string(&commands[j]) << " was optimized " << command_to_string(&commands[j+3]) << "took it's place\n";
                    commands[j] = std::move(commands[j+3]);
                }
                commands.pop_back();
                commands.pop_back();
                commands.pop_back();
                for (int j = i + 1; j < commands.size(); ++j) {
                    Command *c = &commands[j];
                    id_replace_start_if_starts_with_equal_number_and_decrement_if_starts_with_bigger_number(c->block_id, merge_new_id, c0->block_id);
                    if (c->block_id2 != "") {
                        id_replace_start_if_starts_with_equal_number_and_decrement_if_starts_with_bigger_number(c->block_id2, merge_new_id, c0->block_id);
                    }
                }

                found_something = true;
                ++n_optimized;
                break;
            }
        }
        if (!found_something) break;
    }

    for (;;) {
        /*
        std::cout << "#################################################\n";
        for (Command& c : commands) {
            std::cout << command_to_string(&c) << "\n";
        }
        */
        bool found_something = false;
        for (int i = commands.size() - 1; i - 2 >= 0; --i) {
            Command *c0 = &commands[i-2];
            Command *c1 = &commands[i-1];
            Command *c2 = &commands[i];
            if (c0->type != cSET_COLOR) continue;
            if (c1->type != cCUT_LINE)  continue; // todo cCUT_POINT
            if (c2->type != cSET_COLOR) continue;
            u8 R, G, B, A;
            R = c0->R; G = c0->G; B = c0->B; A = c0->A;
            if (c2->R != R || c2->G != G || c2->B != B || c2->A != A) continue;

            std::string const& id = c0->block_id;
            if (c2->block_id != id + ".0" && c2->block_id != id + ".1") continue;
            for (int j = i; j + 1 < commands.size(); ++j) {
                commands[j] = std::move(commands[j+1]);
            }
            commands.pop_back();

            found_something = true;
            ++n_optimized;
        }
        if (!found_something) break;
    }

    for (;;) {
        /*
        std::cout << "#################################################\n";
        for (Command& c : commands) {
            std::cout << command_to_string(&c) << "\n";
        }
        */

        int merge_new_id = 1;
        bool found_something = false;
        for (int i = 0; i+1 < commands.size(); ++i) {
            std::string id;
            {
                Command *c0 = &commands[i];
                Command *c1 = &commands[i+1];
                if (c0->type == cMERGE) {
                    merge_new_id += 1;
                    continue;
                }
                if (c0->type != cCUT_LINE) continue;  // todo cCUT_POINT
                if (c1->type != cMERGE) continue;

                id = c0->block_id;
                if (c1->block_id != id + ".0" && c1->block_id != id + ".1") continue;
                for (int j = i; j + 2 < commands.size(); ++j) {
                    commands[j] = std::move(commands[j+2]);
                }
                commands.pop_back();
                commands.pop_back();
            }

            for (int j = i; j < commands.size(); ++j) {
                Command *c = &commands[j];
                id_replace_start_if_starts_with_equal_number_and_decrement_if_starts_with_bigger_number(c->block_id, merge_new_id, id);
                if (c->block_id2 != "") {
                    id_replace_start_if_starts_with_equal_number_and_decrement_if_starts_with_bigger_number(c->block_id2, merge_new_id, id);
                }
            }
            found_something = true;
            ++n_optimized;
        }
        if (!found_something) break;
    }

    for (;;) {
        /*
        std::cout << "#################################################\n";
        for (Command& c : commands) {
            std::cout << command_to_string(&c) << "\n";
        }
        */



        bool found_something = false;
        int merge_new_id = 1;
        for (int i = 0; i + 3 < commands.size(); ++i) {
            //std::cerr << "check from " << i << "\n";
            Command *cut0   = &commands[i];
            Command *cut1   = &commands[i+1];
            if (cut0->type == cMERGE) {
                merge_new_id += 1;
                continue;
            }

            if (cut0->type != cCUT_LINE) continue;
            if (cut1->type != cCUT_LINE) continue;

            char axis = cut0->axis;
            if (cut1->axis != axis) continue;

            if (cut1->block_id == cut0->block_id + ".1") {
                std::string unused0 = cut0->block_id + ".0";
                std::string unused1 = cut1->block_id + ".0";

                bool unused_are_unused = false;
                std::string id_closest = cut1->block_id + ".1";
                int merge_internal_part_id = merge_new_id;
                for (int j = i+2; j + 1 < commands.size(); ++j) {
                    if (j > i + 2 && commands[j-1].type == cMERGE) merge_internal_part_id += 1;

                    {
                        Command *c = &commands[j];
                        if (c->type == cMERGE && c->block_id == unused1) {}
                        else
                        if (c->block_id == id_closest || c->block_id2 == id_closest) {
                            if (c->type != cCUT_LINE && c->type != cMERGE && c->type != cSET_COLOR) break;

                            if (c->type == cCUT_LINE) {
                                if (c->axis != axis) break; // пока что не разрешаем резать по другой оси, а то сложно следить за нужной границей
                                id_closest = c->block_id + ".0";
                                continue;
                            }

                            if (c->type == cMERGE) {
                                id_closest = std::to_string(merge_internal_part_id);
                                continue;
                            }
                        }
                    }

                    Command *merge1 = &commands[j];
                    // TODO: если встетилось слияние двух нетронутых блоков, то ведь тоже можно кое-что почистить
                    if (merge1->block_id == unused0 || merge1->block_id2 == unused0) break;
                    if (merge1->block_id != unused1) continue; // TODO: не полагаться на то что нужный айдишник первый
                    if (merge1->type != cMERGE) break;
                    if (merge1->block_id2 != id_closest) break;

                    Command *merge0 = &commands[j+1];
                    if (merge0->type     != cMERGE) break;
                    if (merge0->block_id != unused0) break; // TODO: не полагаться на то что нужный айдишник первый
                    if (merge0->block_id2 != std::to_string(merge_internal_part_id)) break;

                    cut0->y = cut1->y;
                    cut0->x = cut1->x;

                    for (int k = i + 1; k + 1 < j; ++k) {
                        commands[k] = std::move(commands[k+1]);
                    }
                    commands[j-1] = *merge0;
                    merge0 = &commands[j-1];
                    for (int k = j; k + 2 < commands.size(); ++k) {
                        commands[k] = std::move(commands[k+2]);
                    }
                    commands.pop_back();
                    commands.pop_back();


                    for (int k = i+1; k < j-1; ++k) {
                        Command *c = &commands[k];
                        id_replace_start_if_starts_with(c->block_id, cut1->block_id + ".1", cut0->block_id + ".1");
                        if (c->block_id2 != "") {
                            id_replace_start_if_starts_with(c->block_id2, cut1->block_id + ".1", cut0->block_id + ".1");
                        }
                    }

                    warning(merge0->block_id2 == std::to_string(merge_internal_part_id));
                    merge0->block_id2 = id_closest;

                    for (int k = j; k < commands.size(); ++k) {
                        Command *c = &commands[k];
                        id_replace_start_if_starts_with_equal_number_and_decrement_if_starts_with_bigger_number(c->block_id, merge_internal_part_id, "");
                        if (c->block_id2 != "") {
                            id_replace_start_if_starts_with_equal_number_and_decrement_if_starts_with_bigger_number(c->block_id2, merge_internal_part_id, "");
                        }
                    }

                    ++n_optimized;
                    unused_are_unused = true;
                    found_something = true;
                    break;
                }
                if (!unused_are_unused) continue;
            }
            break;
        }
        if (!found_something) break;
    }

    // std::cout << "+++\n";
    std::cerr << "optimized " << n_optimized << " things\n";
    for (Command& c : commands) {
        std::cout << command_to_string(&c) << "\n";
    }
    return 0;
}
