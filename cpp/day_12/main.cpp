#include <functional>
#include <optional>
#include <set>
#include <map>
#include <vector>
#include <memory>

#include <iostream>
#include <chrono>

using Name = const char*; // std::string_view;
struct Cave;
using CaveRef = const Cave*;

struct Cave {
    Name name;
    bool is_small;
    std::vector<CaveRef> neighbors;
};

static const Name START_CAVE = "start";
static const Name END_CAVE = "end";

struct Path {
    CaveRef last;
    bool is_started = false;
    bool is_ended = false;
    bool two_times = false;
    std::shared_ptr<std::set<Name>> visited;

    std::optional<Path> try_append(CaveRef next) const {
        const auto copy = [this, next](auto callback) -> Path {
            auto new_path = *this;
            new_path.last = next;
            callback(new_path);
            return new_path;
        };

        if (!next->is_small) return copy([](auto&) {});

        if (next->name == START_CAVE) {
            if (is_started) return {};
            return copy([](auto& new_path) {
                new_path.is_started = true;
            });
        }

        if (next->name == END_CAVE) {
            if (is_ended) return {};
            return copy([](auto& new_path) {
                new_path.is_ended = true;
            });
        }

        if (visited != nullptr && visited->contains(next->name)) {
            if (two_times) return {};
            return copy([](auto& new_path) {
                new_path.two_times = true;
            });
        }

        return copy([next](auto& new_path) {
            new_path.visited = new_path.visited == nullptr
                               ? std::make_shared<std::set<Name>>()
                               : std::make_shared<std::set<Name>>(*new_path.visited);
            new_path.visited->insert(next->name);
        });
    }
};


int main() {
    const auto start = std::chrono::system_clock::now();

    const std::vector<std::pair<Name, Name>> input{
            {"end", "ry"},
            {"jf",  "jb"},
            {"jf",  "IO"},
            {"jb",  "hz"},
            {"jo",  "LM"},
            {"hw",  "end"},
            {"hw",  "LM"},
            {"hz",  "ry"},
            {"WI",  "start"},
            {"LM",  "start"},
            {"kd",  "jf"},
            {"xi",  "WI"},
            {"hw",  "jb"},
            {"hz",  "jf"},
            {"LM",  "jb"},
            {"jb",  "xi"},
            {"ry",  "jf"},
            {"WI",  "jb"},
            {"end", "hz"},
            {"jo",  "start"},
            {"WI",  "jo"},
            {"xi",  "ry"},
            {"xi",  "LM"},
            {"xi",  "hw"},
            {"jo",  "xi"},
            {"WI",  "jf"},
    };

    const std::map<Name, Cave> caves = std::invoke([&input] {
        std::map<Name, Cave> caves;
        for (const auto&[n1, n2] : input) {
            caves[n1].neighbors.push_back(&caves[n2]);
            caves[n2].neighbors.push_back(&caves[n1]);
        }
        for (auto&[k, v] : caves) {
            v.name = k;
            v.is_small = std::islower(k[0]);
        }
        return std::move(caves);
    });

    std::vector<Path> result;
    std::vector<Path> partial{Path{.last= nullptr}.try_append(&caves.at("end")).value()};
    std::vector<Path> new_partial;
    std::vector<decltype(new_partial)> destroy_later; // to defer destructor calls

    while (!partial.empty()) {
        for (const auto& path : partial) {
            for (const auto next : path.last->neighbors) {
                auto new_path = path.try_append(next);
                if (!new_path) continue;
                if (next->name == START_CAVE) {
                    result.push_back(std::move(*new_path));
                } else {
                    new_partial.push_back(std::move(*new_path));
                }
            }
        }
        std::swap(partial, new_partial);
        destroy_later.push_back(std::move(new_partial));
        new_partial.clear();
    }

    // cleans almost all garbage, takes from 20 to 50 ms
    // destroy_later.clear();

    const auto end = std::chrono::system_clock::now();
    const auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count();
    // elapsed = 90 milliseconds
    std::cout << "elapsed = " << elapsed << " milliseconds" << std::endl;
}
