#pragma once

#include <cstdint>
constexpr bool DEBUG{true};

constexpr auto FRAMES_MAX{256};
constexpr auto STACK_MAX{FRAMES_MAX * UINT8_MAX};
