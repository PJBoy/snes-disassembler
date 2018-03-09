#include "disassemble.h"
#include "utility.h"

#include "common_utility/iterator.h"
#include "common_utility/type_traits.h"

#include <cstddef>
#include <experimental/filesystem>
#include <fstream>
#include <iostream>
#include <iterator>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>
#include <valarray>


struct ProgramOptions
{
    std::experimental::filesystem::path romFilepath;
    long_t romBeginAddress, romEndAddress;
    bool m{}, x{};

    template<typename It>
    ProgramOptions(It begin_args, It end_args)
    {
        static_assert(isRandomAccessIterator<It>);

        if (end_args - begin_args < 4)
            throw std::runtime_error("Invalid number of arguments");

        romFilepath = std::move(begin_args[1]);
        romBeginAddress = std::stoul(begin_args[2], nullptr, 16);
        romEndAddress   = std::stoul(begin_args[3], nullptr, 16);
        for (It it_args(begin_args + 4); it_args != end_args; ++it_args)
            if (*it_args == "m"s)
                m = true;
            else if (*it_args == "x"s)
                x = true;
            else
                throw std::runtime_error("Invalid argument: " + *it_args);

        if (romEndAddress < romBeginAddress)
            throw std::runtime_error("ROM end address is less than begin address");
    }
};


std::string usage(std::string program)
{
    if (std::empty(program))
        program = "<this executable>"s;

    return program + " <ROM filepath> <begin address> <end address> [m] [x]\n"s;
}

int main(unsigned argc, char* argv[])
try
{
    std::vector<std::string> args(argv, argv + argc);
    ProgramOptions programOptions(std::make_move_iterator(std::begin(args)), std::make_move_iterator(std::end(args)));
    const long_t romSize(snes2hex(programOptions.romEndAddress) - snes2hex(programOptions.romBeginAddress));
    std::valarray<byte_t> rom(romSize);
    {
        std::ifstream romFile(programOptions.romFilepath);
        romFile.seekg(snes2hex(programOptions.romBeginAddress));
        romFile.read(reinterpret_cast<char*>(data(rom)), romSize);
    }

    disassemble(std::begin(rom), std::end(rom), programOptions.romBeginAddress, programOptions.m, programOptions.x);
}
catch (const std::exception& e)
{
    std::cerr
        << e.what() << '\n'
        << usage(argv[0]) << '\n';

    return EXIT_FAILURE;
}
