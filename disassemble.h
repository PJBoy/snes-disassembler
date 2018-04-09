#pragma once

#include "utility.h"

#include "common_utility/type_traits.h"

#include <iomanip>
#include <iostream>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <utility>

template<typename It>
class Disassemble
{
/*
                            0   1   2   3   4   5   6   7
    00 = ooo                BRK     RTI RTS
    00 = JSR xxxx               JSR
    00 = BRA xx                             BRA
    00 = ooo #xxxx                              LDY CPY CPX
    01 = ooo (xx,X)         ORA AND EOR ADC STA LDA CMP SBC
    02 = ooo                COP     WDM
    02 = JSR xxxxxx             JSR
    02 = ooo xxxx                       PER BRL
    02 = LDX #xxxx                              LDX
    02 = oEP #xx                                    REP SEP
    03 = ooo xx,S           ORA AND EOR ADC STA LDA CMP SBC
    04 = ooo xx             TSB BIT     STZ STY LDY CPY CPX
    04 = MVP xx yy                  MVP
    05 = ooo xx             ORA AND EOR ADC STA LDA CMP SBC
    06 = ooo xx             ASL ROL LSR ROR STX LDX DEC INC
    07 = ooo [xx]           ORA AND EOR ADC STA LDA CMP SBC
    08 = ooo                PHP PLP PHA PLA DEY TAY INY INX
    09 = ooo #xxxx          ORA AND EOR ADC BIT LDA CMP SBC
    0A = ooo                ASL ROL LSR ROR TXA TAX DEC NOP
    0B = ooo                PHD PLD PHK RTL PHB PLB WAI XBA
    0C = ooo xxxx           TSB BIT JMP     STY LDY CPY CPX
    0C = JMP (xxxx)                     JMP
    0D = ooo xxxx           ORA AND EOR ADC STA LDA CMP SBC
    0E = xxxx               ASL ROL LSR ROR STX LDX DEC INC
    0F = ooo xxxxxx         ORA AND EOR ADC STA LDA CMP SBC
    10 = Boo xx             BPL BMI BVC BVS BCC BCS BNE BEQ
    11 = ooo (xx),Y         ORA AND EOR ADC STA LDA CMP SBC
    12 = ooo (xx)           ORA AND EOR ADC STA LDA CMP SBC
    13 = ooo (xx,S),Y       ORA AND EOR ADC STA LDA CMP SBC
    14 = ooo xx             TRB
    14 = ooo xx,X               BIT     STZ STY LDY
    14 = MVN xx yy                  MVN
    14 = PEI (xx)                                   PEI
    14 = PEA xxxx                                       PEA
    15 = ooo xx,X           ORA AND EOR ADC STA LDA CMP SBC
    16 = ooo xx,X           ASL ROL LSR ROR STX LDX DEC INC
    17 = ooo [xx],Y         ORA AND EOR ADC STA LDA CMP SBC
    18 = ooo                CLC SEC CLI SEI TYA CLV CLD SED
    19 = ooo xxxx,Y         ORA AND EOR ADC STA LDA CMP SBC
    1A = ooo                INC DEC PHY PLY TXS TSX PHX PLX
    1B = ooo                TCS TSC TCD TDC TXY TYX STP XCE
    1C = ooo xxxx           TRB             STZ
    1C = ooo xxxx,X             BIT             LDY
    1C = JMP xxxxxx                 JMP
    1C = Joo (xxxx,X)                   JMP             JSR
    1C = JMP [xxxxxx]                               JMP
    1D = ooo xxxx,X         ORA AND EOR ADC STA LDA CMP SBC
    1E = ooo xxxx,X         ASL ROL LSR ROR STZ     DEC INC
    1E = ooo xxxx,Y                             LDX
    1F = ooo xxxxxx,X       ORA AND EOR ADC STA LDA CMP SBC
*/

    const inline static std::string operations_alu0[]{"ORA"s, "AND"s, "EOR"s, "ADC"s, "STA"s, "LDA"s, "CMP"s, "SBC"s};

    It it_rom, end_rom;
    long_t romAddress;
    bool m, x;
    std::set<long_t> branchTargets;


    byte_t loadByte()
    {
        if (it_rom >= end_rom)
            throw std::runtime_error("Passed end of ROM range mid instruction");

        ++romAddress;
        return *it_rom++;
    }

    word_t loadWord()
    {
        return loadByte() | loadByte() << 8;
    }

    long_t loadLong()
    {
        return loadWord() | loadByte() << 16;
    }

    template<typename Int>
    static std::string format(Int data)
    {
        std::ostringstream out;
        out << '$' << std::hex << std::uppercase << std::setfill('0') << std::setw(std::min(size_t(3), sizeof(Int)) * 2) << +data;
        return out.str();
    }

    static std::string formatRomAddress(long_t address)
    {
        std::ostringstream out;
        out << '$' << std::hex << std::uppercase << std::setfill('0') << std::setw(2) << (address >> 16) << ':' << std::setw(4) << (address & 0xFFFF);
        return out.str();
    }

    static std::string formatByte(byte_t v)
    {
        std::ostringstream out;
        out << std::hex << std::uppercase << std::setfill('0') << std::setw(2) << +v;
        return out.str();
    }

    static std::string formatWord(word_t v)
    {
        std::ostringstream out;
        out << std::hex << std::uppercase << std::setfill('0') << std::setw(4) << v;
        return out.str();
    }

    std::string handleAImmediate()
    {
        if (m)
            return "#"s + format(loadByte());

        return "#"s + format(loadWord());
    }

    std::string handleXYImmediate()
    {
        if (x)
            return "#"s + format(loadByte());

        return "#"s + format(loadWord());
    }

    std::string handleByteAddress()
    {
        byte_t v(loadByte());
        return format(v) + "    [$7E:00"s + formatByte(v) + ']';
    }

    std::string handleWordAddress()
    {
        word_t v(loadWord());
        if (v < 0x8000)
            return format(v) + "  [$7E:"s + formatWord(v) + ']';
        else
            return format(v) + "  ["s + format(byte_t(hex2snes(romAddress) >> 16)) + ":"s + formatWord(v) + ']';
    }

    std::string handleLongAddress()
    {
        long_t v(loadLong());
        return format(v) + '[' + formatRomAddress(v) + ']';
    }

    std::string handleRelativeByteAddress()
    {
        byte_t v(loadByte());
        long_t target(romAddress + std::make_signed_t<byte_t>(v));
        branchTargets.insert(target);
        return format(v) + "    ["s + format(word_t(hex2snes(target))) + ']';
    }

    std::string handleRelativeWordAddress()
    {
        word_t v(loadWord());
        long_t target(romAddress + std::make_signed_t<word_t>(v));
        branchTargets.insert(target);
        return format(v) + "  ["s + format(word_t(hex2snes(target))) + ']';
    }

    std::string formatOperandBytes(n_t n) const
    {
        if (it_rom + n > end_rom)
            throw std::runtime_error("Passed end of ROM range mid instruction");

        std::ostringstream out;
        for (index_t i{}; i < n; ++i)
            out << formatByte(it_rom[i]) << ' ';

        std::ostringstream out_padded;
        out_padded << std::setfill(' ') << std::setw(9) << std::left << out.str();
        return out_padded.str();
    }

    void addressMode0(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        0  = ooo                BRK     RTI RTS
        0  = JSR xxxx               JSR
        0  = BRA xx                             BRA
        0  = ooo #xxxx                              LDY CPY CPX
    */
        
        const static std::string operations[]{"BRK"s, "JSR"s, "RTI"s, "RTS"s, "BRA"s, "LDY"s, "CPY"s, "CPX"s};

        switch (operation)
        {
            case 0:
            case 2:
            case 3:
                std::cout << formatOperandBytes(0) << operations[operation];
                break;

            case 1:
                std::cout << formatOperandBytes(2) << operations[operation];
                std::cout << ' ' << handleWordAddress();
                break;

            case 4:
                std::cout << formatOperandBytes(1) << operations[operation];
                std::cout << ' ' << handleRelativeByteAddress();
                break;

            case 5:
            case 6:
            case 7:
                std::cout << formatOperandBytes(2 - x) << operations[operation];
                std::cout << ' ' << handleXYImmediate();
                break;
        }
    }

    void indexedIndirect(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        2  = ooo (xx,X)         ORA AND EOR ADC STA LDA CMP SBC
    */

        std::cout << formatOperandBytes(1) << operations_alu0[operation];
        std::cout << " ("s << format(loadByte()) << ",x)"s;
    }

    void addressMode4(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        4  = ooo                COP     WDM
        4  = JSR xxxxxx             JSR
        4  = ooo xxxx                       PER BRL
        4  = LDX #xxxx                              LDX
        4  = oEP #xx                                    REP SEP
    */
        
        const static std::string operations[]{"COP"s, "JSL"s, "WDM"s, "PER"s, "BRL"s, "LDX"s, "REP"s, "SEP"s};

        switch (operation)
        {
            case 0:
            case 2:
                std::cout << formatOperandBytes(0) << operations[operation];
                break;

            case 1:
                std::cout << formatOperandBytes(3) << operations[operation];
                std::cout << ' ' << handleLongAddress();
                break;

            case 3:
            case 4:
                std::cout << formatOperandBytes(2) << operations[operation];
                std::cout << ' ' << handleRelativeWordAddress();
                break;

            case 5:
                std::cout << formatOperandBytes(2 - x) << operations[operation];
                std::cout << ' ' << handleXYImmediate();
                break;

            case 6:
            case 7:
                std::cout << formatOperandBytes(1) << operations[operation];
                std::cout << " #"s << format(loadByte());
                break;
        }
    }

    void stackRelative(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        6  = ooo xx,S           ORA AND EOR ADC STA LDA CMP SBC
    */

        std::cout << formatOperandBytes(1) << operations_alu0[operation];
        std::cout << ' ' << format(loadByte()) << ",s"s;
    }

    void addressMode8(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        8  = ooo xx             TSB BIT     STZ STY LDY CPY CPX
        8  = MVP xx yy                  MVP
    */

        const static std::string operations[]{"TSB"s, "BIT"s, "MVP"s, "STZ"s, "STY"s, "LDY"s, "CPY"s, "CPX"s};

        if (operation == 2)
        {
            std::cout << formatOperandBytes(2) << operations[operation];
            std::cout << ' ' << format(loadByte());
            std::cout << ' ' << format(loadByte());
        }
        else
        {
            std::cout << formatOperandBytes(1) << operations[operation];
            std::cout << ' ' << handleByteAddress();
        }
    }

    void directPageAlu0(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        A  = ooo xx             ORA AND EOR ADC STA LDA CMP SBC
    */

        std::cout << formatOperandBytes(1) << operations_alu0[operation];
        std::cout << ' ' << handleByteAddress();
    }

    void directPageAlu1(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        C  = ooo xx             ASL ROL LSR ROR STX LDX DEC INC
    */

        const static std::string operations[]{"ASL"s, "ROL"s, "LSR"s, "ROR"s, "STX"s, "LDX"s, "DEC"s, "INC"s};

        std::cout << formatOperandBytes(1) << operations[operation];
        std::cout << ' ' << handleByteAddress();
    }

    void indirectLong(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        E  = ooo [xx]           ORA AND EOR ADC STA LDA CMP SBC
    */

        std::cout << formatOperandBytes(1) << operations_alu0[operation];
        std::cout << " ["s << format(loadByte()) << "]"s;
    }

    void implied0(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        10 = ooo                PHP PLP PHA PLA DEY TAY INY INX
    */

        const static std::string operations[]{"PHP"s, "PLP"s, "PHA"s, "PLA"s, "DEY"s, "TAY"s, "INY"s, "INX"s};

        std::cout << formatOperandBytes(0) << operations[operation];
    }

    void immediate(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        12 = ooo #xxxx          ORA AND EOR ADC BIT LDA CMP SBC
    */

        const static std::string operations[]{"ORA"s, "AND"s, "EOR"s, "ADC"s, "BIT"s, "LDA"s, "CMP"s, "SBC"s};

        std::cout << formatOperandBytes(2 - m) << operations[operation];
        std::cout << ' ' << handleAImmediate();
    }

    void implied2(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        14 = ooo                ASL ROL LSR ROR TXA TAX DEC NOP
    */

        const static std::string operations[]{"ASL"s, "ROL"s, "LSR"s, "ROR"s, "TXA"s, "TAX"s, "DEC"s, "NOP"s};

        std::cout << formatOperandBytes(0) << operations[operation];
    }

    void implied4(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        16 = ooo                PHD PLD PHK RTL PHB PLB WAI XBA
    */

        const static std::string operations[]{"PHD"s, "PLD"s, "PHK"s, "RTL"s, "PHB"s, "PLB"s, "WAI"s, "XBA"s};

        std::cout << formatOperandBytes(0) << operations[operation];
    }

    void addressMode18(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        18 = ooo xxxx           TSB BIT JMP     STY LDY CPY CPX
        18 = JMP (xxxx)                     JMP
    */

        const static std::string operations[]{"TSB"s, "BIT"s, "JMP"s, "JMP"s, "STY"s, "LDY"s, "CPY"s, "CPX"s};

        std::cout << formatOperandBytes(2) << operations[operation] << ' ';
        if (operation == 3)
            std::cout << '(' << format(loadWord()) << ')';
        else
            std::cout << handleWordAddress();
    }

    void direct(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        1A = ooo xxxx           ORA AND EOR ADC STA LDA CMP SBC
    */

        std::cout << formatOperandBytes(2) << operations_alu0[operation];
        std::cout << ' ' << handleWordAddress();
    }

    void implied6(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        1C = xxxx               ASL ROL LSR ROR STX LDX DEC INC
    */

        const static std::string operations[]{"ASL"s, "ROL"s, "LSR"s, "ROR"s, "STX"s, "LDX"s, "DEC"s, "INC"s};

        std::cout << formatOperandBytes(0) << operations[operation];
    }

    void directLong(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        1E = ooo xxxxxx         ORA AND EOR ADC STA LDA CMP SBC
    */

        std::cout << formatOperandBytes(3) << operations_alu0[operation];
        std::cout << ' ' << handleLongAddress();
    }

    void branch(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        1  = Boo xx             BPL BMI BVC BVS BCC BCS BNE BEQ
    */

        const static std::string operations[]{"BPL"s, "BMI"s, "BVC"s, "BVS"s, "BCC"s, "BCS"s, "BNE"s, "BEQ"s};

        std::cout << formatOperandBytes(1) << operations[operation];
        std::cout << ' ' << handleRelativeByteAddress();
    }

    void indirectIndexed(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        3  = ooo (xx),Y         ORA AND EOR ADC STA LDA CMP SBC
    */

        std::cout << formatOperandBytes(1) << operations_alu0[operation];
        std::cout << " ("s << format(loadByte()) << "),y"s;
    }

    void indirectShort(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        5  = ooo (xx)           ORA AND EOR ADC STA LDA CMP SBC
    */

        std::cout << formatOperandBytes(1) << operations_alu0[operation];
        std::cout << " ("s << format(loadByte()) << ")"s;
    }

    void stackRelativeIndirectIndexed(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        7  = ooo (xx,S),Y       ORA AND EOR ADC STA LDA CMP SBC
    */

        std::cout << formatOperandBytes(1) << operations_alu0[operation];
        std::cout << " ("s << format(loadByte()) << ",s),y"s;
    }

    void addressMode9(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        9  = ooo xx             TRB
        9  = ooo xx,X               BIT     STZ STY LDY
        9  = MVN xx yy                  MVN
        9  = PEI (xx)                                   PEI
        9  = PEA xxxx                                       PEA
    */

        const static std::string operations[]{"TRB"s, "BIT"s, "MVN"s, "STZ"s, "STY"s, "LDY"s, "PEI"s, "PEA"s};

        switch (operation)
        {
            case 0:
                std::cout << formatOperandBytes(1) << operations[operation];
                std::cout << ' ' << handleByteAddress();
                break;

            case 1:
            case 3:
            case 4:
            case 5:
                std::cout << formatOperandBytes(1) << operations[operation];
                std::cout << ' ' << handleByteAddress() << ",x"s;
                break;

            case 2:
                std::cout << formatOperandBytes(2) << operations[operation];
                std::cout << ' ' << format(loadByte());
                std::cout << ' ' << format(loadByte());
                break;

            case 6:
                std::cout << formatOperandBytes(1) << operations[operation];
                std::cout << " ("s << format(loadByte()) << ')';
                break;

            case 7:
                std::cout << formatOperandBytes(2) << operations[operation];
                std::cout << ' ' << format(loadWord());
                break;
        }
    }

    void indexdDirectPageAlu0(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        B  = ooo xx,X           ORA AND EOR ADC STA LDA CMP SBC
    */

        std::cout << formatOperandBytes(1) << operations_alu0[operation];
        std::cout << ' ' << format(loadByte());
    }

    void indexdDirectPageAlu1(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        D  = ooo xx,X           ASL ROL LSR ROR STX LDX DEC INC
    */

        const static std::string operations[]{"ASL"s, "ROL"s, "LSR"s, "ROR"s, "STX"s, "LDX"s, "DEC"s, "INC"s};

        std::cout << formatOperandBytes(1) << operations[operation];
        std::cout << ' ' << format(loadByte());
    }

    void indirectLongIndexed(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        F  = ooo [xx],Y         ORA AND EOR ADC STA LDA CMP SBC
    */

        std::cout << formatOperandBytes(1) << operations_alu0[operation];
        std::cout << " ["s << format(loadByte()) << "],y"s;
    }

    void implied1(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        11 = ooo                CLC SEC CLI SEI TYA CLV CLD SED
    */

        const static std::string operations[]{"CLC"s, "SEC"s, "CLI"s, "SEI"s, "TYA"s, "CLV"s, "CLD"s, "SED"s};

        std::cout << formatOperandBytes(0) << operations[operation];
    }

    void indexedY(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        13 = ooo xxxx,Y         ORA AND EOR ADC STA LDA CMP SBC
    */

        std::cout << formatOperandBytes(2) << operations_alu0[operation];
        std::cout << ' ' << format(loadWord()) << ",y"s;
    }

    void implied3(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        15 = ooo                INC DEC PHY PLY TXS TSX PHX PLX
    */

        const static std::string operations[]{"INC"s, "DEC"s, "PHY"s, "PLY"s, "TXS"s, "TSX"s, "PHX"s, "PLX"s};

        std::cout << formatOperandBytes(0) << operations[operation];
    }

    void implied5(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        17 = ooo                TCS TSC TCD TDC TXY TYX STP XCE
    */

        const static std::string operations[]{"TCS"s, "TSC"s, "TCD"s, "TDC"s, "TXY"s, "TYX"s, "STP"s, "XCE"s};

        std::cout << formatOperandBytes(0) << operations[operation];
    }

    void addressMode19(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        19 = ooo xxxx           TRB             STZ
        19 = ooo xxxx,X             BIT             LDY
        19 = JMP xxxxxx                 JMP
        19 = Joo (xxxx,X)                   JMP             JSR
        19 = JMP [xxxxxx]                               JMP
    */

        const static std::string operations[]{"TRB"s, "BIT"s, "JML"s, "JMP"s, "STZ"s, "LDY"s, "JML"s, "JSR"s};

        switch (operation)
        {
            case 0:
            case 4:
                std::cout << formatOperandBytes(2) << operations[operation];
                std::cout << ' ' << handleWordAddress();
                break;

            case 1:
            case 5:
                std::cout << formatOperandBytes(2) << operations[operation];
                std::cout << ' ' << format(loadWord()) << ",x"s;
                break;

            case 2:
                std::cout << formatOperandBytes(3) << operations[operation];
                std::cout << ' ' << handleLongAddress();
                break;

            case 3:
            case 7:
                std::cout << formatOperandBytes(2) << operations[operation];
                std::cout << " ("s << format(loadWord()) << ",x)"s;
                break;

            case 6:
                std::cout << formatOperandBytes(3) << operations[operation];
                std::cout << " ["s << format(loadLong()) << ']';
                break;
        }
    }

    void indexedX(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        1B = ooo xxxx,X         ORA AND EOR ADC STA LDA CMP SBC
    */

        std::cout << formatOperandBytes(2) << operations_alu0[operation];
        std::cout << ' ' << format(loadWord()) << ",x"s;
    }

    void addressMode1D(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        1D = ooo xxxx,X         ASL ROL LSR ROR STZ     DEC INC
        1D = ooo xxxx,Y                             LDX
    */

        const static std::string operations[]{"ASL"s, "ROL"s, "LSR"s, "ROR"s, "STZ"s, "LDX"s, "DEC"s, "INC"s};

        std::cout << formatOperandBytes(2) << operations[operation];
        std::cout << ' ' << handleWordAddress() << ',' << 'x' + (operation == 5);
    }

    void indexedLong(unsigned operation)
    {
    /*
                                0   1   2   3   4   5   6   7
        1F = ooo xxxxxx,X       ORA AND EOR ADC STA LDA CMP SBC
    */

        std::cout << formatOperandBytes(3) << operations_alu0[operation];
        std::cout << ' ' << format(loadLong()) << ",x"s;
    }


    void disassembleInstruction(byte_t opcode)
    {
        constexpr void (Disassemble::*addressModeHandlers[])(unsigned)
        {
            &Disassemble::addressMode0,
            &Disassemble::indexedIndirect,
            &Disassemble::addressMode4,
            &Disassemble::stackRelative,
            &Disassemble::addressMode8,
            &Disassemble::directPageAlu0,
            &Disassemble::directPageAlu1,
            &Disassemble::indirectLong,
            &Disassemble::implied0,
            &Disassemble::immediate,
            &Disassemble::implied2,
            &Disassemble::implied4,
            &Disassemble::addressMode18,
            &Disassemble::direct,
            &Disassemble::implied6,
            &Disassemble::directLong,
            &Disassemble::branch,
            &Disassemble::indirectIndexed,
            &Disassemble::indirectShort,
            &Disassemble::stackRelativeIndirectIndexed,
            &Disassemble::addressMode9,
            &Disassemble::indexdDirectPageAlu0,
            &Disassemble::indexdDirectPageAlu1,
            &Disassemble::indirectLongIndexed,
            &Disassemble::implied1,
            &Disassemble::indexedY,
            &Disassemble::implied3,
            &Disassemble::implied5,
            &Disassemble::addressMode19,
            &Disassemble::indexedX,
            &Disassemble::addressMode1D,
            &Disassemble::indexedLong
        };

        std::cout << formatByte(opcode) << ' ';

        const unsigned
            addressMode(opcode & 0x1F),
            operation(opcode >> 5);

        (this->*addressModeHandlers[addressMode])(operation);
    }

public:
    Disassemble(It begin_rom, It end_rom, long_t romBeginAddress, bool m = false, bool x = false)
        : it_rom(begin_rom), end_rom(end_rom), romAddress(snes2hex(romBeginAddress)), m(m), x(x)
    {
        static_assert(isRandomAccessIterator<It>);

        for (; it_rom != end_rom;)
        {
            if (branchTargets.extract(romAddress))
                std::cout << '\n';

            std::cout << formatRomAddress(hex2snes(romAddress++)) << ' ';
            disassembleInstruction(*it_rom++);
            std::cout << '\n';
        }
    }
};

template<typename It>
void disassemble(It&& rom_begin, It&& rom_end, long_t romBeginAddress, bool m = false, bool x = false)
{
    Disassemble<It>(std::forward<It>(rom_begin), std::forward<It>(rom_end), romBeginAddress);
}
