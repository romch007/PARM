from enum import IntEnum
import sys


def twos_comp(val, bits):
    """compute the 2's complement of int value val"""
    if (val & (1 << (bits - 1))) != 0:  # if sign bit is set e.g., 8bit: 128-255
        val = val - (1 << bits)        # compute negative value
    return val


class Register(IntEnum):
    R0 = 0
    R1 = 1
    R2 = 2
    R3 = 3
    R4 = 4
    R5 = 5
    R6 = 6
    R7 = 7
    PC = 14
    SP = 15

    @staticmethod
    def from_str(value):
        if value == "r0":
            return Register.R0
        elif value == "r1":
            return Register.R1
        elif value == "r2":
            return Register.R2
        elif value == "r3":
            return Register.R3
        elif value == "r4":
            return Register.R4
        elif value == "r5":
            return Register.R5
        elif value == "r6":
            return Register.R6
        elif value == "r7":
            return Register.R7
        elif value == "pc":
            return Register.PC
        elif value == "sp":
            return Register.SP

        raise ValueError(f"invalid register {value}")


class Instruction(IntEnum):
    # Data processing
    LSLS = 0
    LSRS = 1
    ASRS = 2
    ADDS = 3
    SUBS = 4
    ADDS2 = 5
    SUBS2 = 6
    ADDS3 = 7
    SUBS3 = 8
    MOVS = 9
    ANDS = 10
    EORS = 11
    LSLS2 = 12
    LSRS2 = 13
    ASRS2 = 14
    ADCS = 15
    SBCS = 16
    RORS = 17
    TST = 18
    RSBS = 19
    CMP = 20
    CMP2 = 21
    CMN = 22
    ORRS = 23
    MULS = 24
    BICS = 25
    MVNS = 26
    # LOAD / STORE
    STR = 27
    LDR = 28
    # MISC
    ADDSP = 31
    SUBSP = 32
    BEQ = 33
    BNE = 34
    BCS = 35
    BCC = 36
    BMI = 37
    BPL = 38
    BVS = 39
    BVC = 40
    BHI = 41
    BLS = 42
    BGE = 43
    BLT = 44
    BGT = 45
    BLE = 46
    BAL = 47
    B = 48

    @staticmethod
    def from_str(value, args):
        if value == "lsls":
            if "#" in args:
                return Instruction.LSLS

            return Instruction.LSLS2
        elif value == "lsrs":
            if "#" in args:
                return Instruction.LSRS

            return Instruction.LSRS2
        elif value == "asrs":
            if "#" in args:
                return Instruction.ASRS

            return Instruction.ASRS2
        elif value == "adds":
            if "#" in args:
                if args.count(",") == 2:
                    return Instruction.ADDS2
                return Instruction.ADDS3
            return Instruction.ADDS
        elif value == "subs":
            if "#" in args:
                if args.count(",") == 2:
                    return Instruction.SUBS2
                return Instruction.SUBS3
            return Instruction.SUBS
        elif value == "movs":
            return Instruction.MOVS
        elif value == "ands":
            return Instruction.ANDS
        elif value == "eors":
            return Instruction.EORS
        elif value == "adcs":
            return Instruction.ADCS
        elif value == "sbcs":
            return Instruction.SBCS
        elif value == "rors":
            return Instruction.RORS
        elif value == "tst":
            return Instruction.TST
        elif value == "rsbs":
            return Instruction.RSBS
        elif value == "cmp":
            if "#" in args:
                return Instruction.CMP2
            return Instruction.CMP
        elif value == "cmn":
            return Instruction.CMN
        elif value == "orrs":
            return Instruction.ORRS
        elif value == "muls":
            return Instruction.MULS
        elif value == "bics":
            return Instruction.BICS
        elif value == "mvns":
            return Instruction.MVNS
        elif value == "str":
            return Instruction.STR
        elif value == "ldr":
            return Instruction.LDR
        elif value == "add":
            return Instruction.ADDSP
        elif value == "sub":
            return Instruction.SUBSP
        elif value == "beq":
            return Instruction.BEQ
        elif value == "bne":
            return Instruction.BNE
        elif value == "bcs":
            return Instruction.BCS
        elif value == "bcc":
            return Instruction.BCC
        elif value == "bmi":
            return Instruction.BMI
        elif value == "bpl":
            return Instruction.BPL
        elif value == "bvs":
            return Instruction.BVS
        elif value == "bvc":
            return Instruction.BVC
        elif value == "bhi":
            return Instruction.BHI
        elif value == "bls":
            return Instruction.BLS
        elif value == "bge":
            return Instruction.BGE
        elif value == "blt":
            return Instruction.BLT
        elif value == "bgt":
            return Instruction.BGT
        elif value == "ble":
            return Instruction.BLE
        elif value == "bal":
            return Instruction.BAL
        elif value == "b":
            return Instruction.B

    def to_binary(self):
        if self == Instruction.LSLS:
            return 0b00000
        elif self == Instruction.LSRS:
            return 0b00001
        elif self == Instruction.ASRS:
            return 0b00010
        elif self == Instruction.ADDS:
            return 0b0001100
        elif self == Instruction.SUBS:
            return 0b0001101
        elif self == Instruction.ADDS2:
            return 0b0001110
        elif self == Instruction.SUBS2:
            return 0b0001111
        elif self == Instruction.MOVS:
            return 0b00100
        elif self == Instruction.CMP:
            return 0b00101
        elif self == Instruction.ANDS:
            return 0b0100000000
        elif self == Instruction.EORS:
            return 0b0100000001
        elif self == Instruction.LSLS:
            return 0b0100000010
        elif self == Instruction.LSRS:
            return 0b0100000011
        elif self == Instruction.ASRS:
            return 0b0100000100
        elif self == Instruction.ADCS:
            return 0b0100000101
        elif self == Instruction.SBCS:
            return 0b0100000110
        elif self == Instruction.RORS:
            return 0b0100000111
        elif self == Instruction.TST:
            return 0b0100001000
        elif self == Instruction.RSBS:
            return 0b0100001001
        elif self == Instruction.CMP:
            return 0b0100001010
        elif self == Instruction.CMN:
            return 0b0100001011
        elif self == Instruction.MULS:
            return 0b0100001101
        elif self == Instruction.BICS:
            return 0b0100001110
        elif self == Instruction.MVNS:
            return 0b0100001111
        elif self == Instruction.STR:
            return 0b10010
        elif self == Instruction.LDR:
            return 0b10011
        elif self == Instruction.ADDSP:
            return 0b101100000
        elif self == Instruction.SUBSP:
            return 0b101100001
        elif self == Instruction.B:
            return 0b11100
        elif self == Instruction.BEQ:
            return 0b111000000
        elif self == Instruction.BNE:
            return 0b111000001
        elif self == Instruction.BCS:
            return 0b111000010
        elif self == Instruction.BCC:
            return 0b111000011
        elif self == Instruction.BMI:
            return 0b111000100
        elif self == Instruction.BPL:
            return 0b111000101
        elif self == Instruction.BVS:
            return 0b111000110
        elif self == Instruction.BVC:
            return 0b111000111
        elif self == Instruction.BHI:
            return 0b111001000
        elif self == Instruction.BLS:
            return 0b111001001
        elif self == Instruction.BGE:
            return 0b111001010
        elif self == Instruction.BLT:
            return 0b111001011
        elif self == Instruction.BGT:
            return 0b111001100
        elif self == Instruction.BLE:
            return 0b111001101
        elif self == Instruction.BAL:
            return 0b111001110

        raise ValueError(f"Missing binary repr for opcode {self.__repr__()}")


class Immediate:
    def __init__(self, size, value):
        self.size = size
        self.value = value

    @staticmethod
    def from_str(size, value):
        if value[0] != "#":
            raise ValueError("no hashtag at start of immediate")

        return Immediate(size, int(value[1:]))

    def __repr__(self):
        return f"<Immediate: size={self.size},value={self.value}>"


class Argument:
    pass


class TwoRegistersArgument(Argument):
    def __init__(self, first: Register, second: Register):
        self.first = first
        self.second = second

    def __repr__(self):
        return f"<TwoRegistersArgument: first={self.first.__repr__()},second={self.second.__repr__()}>"

    def to_binary(self):
        return (self.first << 3) | self.second

    def binary_width(self):
        return 6


class RdRnRmArgument(Argument):
    def __init__(self, rd: Register, rn: Register, rm: Register):
        self.rd = rd
        self.rn = rn
        self.rm = rm

    def __repr__(self):
        return f"<RdRnRmArgument: rd={self.rd.__repr__()},rn={self.rn.__repr__()},rm={self.rm.__repr__()}>"

    def to_binary(self):
        return (self.rm << 6) | (self.rn << 3) | self.rd

    def binary_width(self):
        return 9


class RdRmImm5Argument(Argument):
    def __init__(self, rd: Register, rm: Register, imm5: Immediate):
        self.rd = rd
        self.rm = rm
        self.imm5 = imm5

    def __repr__(self):
        return f"<RdRmImm5Argument: rd={self.rd.__repr__()},rm={self.rm.__repr__()},imm5={self.imm5.__repr__()}>"

    def to_binary(self):
        return (self.imm5.value << 6) | (self.rm << 3) | self.rd

    def binary_width(self):
        return 11


class RdRnImm3Argument(Argument):
    def __init__(self, rd: Register, rn: Register, imm3: Immediate):
        self.rd = rd
        self.rn = rn
        self.imm3 = imm3

    def __repr__(self):
        return f"<RdRnImm3Argument: rd={self.rd.__repr__()},rn={self.rn.__repr__()},imm3={self.imm3.__repr__()}>"

    def to_binary(self):
        return (self.imm3.value << 6) | (self.rn << 3) | self.rd

    def binary_width(self):
        return 9


class RdImm8Argument(Argument):
    def __init__(self, rd: Register, imm8: Immediate):
        self.rd = rd
        self.imm8 = imm8

    def __repr__(self):
        return f"<RdImm8Argument: rd={self.rd.__repr__()},imm8={self.imm8.__repr__()}>"

    def to_binary(self):
        return (self.rd << 8) | self.imm8.value

    def binary_width(self):
        return 11


class RtSpImm8(Argument):
    def __init__(self, rt: Register, imm8: Immediate):
        self.rt = rt
        self.imm8 = imm8

    def __repr__(self):
        return f"<RtSpImm8: rt={self.rt.__repr__()},imm8={self.imm8.__repr__()}>"

    def to_binary(self):
        return (self.rt << 8) | self.imm8.value

    def binary_width(self):
        return 11


class SpImm7(Argument):
    def __init__(self, imm7: Immediate):
        self.imm7 = imm7

    def __repr__(self):
        return f"<SpImm7: imm7={self.imm7.__repr__()}>"

    def to_binary(self):
        return self.imm7.value

    def binary_width(self):
        return 7


class LabelArgument(Argument):
    def __init__(self, label):
        self.label = label

    def __repr__(self):
        return f"<LabelArgument: {self.label}>"

    def to_binary(self, labels, pc, is_b):
        if self.label not in labels:
            raise RuntimeError(f"label '{self.label}' not found")

        addr = labels[self.label] - pc - 3
        val = twos_comp(addr, 11 if is_b else 8)

        return val

    def binary_width(self):
        return 8


def parse_two_registers(args):
    registers = args.split(", ")
    if len(registers) != 2:
        raise RuntimeError("more than two registers")
    first_reg = Register.from_str(registers[0])
    second_reg = Register.from_str(registers[1])
    return TwoRegistersArgument(first_reg, second_reg)


def parse_rd_rn_rm(args):
    registers = args.split(", ")
    if len(registers) != 3:
        raise RuntimeError("more than three registers")
    first_reg = Register.from_str(registers[0])
    second_reg = Register.from_str(registers[1])
    third_reg = Register.from_str(registers[2])
    return RdRnRmArgument(first_reg, second_reg, third_reg)


def parse_rd_rm_imm5(args):
    args = args.split(", ")
    first_reg = Register.from_str(args[0])
    second_reg = Register.from_str(args[1])

    imm = Immediate.from_str(5, args[2])

    return RdRmImm5Argument(first_reg, second_reg, imm)


def parse_rd_rn_imm3(args):
    args = args.split(", ")
    first_reg = Register.from_str(args[0])
    second_reg = Register.from_str(args[1])

    imm = Immediate.from_str(3, args[2])

    return RdRnImm3Argument(first_reg, second_reg, imm)


def parse_rd_imm8(args):
    args = args.split(", ")
    reg = Register.from_str(args[0])

    imm = Immediate.from_str(8, args[1])

    return RdImm8Argument(reg, imm)


def parse_rt_sp_imm8(args):
    args = args.split(", ", 1)

    reg = Register.from_str(args[0])
    addr = parse_address(args[1], 8)
    addr[1].value //= 4

    if addr[0] != Register.SP:
        raise ValueError("invalid register")

    return RtSpImm8(reg, addr[1])


def parse_sp_imm7(args):
    args = args.split(", ", 1)
    reg = Register.from_str(args[0])
    if reg != Register.SP:
        raise ValueError("register should be sp")
    imm = Immediate.from_str(7, args[1])
    imm.value //= 4

    return SpImm7(imm)


def parse_label(content):
    return LabelArgument(content)


def parse_address(value, imm_size):
    if value[0] != "[" or value[-1] != "]":
        raise ValueError(f"invalid address {value}")

    res = value[1:-1].split(", ")
    reg = Register.from_str(res[0])
    imm = Immediate(imm_size, 0)

    if len(res) > 1:
        imm = Immediate.from_str(imm_size, res[1])

    return (reg, imm)


args_parser = {
    # Two registers
    Instruction.EORS: parse_two_registers,
    Instruction.ANDS: parse_two_registers,
    Instruction.LSLS2: parse_two_registers,
    Instruction.LSRS2: parse_two_registers,
    Instruction.ASRS2: parse_two_registers,
    Instruction.ADCS: parse_two_registers,
    Instruction.SBCS: parse_two_registers,
    Instruction.RORS: parse_two_registers,
    Instruction.TST: parse_two_registers,
    Instruction.RSBS: parse_two_registers,
    Instruction.CMP: parse_two_registers,
    Instruction.CMN: parse_two_registers,
    Instruction.ORRS: parse_two_registers,
    Instruction.BICS: parse_two_registers,
    Instruction.MVNS: parse_two_registers,
    # Rd, Rn, Rm
    Instruction.ADDS: parse_rd_rn_rm,
    Instruction.SUBS: parse_rd_rn_rm,
    # Label
    Instruction.BEQ: parse_label,
    Instruction.BNE: parse_label,
    Instruction.BCS: parse_label,
    Instruction.BCC: parse_label,
    Instruction.BMI: parse_label,
    Instruction.BPL: parse_label,
    Instruction.BVS: parse_label,
    Instruction.BVC: parse_label,
    Instruction.BHI: parse_label,
    Instruction.BLS: parse_label,
    Instruction.BGE: parse_label,
    Instruction.BLT: parse_label,
    Instruction.BGT: parse_label,
    Instruction.BLE: parse_label,
    Instruction.BAL: parse_label,
    Instruction.B: parse_label,
    # Rd, Rm, imm5
    Instruction.LSLS: parse_rd_rm_imm5,
    Instruction.LSRS: parse_rd_rm_imm5,
    Instruction.ASRS: parse_rd_rm_imm5,
    # Rd, Rn, imm3
    Instruction.ADDS2: parse_rd_rn_imm3,
    Instruction.SUBS2: parse_rd_rn_imm3,
    # Rd, imm8
    Instruction.ADDS3: parse_rd_imm8,
    Instruction.SUBS3: parse_rd_imm8,
    Instruction.MOVS: parse_rd_imm8,
    Instruction.CMP2: parse_rd_imm8,
    # Rt, sp, imm8
    Instruction.LDR: parse_rt_sp_imm8,
    Instruction.STR: parse_rt_sp_imm8,
    # sp, imm7
    Instruction.ADDSP: parse_sp_imm7,
    Instruction.SUBSP: parse_sp_imm7,
}


def parse(input_assembly):
    labels = {}

    lines = input_assembly.splitlines()
    lines = filter(None, lines)
    instructions = []
    pc = 0

    for line in lines:
        parsed = parse_line(line, labels, pc)
        if parsed is not None:
            instructions.append(parsed)
            pc += 1

    print(labels)

    return (instructions, labels)


def parse_line(content, labels, pc):
    line = content.strip()

    if ":" in line:
        labels[line[:-1]] = pc
        return

    (instr_str, args_str) = line.split(" ", 1)
    instr = Instruction.from_str(instr_str, args_str)

    instr_parser = args_parser[instr]
    args = instr_parser(args_str)

    return (instr, args)


def export_instruction(instr, labels, pc):
    opcode = instr[0].to_binary()
    if isinstance(instr[1], LabelArgument):
        args = instr[1].to_binary(labels, pc)
    else:
        args = instr[1].to_binary()

    if instr[0] == Instruction.B:
        width = 11
    else:
        width = instr[1].binary_width()

    return (opcode << width) | args


def export(instructions, labels):
    binarys = [export_instruction(instr, labels, pc)
               for (pc, instr) in enumerate(instructions)]

    binarys = ['{0:04x}'.format(val) for val in binarys]
    return "v2.0 raw\n" + " ".join(binarys)


f = open(sys.argv[1], "r")
content = f.read()

(instructions, labels) = parse(content)

print(instructions)
print(labels)

output = export(instructions, labels)

print(output)
