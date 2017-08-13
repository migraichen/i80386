------------------------------------------------------------------------------
--                                                                          --
-- Intel 80386 VHDL model                                                   --
-- Copyright (C) Convergent, Inc. 1988                                      --
--                                                                          --
--               File: i80386.vhd                                           --
--           Revision: E0.1                                                 --
--       Date Created: 6-12-1988                                            --
--             Author: Mark Dakur                                           --
--           Function: This VHDL model emulates the Intel 80386 32-bit CPU  --
--                     to the instruction and bus timing level.             --
--           Generics: Debug 1=Enable Reporting of Model Status.            --
--                           0=None (Default)                               --
--                     Inst                                                 --
--                     Performance                                          --
--                     Speed                                                --
--   Target Simulator: ViewSim                                              --
--                                                                          --
-- Reference Material: Intel Data Book, 80386-20, Oct., 1987                --
--                     Intel 80386 Programmers Reference, 1986              --
--                     80386 Technical Reference, Edmund Strauss, 1987      --
--                                                                          --
--       Verification: No                                                   --
--         Validation: No                                                   --
--      Certification: No                                                   --
--                                                                          --
-- Behavioral models have two main parts:  a package declaration and its    --
-- corresponding package body, and an entity declaration and its            --
-- corresponding architecture body.  The package declaration and            --
-- package body define subprograms used by the behavioral model;            --
-- the entity declaration and architecture body define the behavior         --
-- of the model.                                                            --
-- This file contains the entity declaration and architecture.              --
------------------------------------------------------------------------------
------------------------------------------------------------------------------
--                              Specification                               --
--                                                                          --
-- 1.0 Introduction                                                         --
-- 2.0 Description                                                          --
--                                                                          --
-- The i80386 consists of 6 functional units defined as follows:            --
--                                                                          --
-- 1) Bus Interface Unit {BIunit}                                           --
--    Accepts internal requests for code fetches from the CPunit and        --   
--    data transfers from the Eunit and prioritizes the requests.           --
--    It is the interface to the external pins (ports) of the package.      --
--                                                                          --
-- 2) Code Prefetch Unit {CPunit}                                           --
--    Performs the program look ahead function.  When the BIunit is not     --
--    performing bus cycles to execute an instruction, it uses the BIunit   --
--    to to fetch sequentially along the instruction byte stream.  These    --
--    prefetched instructions are stored in the 16-byte Code Queue to       --
--    await processing by the IDunit.                                       --
--                                                                          --
-- 3) Instruction Decode Unit {IDunit}                                      --
--    a) Instructions Supported:                                            --
--      1)  nop                                                             --
--      2)  mov eax,"immediate 32 bit data"                                 --
--      3)  mov ebx,"immediate 32 bit data"                                 --
--      4)  mov eax,[ebx]                                                   --
--      5)  mov [ebx],eax                                                   --
--      6)  in al,"byte address"                                            --
--      7)  out "byte address",al                                           --
--      8)  inc eax                                                         --
--      9)  inc ebx                                                         --
--      10) jmp "label" (relative nears and shorts)                         --
--                                                                          --
-- 4) Execution Unit {Eunit}                                                --
--    a) Control Unit {Cunit}                                               --
--    b) Data Unit {Dunit}                                                  --
--    c) Protection Test Unit {PTunit}                                      --
--                                                                          --
-- 5) Segmentation Unit {Sunit}                                             --
--                                                                          --
-- 6) Paging Unit {Punit}                                                   --
--    a) Page Translator Unit {PTunit}                                      --
--       i) Translation Lookaside Buffer {TLB}                              --
--          a) Page Directory                                               --
--          b) Page Table                                                   --
------------------------------------------------------------------------------
------------------------------------------------------------------------------
--                         Revision History                                 --
--                                                                          --
-- Revision                                                                 --
-- Level    Date    Engineer        Description                             --
-- -------- ------- --------------- --------------------------------------- --
-- E0.1     6-12-88 Dakur       First Release                               --
------------------------------------------------------------------------------
------------------------------------------------------------------------------
--
-- Entity declaration for i80386:
--
-- The following entity declaration begins the definition of the
-- behavioral model of the i80386.  It declares the model's name
-- and its IO signals, or ports.  This declaration defines the
-- model's interface with enclosing designs; it defines the part
-- of the model that is externally visible.  Following this
-- entity declaration is its corresponding architecture body;
-- the architecture body defines the behavior of the model.
--
-----------------------------------------------------------------------
PACKAGE i80386 is
FUNCTION tohex (CONSTANT value, Bytes: IN INTEGER) RETURN integer;

END i80386;

PACKAGE BODY i80386 is
FUNCTION tohex (CONSTANT value, Bytes: IN INTEGER) RETURN integer IS
    VARIABLE dWord: vlbit_1d(31 downto 0);
    VARIABLE Byte:  vlbit_1d(31 downto 0);
    VARIABLE Count: INTEGER;
BEGIN                   
    Count := 1;
    dWord := vlbit_vector(value);
    Convert: WHILE Count <= Bytes LOOP
        CASE integer(Bytes) is
            WHEN 4 =>
                CASE Count is
                    WHEN 1 =>
                        Byte := X"000000" & dWord(31 downto 24);
                    WHEN 2 =>
                        Byte := X"000000" & dWord(23 downto 16);
                    WHEN 3 =>
                        Byte := X"000000" & dWord(15 downto 8);
                    WHEN 4 =>
                        Byte := X"000000" & dWord(7 downto 0);
                    WHEN OTHERS => NULL;
                END CASE;
            WHEN 2 =>
                CASE Count is
                    WHEN 1 =>
                        Byte := X"000000" & dWord(15 downto 8);
                    WHEN 2 =>
                        Byte := X"000000" & dWord(7 downto 0);
                    WHEN OTHERS => NULL;
                END CASE;
            WHEN 1 =>
                Byte := X"000000" & dWord(7 downto 0);
            WHEN OTHERS => NULL;
        END CASE;
        Count := Count + 1;
        CASE integer(Byte(7 downto 4)) is
            WHEN 15 =>
                put("F");
            WHEN 14 =>
                put("E");
            WHEN 13 =>
                put("D");
            WHEN 12 =>
                put("C");
            WHEN 11 =>
                put("B");
            WHEN 10 =>
                put("A");
            WHEN 9 =>
                put("9");
            WHEN 8 =>
                put("8");
            WHEN 7 =>
                put("7");
            WHEN 6 =>
                put("6");
            WHEN 5 =>
                put("5");
            WHEN 4 =>
                put("4");
            WHEN 3 =>
                put("3");
            WHEN 2 =>
                put("2");
            WHEN 1 =>
                put("1");
            WHEN 0 =>
                put("0");
            WHEN OTHERS => put("X");
        END CASE;
        CASE integer(Byte(3 downto 0)) is
            WHEN 15 =>
                put("F");
            WHEN 14 =>
                put("E");
            WHEN 13 =>
                put("D");
            WHEN 12 =>
                put("C");
            WHEN 11 =>
                put("B");
            WHEN 10 =>
                put("A");
            WHEN 9 =>
                put("9");
            WHEN 8 =>
                put("8");
            WHEN 7 =>
                put("7");
            WHEN 6 =>
                put("6");
            WHEN 5 =>
                put("5");
            WHEN 4 =>
                put("4");
            WHEN 3 =>
                put("3");
            WHEN 2 =>
                put("2");
            WHEN 1 =>
                put("1");
            WHEN 0 =>
                put("0");
            WHEN OTHERS => put("X");
        END CASE;
    END LOOP Convert;
    put("h");
    RETURN 1;
END tohex;

END i80386;
USE work.i80386.tohex;
entity i80386 is

    GENERIC (CONSTANT Debug:        BOOLEAN := FALSE;
             CONSTANT Inst:         BOOLEAN := FALSE;
             CONSTANT Performance:  INTEGER := 1;
             CONSTANT Speed:        INTEGER := 32);

-- USE: Pass a value to the above generics from attributes attached to the 80386 symbol
--      on the schematic.

-- Description: Debug; A value of integer 1 (one) means that the model will output
--              status information as simulation progresses.  The default if no attribute exists is
--              FALSE, or no status reported.

--				Inst; A value of interger 1 (one) means that the model will output
--				instructions.  The Debug generic overides this one.

--              Performance; 0=min, 1=typ, 2=max

--              Speed; Processor speed choices, values are: 0=16MHz, 1=20MHz, 2=25MHZ, 3=30MHz

    port    (BE_n:                  out vlbit_1d(3 downto 0) := B"0000";
            Address:                out vlbit_1d(31 downto 2) := B"111111111111111111111111111111";
            W_R_n:                  out vlbit := '0';
            D_C_n:                  out vlbit := '1';
            M_IO_n:                 out vlbit := '0';
            LOCK_n, ADS_n:          out vlbit := '1';
            HLDA:                   out vlbit := '0';
            Data:                   inout vlbit_1d(31 downto 0) := X"ZZZZZZZZ";
            CLK2:                   in vlbit := '0';
            NA_n, BS16_n:           in vlbit := '1';
            READY_n, HOLD, PERQ:    in vlbit := '0';
            BUSY_n, ERROR_n:        in vlbit := '1';
            INTR:                   in vlbit := '0';
            NMI, RESET:             in vlbit := '0');

-- THE ORDER OF THE PORTS IS IMPORTANT FOR COMPATIBILITY WITH THE "PINORDER"
-- ATTRIBUTE ON THE SYMBOL FOR THIS MODEL.

end i80386;

-----------------------------------------------------------------------
-----------------------------------------------------------------------
--
-- Architecture Body of i80386:
--
-- The following architecture body defines the behavior of the i80386
-- model.  It consists of a set of process statements and other
-- concurrent statements.  These statements are all invoked when
-- simulation begins, and continue to execute concurrently throughout
-- simulation.  The statements communicate via the internal signals
-- declared at the top of the architecture body. Each statement either
-- checks the validity of input signals, or modifies the values of
-- output signals or internal signals in response to changes on input
-- signals or internal signals.
--
-----------------------------------------------------------------------

architecture behavior of i80386 is

-- Internal Signals
-- These information paths allow for communication between Concurent
-- Process Blocks within the model.  All signals that are defined here have
-- global visibility.  Signals, variables and constants defined within process
-- blocks have local visibility within that process ONLY.

    SIGNAL CLK:             vlbit                   := '1'; -- 80386 internal clock=CLK2 / 2

    SIGNAL StateNA:         vlbit                   := '1';
    SIGNAL StateBS16:       vlbit                   := '1';
    SIGNAL RequestPending:  vlbit                   := '1';
    CONSTANT Pending:       vlbit                   := '1';
    CONSTANT NotPending:    vlbit                   := '0';
    SIGNAL NonAligned:      vlbit                   := '0';
    SIGNAL ReadRequest:     vlbit                   := '1';
    SIGNAL MemoryFetch:     vlbit                   := '1';
    SIGNAL CodeFetch:       vlbit                   := '1';
    SIGNAL ByteEnable:      vlbit_1d(3 downto 0)    := X"0";
    SIGNAL DataWidth:       vlbit_1d(31 downto 0)   := X"00000002";
    CONSTANT WidthByte:     INTEGER                 := 0; -- Byte
    CONSTANT WidthWord:     INTEGER                 := 1; -- Word  (2 bytes)
    CONSTANT WidthDword:    INTEGER                 := 2; -- Dword (4 bytes)
    SIGNAL dWord:           vlbit_1d(31 downto 0)   := X"00000000";

    SIGNAL State:           vlbit_1d(31 downto 0) := X"00000000";   -- State Register, Initialized to StateTi
    CONSTANT StateTi:       INTEGER := 0;   -- Reset State
    CONSTANT StateT1:       INTEGER := 1;   -- First state of a non-pipelined bus cycle
    CONSTANT StateT2:       INTEGER := 2;   -- State where NA_n is false (non-pipelined)
    CONSTANT StateT1P:      INTEGER := 3;   -- First state of a pipelined bus cycle
    CONSTANT StateTh:       INTEGER := 4;   -- Hold acknowledge state
    CONSTANT StateT2P:      INTEGER := 5;   -- Subsequent state of a pipelined bus cycle
    CONSTANT StateT2I:      INTEGER := 6;   -- Subsequent state of a potential pipelined
                                            -- bus cycle.
    -- The constants are indexes into the vector State where each constant represents 1 bit of the vector.

-- Internal User Registers
--- General Purpose Data and Address
    SIGNAL EAX: vlbit_1d(31 DOWNTO 0);
    SIGNAL EDX: vlbit_1d(31 DOWNTO 0);
    SIGNAL ECX: vlbit_1d(31 DOWNTO 0);
    SIGNAL EBX: vlbit_1d(31 DOWNTO 0);
    SIGNAL EBP: vlbit_1d(31 DOWNTO 0);
    SIGNAL ESI: vlbit_1d(31 DOWNTO 0);
    SIGNAL EDI: vlbit_1d(31 DOWNTO 0);
    SIGNAL ESP: vlbit_1d(31 DOWNTO 0);
-- NOTE: Create a proceedure that can be called with the appropriate mnemonic
-- to access the appropriate register.  Futher work must be done to implement
-- the 16-bit and 8-bit versions of these registers.

--- Segment Selectors
    SIGNAL CS:  vlbit_1d(15 DOWNTO 0);  -- Code Segment
    SIGNAL SS:  vlbit_1d(15 DOWNTO 0);  -- Stack Segment
    SIGNAL DS:  vlbit_1d(15 DOWNTO 0);  -- Data Segment Module A
    SIGNAL ES:  vlbit_1d(15 DOWNTO 0);  -- Data Segment Structure 1
    SIGNAL FSS: vlbit_1d(15 DOWNTO 0);  -- Data Segment Structure 2
    SIGNAL GS:  vlbit_1d(15 DOWNTO 0);  -- Data Segment Structure 3

--- Segment Descripters
--- These register are associated with each Segment Selector Register and are
--- not visible to the programmer.
--- Instruction Pointer and Flags
    SIGNAL rEIP:        vlbit_1d(31 downto 0) := X"FFFFFFF0";
-- Must create a proceedure to access by mnemonic the IP within the EIP register.
    SIGNAL rEFLAGS:     vlbit_1d(31 downto 0) := B"XXXXXXXXXXXXXXXX0XXXXXXXXX0X0X1X";
    CONSTANT VM:        INTEGER := 0;
    CONSTANT RF:        INTEGER := 0;
    CONSTANT NT:        INTEGER := 0;
    CONSTANT IOPL:      INTEGER := 0;
    CONSTANT xOF:       INTEGER := 0;
    CONSTANT DF:        INTEGER := 0;
    CONSTANT xIF:       INTEGER := 0;
    CONSTANT TF:        INTEGER := 0;
    CONSTANT SF:        INTEGER := 0;
    CONSTANT ZF:        INTEGER := 0;
    CONSTANT AF:        INTEGER := 4;
    CONSTANT PF:        INTEGER := 2;
    CONSTANT CF:        INTEGER := 0;

--- Machine Control
    SIGNAL rCR0:        vlbit_1d(31 downto 0) := X"00000000";
    SIGNAL rCR1:        vlbit_1d(31 downto 0) := X"00000000";
    SIGNAL rCR2:        vlbit_1d(31 downto 0) := X"00000000";
    -- Page Directory Base Register
    SIGNAL rCR3:        vlbit_1d(31 downto 0) := X"00000000";

--- System Address (Memory Mapping Management)
    -- Global Descripter Table Pointer
    SIGNAL rGDTbase:        vlbit_1d(31 downto 0) := X"00000000";
    SIGNAL rGDTlimit:       vlbit_1d(15 downto 0) := X"0000";
    SIGNAL rGDTselector:    vlbit_1d(15 downto 0) := X"0000";
    -- Local Descripter Table Pointer
    SIGNAL rLDTbase:        vlbit_1d(31 downto 0) := X"00000000";
    SIGNAL rLDTlimit:       vlbit_1d(15 downto 0) := X"0000";
    SIGNAL rLDTselector:    vlbit_1d(15 downto 0) := X"0000";
    -- Interrupt Descripter Table Pointer
    SIGNAL rIDTbase:        vlbit_1d(31 downto 0) := X"00000000";
    SIGNAL rIDTlimit:       vlbit_1d(15 downto 0) := X"0000";
    SIGNAL rIDTselector:    vlbit_1d(15 downto 0) := X"0000";
    -- Task State Segment Descripter Table Pointer
    SIGNAL rTSSbase:        vlbit_1d(31 downto 0) := X"00000000";
    SIGNAL rTSSlimit:       vlbit_1d(15 downto 0) := X"0000";
    SIGNAL rTSSselector:    vlbit_1d(15 downto 0) := X"0000";
    -- Page Table Register Files
--  SIGNAL rfPageDir:       vlbit_2d(0 to 1024,31 downto 0);
--  SIGNAL rfPageTable:     vlbit_2d(0 to 1024,31 downto 0);
--- Debug
--- Test

-- 80386 Instruction Set (Supported by this model)

--- Instruction Prefixes
        CONSTANT REP:               INTEGER := 16#F3#;
        CONSTANT REPNE:             INTEGER := 16#F2#;
        CONSTANT LOCK:              INTEGER := 16#F0#;

--- Segment Override Prefixes
        CONSTANT CSsop:             INTEGER := 16#2E#;
        CONSTANT SSsop:             INTEGER := 16#36#;
        CONSTANT DSsop:             INTEGER := 16#3E#;
        CONSTANT ESsop:             INTEGER := 16#26#;
        CONSTANT FSsop:             INTEGER := 16#64#;
        CONSTANT GSsop:             INTEGER := 16#65#;
        CONSTANT OPsop:             INTEGER := 16#66#;
        CONSTANT ADsop:             INTEGER := 16#67#;

--- Data Transfer
        CONSTANT MOV_al_b:          INTEGER := 16#B0#;
        CONSTANT MOV_eax_dw:        INTEGER := 16#B8#; -- mov eax,0000A5A5h
        CONSTANT MOV_ebx_dw:        INTEGER := 16#BB#; -- mov ebx,0FFFFFFF0h
        CONSTANT MOV_ebx_eax:       INTEGER := 16#89#; -- mov [ebx],eax {89,03}
        CONSTANT MOV_eax_ebx:       INTEGER := 16#8B#; -- mov eax,[ebx] {8B,03}
        CONSTANT IN_al:             INTEGER := 16#E4#;
        CONSTANT OUT_al:            INTEGER := 16#E6#;
--- Arithmetic
        CONSTANT ADD_al_b:          INTEGER := 16#04#;
        CONSTANT ADD_ax_w:          INTEGER := 16#05#;
--- Shift/Rotate
        CONSTANT ROL_eax_b:         INTEGER := 16#D1#; -- rol eax,1 {D1,C0}
        CONSTANT ROL_al_1:          INTEGER := 16#D0#;
        CONSTANT ROL_al_n:          INTEGER := 16#C0#;
--- String Manipulation
        CONSTANT INC_eax:           INTEGER := 16#40#;
        CONSTANT INC_ebx:           INTEGER := 16#43#;
--- Bit Manipulation
--- Control Transfer
        CONSTANT JMP_rel_short:     INTEGER := 16#EB#;
        CONSTANT JMP_rel_near:      INTEGER := 16#E9#;
        CONSTANT JMP_intseg_immed:  INTEGER := 16#EA#;
--- High Level Language Support
--- Operating System Support
--- Processor Control
        CONSTANT HLT:               INTEGER := 16#F4#;
        CONSTANT WAITx:             INTEGER := 16#9B#;
        CONSTANT NOP:               INTEGER := 16#90#;

    BEGIN

-- Begin Fault Detection Section
    Faults: PROCESS
    BEGIN
        WAIT UNTIL now > 1;
        assert not bitunknown(CLK2)
            report "Clock {i}: CLK2 (pin F12) is undefined"
            severity FAILURE;
        assert not bitunknown(READY_n)
            report "Control {i}: READY (pin G13) is undefined"
            severity FAILURE;
    END PROCESS Faults;
-- End Fault Detection Section

-- Begin Behavioral Blocks
    -- Port Signals Status Reports Begin
    CLK2status: PROCESS     -- Function:    The first time (after the loading the network)
                            --              the simulation is run, this process will report
                            --              status of the 80386's CLK2 input from the network.

        VARIABLE StartTime:     INTEGER;
        VARIABLE Pwidth:        INTEGER;
        VARIABLE freq:          INTEGER;
    BEGIN
        WAIT UNTIL prising(CLK2);
        StartTime := now;
        WAIT UNTIL prising(CLK2);
        Pwidth := (now - StartTime);
        freq := 10000000 / Pwidth;
        put("CLK2 Pulse Width is=",Pwidth);
        putline(" in 10ths of nS");
        put("CLK2 Frequency is=",freq);
        putline("kHZ");
        WAIT;
    end PROCESS CLK2status;
    -- Port Signals Status Reports End

    -- Internal Control Logic Processes Begin
    GenCLK: PROCESS
    begin
        -- CLK is the 80386's internal clock an is 1/2 of CLK2
        wait until prising(CLK2);
        CLK <= not CLK;
    end PROCESS GenCLK;

    Initialize: PROCESS
    BEGIN
        EAX <= X"00000000";
        rEFLAGS <= X"00000002";
        rEIP <= X"FFFFFFF0";
        rIDTbase <= X"00000000";
        rIDTlimit <= X"03FF";
        State <= vlbit_vector(StateTi);
        IF Debug THEN
            putline("DEBUG: State=RESET");
        END IF;
        WAIT UNTIL pfalling(RESET); -- De-assert the drivers
        IF Debug THEN
            putline("DEBUG: 80386 was successfully Reset.");
        END IF;
        EAX <= X"ZZZZZZZZ";
        rEFLAGS <= X"ZZZZZZZZ";
        rEIP <= X"ZZZZZZZZ";
        rIDTbase <= X"ZZZZZZZZ";
        rIDTlimit <= X"ZZZZ";
        State <= X"ZZZZZZZZ";
        RequestPending <= 'Z';
        WAIT UNTIL prising(RESET);
    end PROCESS Initialize;

    TstateMachine: PROCESS
    VARIABLE nState:    vlbit_1d(31 downto 0) := X"00000000";
    BEGIN
        WAIT UNTIL pfalling(CLK);
        CASE integer(State) is
            WHEN StateTi =>
                IF Debug THEN
                    put("DEBUG: 80386 is in State Ti");
                END IF;
                IF RESET = '0' and RequestPending = Pending THEN
                    nState := vlbit_vector(StateT1);
                    IF Debug THEN
                        putline(", Moving to StateT1");
                    END IF;
                ELSIF RESET = '0' and HOLD = '1' THEN
                    nState := vlbit_vector(StateTh);
                    IF Debug THEN
                        putline(", Moving to StateTh");
                    END IF;
                ELSE
                    nState := vlbit_vector(StateTi);
                    IF Debug THEN
                        IF RESET = '1' THEN
                            putline(", Due to RESET = Asserted");
                        ELSE
                            putline(", Due to NO Requests Pending");
                        END IF;
                    END IF;
                END IF;

            WHEN StateT1 =>
                IF Debug THEN
                    putline("DEBUG: 80386 is in State T1, Moving to StateT2");
                END IF;
                nState := vlbit_vector(StateT2);

            WHEN StateT2 =>
                IF Debug THEN
                    putline("DEBUG: 80386 is in State T2");
                END IF;
                IF READY_n = '0' and HOLD ='0' and RequestPending = Pending THEN
                    nState := vlbit_vector(StateT1);
                ELSIF READY_N = '1' and NA_n = '1' THEN
                    NULL;
                ELSIF (RequestPending = Pending or HOLD = '1') and (READY_N = '1' and NA_n = '0') THEN
                    nState := vlbit_vector(StateT2I);
                ELSIF RequestPending = Pending and HOLD = '0' and READY_N = '1' and NA_n = '0' THEN
                    nState := vlbit_vector(StateT2P);
                ELSIF RequestPending = NotPending and HOLD = '0' and READY_N = '0' THEN
                    nState := vlbit_vector(StateTi);
                ELSIF HOLD = '1' and READY_N = '1' THEN
                    nState := vlbit_vector(StateTh);
                END IF;

            WHEN StateT1P =>
                IF Debug THEN
                    putline("DEBUG: 80386 is in State T1P");
                END IF;
                IF NA_n = '0' and HOLD = '0' and RequestPending = Pending THEN
                    nState := vlbit_vector(StateT2P);
                ELSIF NA_n = '0' and (HOLD = '1' or RequestPending = NotPending) THEN
                    nState := vlbit_vector(StateT2I);
                ELSIF NA_n = '1' THEN
                    nState := vlbit_vector(StateT2);
                END IF;

            WHEN StateTh =>
                IF Debug THEN
                    putline("DEBUG: 80386 is in State Th");
                END IF;
                IF HOLD = '1' THEN
                    NULL;
                ELSIF HOLD = '0' and RequestPending = Pending THEN
                    nState := vlbit_vector(StateT1);
                ELSIF HOLD = '0' and RequestPending = NotPending THEN
                    nState := vlbit_vector(StateTi);
                END IF;

            WHEN StateT2P =>
                IF Debug THEN
                    putline("DEBUG: 80386 is in State T2P");
                END IF;
                IF READY_n = '0' THEN
                    nState := vlbit_vector(StateT1P);
                END IF;

            WHEN StateT2I =>
                IF Debug THEN
                    putline("DEBUG: 80386 is in State T2I");
                END IF;
                IF READY_n = '1' and (RequestPending = NotPending or HOLD = '1') THEN
                    NULL;
                ELSIF READY_n = '1' and RequestPending = Pending and HOLD = '0' THEN
                    nState := vlbit_vector(StateT2P);
                ELSIF READY_n = '0' and HOLD = '1' THEN
                    nState := vlbit_vector(StateTh);
                ELSIF READY_n = '0' and HOLD = '0' and RequestPending = Pending THEN
                    nState := vlbit_vector(StateT1);
                ELSIF READY_n = '0' and HOLD = '0' and RequestPending = NotPending THEN
                    nState := vlbit_vector(StateTi);
                END IF;

            WHEN OTHERS => putline("MODEL ERROR: Invalid State=",State);
        END CASE;
        State <= nState;    -- This is where the next State is actually assigned.
    end PROCESS TstateMachine;
    -- Internal Control Logic Processes End

    -- Instruction Pre-Fetch, Decode and Execution Unit Begin
    InstDecode: PROCESS
    VARIABLE InstQueue:         vlbit_2d(1 to 16,7 downto 0);
    VARIABLE InstQueueRd_Addr:  INTEGER := 1;       -- Address used by the decode unit to read the queue.
    VARIABLE InstQueueWr_Addr:  INTEGER := 1;       -- Address used by the Pre-fetch unit to fill the queue.
    VARIABLE InstQueueLimit:    INTEGER := 16;      -- Maximum length of the Queue.
    VARIABLE InstAddrPointer:   INTEGER := 0;       -- Allways points to the current instruction's Address.
    VARIABLE PhyAddrPointer:    INTEGER := 0;       -- Allways points to the Systems Physical Address.
    VARIABLE Extended:          BOOLEAN := FALSE;   -- True if an extended op-code prefix was detected.
    VARIABLE More:              BOOLEAN := FALSE;   -- True if instruction was decoded correctly and
                                                    -- another read is needed for data.
    VARIABLE Flush:             BOOLEAN := FALSE;   -- True if JMP was executed, flush the Queue.
    VARIABLE First:             BOOLEAN := TRUE;    -- First time thru.
    VARIABLE Byte:              vlbit_1d(7 downto 0);
    VARIABLE lWord:             vlbit_1d(15 downto 0);
    VARIABLE uWord:             vlbit_1d(15 downto 0);
    VARIABLE fWord:             vlbit_1d(31 downto 0);
    VARIABLE Dummy:             INTEGER;

    BEGIN
        IF First THEN
            PhyAddrPointer := integer(rEIP);
            InstAddrPointer := PhyAddrPointer;
            First := FALSE;
        END IF;
        RequestPending <= Pending;
        ReadRequest <= Pending;
        MemoryFetch <= Pending;
        CodeFetch <= Pending;
        IF Debug THEN
            put("DEBUG: Fetching 1st Word @ Addr=");Dummy := tohex(PhyAddrPointer,4);putline("");
        END IF;
        WAIT UNTIL pfalling(READY_n);
        RequestPending <= NotPending;
        WAIT UNTIL pfalling(CLK);
        InstQueue(InstQueueWr_Addr) := Data(7 downto 0);
        InstQueueWr_Addr := InstQueueWr_Addr + 1;
        InstQueue(InstQueueWr_Addr) := Data(15 downto 8);
        InstQueueWr_Addr := InstQueueWr_Addr + 1;
        IF StateBS16 = '1' THEN  -- A dWord code fetch
            InstQueue(InstQueueWr_Addr) := Data(23 downto 16);
            InstQueueWr_Addr := InstQueueWr_Addr + 1;
            InstQueue(InstQueueWr_Addr) := Data(31 downto 24);
            InstQueueWr_Addr := InstQueueWr_Addr + 1;
            PhyAddrPointer := PhyAddrPointer + 4; -- Point to next dWord since BS16- = 1
        ELSE
            PhyAddrPointer := PhyAddrPointer + 2; -- Point to next word since BS16- = 0
            IF Debug THEN
                put("DEBUG: Fetching 2nd Word @ Addr=");Dummy := tohex(PhyAddrPointer,4);putline("");
            END IF;
            rEIP <= vlbit_vector(PhyAddrPointer);
            WAIT UNTIL prising(CLK);
            RequestPending <= Pending;
            WAIT UNTIL pfalling(READY_n);
            RequestPending <= NotPending;
            WAIT UNTIL pfalling(CLK);
            InstQueue(InstQueueWr_Addr) := Data(7 downto 0);
            InstQueueWr_Addr := InstQueueWr_Addr + 1;
            InstQueue(InstQueueWr_Addr) := Data(15 downto 8);
            InstQueueWr_Addr := InstQueueWr_Addr + 1;
            PhyAddrPointer := PhyAddrPointer + 2; -- Point to next word since BS16- = 0
        END IF;
        Decode: WHILE InstQueueRd_Addr < InstQueueWr_Addr LOOP
            IF DEBUG THEN
                putline("DEBUG: InstQueueRd_Addr=",InstQueueRd_Addr);
                putline("DEBUG: InstQueueWr_Addr=",InstQueueWr_Addr);
                putline("DEBUG: InstQueueLimit=",InstQueueLimit);
                put("DEBUG: InstAddrPointer=");Dummy := tohex(InstAddrPointer,4);putline("");
                put("DEBUG: PhyAddrPointer=");Dummy := tohex(PhyAddrPointer,4);putline("");
                putline("DEBUG: Extended=",Extended);
                putline("DEBUG: Flush=",Flush);
                putline("DEBUG: More=",More);
                put("DEBUG: InstQueue( 1)=");Dummy := tohex(integer(InstQueue(1)),1);putline("");
                put("DEBUG: InstQueue( 2)=");Dummy := tohex(integer(InstQueue(2)),1);putline("");
                put("DEBUG: InstQueue( 3)=");Dummy := tohex(integer(InstQueue(3)),1);putline("");
                put("DEBUG: InstQueue( 4)=");Dummy := tohex(integer(InstQueue(4)),1);putline("");
                put("DEBUG: InstQueue( 5)=");Dummy := tohex(integer(InstQueue(5)),1);putline("");
                put("DEBUG: InstQueue( 6)=");Dummy := tohex(integer(InstQueue(6)),1);putline("");
                put("DEBUG: InstQueue( 7)=");Dummy := tohex(integer(InstQueue(7)),1);putline("");
                put("DEBUG: InstQueue( 8)=");Dummy := tohex(integer(InstQueue(8)),1);putline("");
                put("DEBUG: InstQueue( 9)=");Dummy := tohex(integer(InstQueue(9)),1);putline("");
                put("DEBUG: InstQueue(10)=");Dummy := tohex(integer(InstQueue(10)),1);putline("");
                put("DEBUG: InstQueue(11)=");Dummy := tohex(integer(InstQueue(11)),1);putline("");
                put("DEBUG: InstQueue(12)=");Dummy := tohex(integer(InstQueue(12)),1);putline("");
                put("DEBUG: InstQueue(13)=");Dummy := tohex(integer(InstQueue(13)),1);putline("");
                put("DEBUG: InstQueue(14)=");Dummy := tohex(integer(InstQueue(14)),1);putline("");
                put("DEBUG: InstQueue(15)=");Dummy := tohex(integer(InstQueue(15)),1);putline("");
                put("DEBUG: InstQueue(16)=");Dummy := tohex(integer(InstQueue(16)),1);putline("");
            END IF;
            CASE integer(InstQueue(InstQueueRd_Addr)) is
                WHEN NOP =>
                    InstAddrPointer := InstAddrPointer + 1;
                    InstQueueRd_Addr := InstQueueRd_Addr + 1;
                    Flush := FALSE;
                    More := FALSE;
                    IF Debug OR Inst THEN
                        putline("DEBUG: Executing NOP");
                    END IF;
                WHEN OPsop =>
                    InstAddrPointer := InstAddrPointer + 1;
                    InstQueueRd_Addr := InstQueueRd_Addr + 1;
                    Extended := TRUE;
                    Flush := FALSE;
                    More := FALSE;
                    IF Debug OR Inst THEN
                        put("DEBUG: Extended Op-Code Read:");Dummy := tohex(OPsop,1);putline("");
                    END IF;
                WHEN JMP_rel_short =>
                    IF (InstQueueWr_Addr - InstQueueRd_Addr) >= 3 THEN
                        IF Debug OR Inst THEN
                            put("DEBUG: Executing JMP-Rel-Short from:");Dummy := tohex(InstAddrPointer,4);
                        END IF;
                        IF InstQueue(InstQueueRd_Addr+1,7) = '1' THEN -- Negative Offset
                            PhyAddrPointer := InstAddrPointer + 1 - (16#FF# - integer(extendum(InstQueue(InstQueueRd_Addr+1),32)));
                            InstAddrPointer := PhyAddrPointer;
                            IF Debug OR Inst THEN
                                put(" (-)To:");Dummy := tohex(PhyAddrPointer,4);putline("");
                            END IF;
                        ELSE -- Positive Offset
                            PhyAddrPointer := InstAddrPointer + 2 + integer(extendum(InstQueue(InstQueueRd_Addr+1),32));
                            InstAddrPointer := PhyAddrPointer;
                            IF Debug OR Inst THEN
                                put(" (+)To:");Dummy := tohex(PhyAddrPointer,4);putline("");
                            END IF;
                        END IF;
                        Flush := TRUE;
                        More := FALSE;
                    ELSE
                        Flush := FALSE;
                        More := TRUE;
                    END IF;
                WHEN JMP_rel_near =>
                    IF (InstQueueWr_Addr - InstQueueRd_Addr) >= 5 THEN
                        IF Debug OR Inst THEN
                            put("DEBUG: Executing JMP-Rel-Near from:");Dummy := tohex(InstAddrPointer,4);
                        END IF;
                        PhyAddrPointer := InstAddrPointer + 5 + integer(extendum(InstQueue(InstQueueRd_Addr+1),32));
                        InstAddrPointer := PhyAddrPointer;
                        IF Debug OR Inst THEN
                            put(" To:");Dummy := tohex(PhyAddrPointer,4);putline("");
                        END IF;
                        Flush := TRUE;
                        More := FALSE;
                    ELSE
                        Flush := FALSE;
                        More := TRUE;
                    END IF;
                WHEN JMP_intseg_immed =>
-- To be Implemented (mad/8-23-1988)
                    IF Debug OR Inst THEN
                        putline("DEBUG: {TBD} Executing JMP-IntSeg-Immed from:",InstAddrPointer);
                    END IF;
                    InstAddrPointer := InstAddrPointer + 1;
                    InstQueueRd_Addr := InstQueueRd_Addr + 1;
                    Flush := FALSE;
                    More := FALSE;
                WHEN MOV_al_b =>
-- To be Implemented (mad/8-23-1988)
                    IF Debug OR Inst THEN
                        putline("DEBUG: {TBD} Executing MOV-al<-byte");
                    END IF;
                    InstAddrPointer := InstAddrPointer + 1;
                    InstQueueRd_Addr := InstQueueRd_Addr + 1;
                    Flush := FALSE;
                    More := FALSE;
                WHEN MOV_eax_dw =>
                    IF (InstQueueWr_Addr - InstQueueRd_Addr) >= 5 THEN
                        IF Debug OR Inst THEN
                            put("DEBUG: Executing MOV-eax<-dw");
                        END IF;
                        -- Note Word position is swaped
                        EAX <= InstQueue(InstQueueRd_Addr+4) & InstQueue(InstQueueRd_Addr+3)
                                     & InstQueue(InstQueueRd_Addr+2) & InstQueue(InstQueueRd_Addr+1);
                        WAIT FOR 1;
                        IF Debug OR Inst THEN
                            put(" of:");Dummy := tohex(integer(EAX),4);putline("");
                        END IF;
                        More := FALSE;
                        Flush := FALSE;
                        InstAddrPointer := InstAddrPointer + 5;
                        InstQueueRd_Addr := InstQueueRd_Addr + 5;
                    ELSE
                        Flush := FALSE;
                        More := TRUE;
                        IF Debug THEN
                            putline("DEBUG: Executing MOV-eax<-dw but ...");
                            putline("DEBUG: all of the immediate data is not in queue.");
                        END IF;
                    END IF;
                WHEN MOV_ebx_dw =>
                    IF (InstQueueWr_Addr - InstQueueRd_Addr) >= 5 THEN
                        IF Debug OR Inst THEN
                            put("DEBUG: Executing MOV-ebx<-dw");
                        END IF;
                        -- Note Word position is swaped
                        EBX <= InstQueue(InstQueueRd_Addr+4) & InstQueue(InstQueueRd_Addr+3)
                                     & InstQueue(InstQueueRd_Addr+2) & InstQueue(InstQueueRd_Addr+1);
                        WAIT FOR 1;
                        IF Debug OR Inst THEN
                            put(" of:");Dummy := tohex(integer(EBX),4);putline("");
                        END IF;
                        More := FALSE;
                        Flush := FALSE;
                        InstAddrPointer := InstAddrPointer + 5;
                        InstQueueRd_Addr := InstQueueRd_Addr + 5;
                    ELSE
                        Flush := FALSE;
                        More := TRUE;
                        IF Debug THEN
                            putline("DEBUG: Executing MOV-ebx<-dw but ...");
                            putline("DEBUG: all of the immediate data is not in queue.");
                        END IF;
                    END IF;
                WHEN MOV_eax_ebx =>  -- Read at [ebx] to eax register
                    IF (InstQueueWr_Addr - InstQueueRd_Addr) >= 2 THEN
                        IF Debug OR Inst THEN
                            put("DEBUG: Executing MOV-eax,[ebx]");
                        END IF;
                        IF Debug OR Inst THEN
                            put(" at address:");Dummy := tohex(integer(EBX),4);putline("");
                        END IF;
                        rEIP <= EBX;
                        RequestPending <= Pending;
                        ReadRequest <= Pending;
                        MemoryFetch <= Pending;
                        CodeFetch <= NotPending;
                        WAIT UNTIL pfalling(READY_n);
                        RequestPending <= NotPending;
                        WAIT UNTIL pfalling(CLK);
                        uWord := Data(15 downto 0);
                        IF StateBS16 = '1' THEN
                            lWord := Data(31 downto 16);
                        ELSE
                            rEIP <= vlbit_vector(integer(rEIP) + 2);
                            WAIT FOR 1;
                            IF Debug THEN
                                put("DEBUG: Reading Second Word at Addr=");Dummy := tohex(integer(rEIP),4);putline("");
                            END IF;
                            WAIT UNTIL prising(CLK);
                            RequestPending <= Pending;
                            WAIT UNTIL pfalling(READY_n);
                            RequestPending <= NotPending;
                            WAIT UNTIL pfalling(CLK);
                            lWord := Data(15 downto 0);
                        END IF;
                        EAX <= uWord & lWord;
                        WAIT FOR 1;
                        IF Debug OR Inst THEN
                            put("DEBUG: Data=");Dummy := tohex(integer(EAX),4);putline("");
                        END IF;
                        More := FALSE;
                        Flush := FALSE;
                        InstAddrPointer := InstAddrPointer + 2;
                        InstQueueRd_Addr := InstQueueRd_Addr + 2;
                    ELSE
                        Flush := FALSE;
                        More := TRUE;
                    END IF;
                WHEN MOV_ebx_eax =>  -- Write at [ebx] from eax register
                    IF (InstQueueWr_Addr - InstQueueRd_Addr) >= 2 THEN
                        IF Debug OR Inst THEN
                            put("DEBUG: Executing MOV-[ebx],eax");
                        END IF;
                        IF Debug OR Inst THEN
                            put(" at address:");Dummy := tohex(integer(EBX),4);putline("");
                        END IF;
                        rEIP <= EBX;
                        lWord := EAX(15 downto 0);
                        uWord := EAX(31 downto 16);
                        IF Debug OR Inst THEN
                            put("DEBUG: Data=");Dummy := tohex(integer(EAX),4);putline("");
                        END IF;
                        RequestPending <= Pending;
                        ReadRequest <= NotPending;
                        MemoryFetch <= Pending;
                        CodeFetch <= NotPending;
                        IF Debug THEN
                            put("DEBUG: Writing First Word at Addr=");Dummy := tohex(integer(EBX),4);putline("");
                        END IF;
                        WAIT UNTIL (integer(State) = StateT1 OR integer(State) = StateT1P);
                        WAIT UNTIL prising(CLK);
                        Data <= (uWord & lWord) after 480;
                        WAIT UNTIL pfalling(READY_n);
                        RequestPending <= NotPending;
                        WAIT UNTIL prising(CLK);
                        Data <= X"ZZZZZZZZ" after 480;
                        WAIT FOR 1;
                        IF StateBS16 = '0' THEN
                            IF Debug THEN
                                put("DEBUG: Writing Second Word at Addr=");Dummy := tohex(integer(EBX),4);putline("");
                            END IF;
                            rEIP <= vlbit_vector(integer(rEIP) + 2);
                            RequestPending <= Pending;
                            ReadRequest <= NotPending;
                            MemoryFetch <= Pending;
                            CodeFetch <= NotPending;
                            WAIT UNTIL (integer(State) = StateT1 OR integer(State) = StateT1P);
                            WAIT UNTIL prising(CLK);
                            Data <= (uWord & lWord) after 480;
                            WAIT UNTIL pfalling(READY_n);
                            RequestPending <= NotPending;
                            WAIT UNTIL prising(CLK);
                            Data <= X"ZZZZZZZZ" after 480;
                            WAIT FOR 1;
                        END IF;
                        More := FALSE;
                        Flush := FALSE;
                        InstAddrPointer := InstAddrPointer + 2;
                        InstQueueRd_Addr := InstQueueRd_Addr + 2;
                    ELSE
                        Flush := FALSE;
                        More := TRUE;
                    END IF;
                WHEN IN_al =>
                    IF (InstQueueWr_Addr - InstQueueRd_Addr) >= 2 THEN
                        IF Debug OR Inst THEN
                            put("DEBUG: Executing IN-al");
                        END IF;
                        rEIP <= extendum(InstQueue(InstQueueRd_Addr+1),32);
                        WAIT FOR 1;
                        IF Debug OR Inst THEN
                            put(" from:");Dummy := tohex(integer(rEIP),4);
                        END IF;
                        RequestPending <= Pending;
                        ReadRequest <= Pending;
                        MemoryFetch <= NotPending;
                        CodeFetch <= NotPending;
                        WAIT UNTIL pfalling(READY_n);
                        RequestPending <= NotPending;
                        WAIT UNTIL pfalling(CLK);
                        EAX(7 downto 0) <= Data(7 downto 0);
                        WAIT FOR 1;
                        IF Debug OR Inst THEN
                            put(" Data=");Dummy := tohex(integer(EAX(7 downto 0)),1);putline("");
                        END IF;
                        InstAddrPointer := InstAddrPointer + 2;
                        InstQueueRd_Addr := InstQueueRd_Addr + 2;
                        Flush := FALSE;
                        More := FALSE;
                    ELSE
                        Flush := FALSE;
                        More := TRUE;
                        IF Debug THEN
                            putline("DEBUG: Executing IN-al but ...");
                            putline("DEBUG: the immediate Address is not in queue.");
                        END IF;
                    END IF;
                WHEN OUT_al =>
                    IF (InstQueueWr_Addr - InstQueueRd_Addr) >= 2 THEN
                        IF Debug OR Inst THEN
                            put("DEBUG: Executing OUT-al");
                        END IF;
                        rEIP <= extendum(InstQueue(InstQueueRd_Addr+1),32);
                        WAIT FOR 1;
                        IF Debug OR Inst THEN
                            put(" to:");Dummy := tohex(integer(rEIP),4);
                        END IF;
                        RequestPending <= Pending;
                        ReadRequest <= NotPending;
                        MemoryFetch <= NotPending;
                        CodeFetch <= NotPending;
                        IF Debug OR Inst THEN
                            put(" Data=");Dummy := tohex(integer(EAX(7 downto 0)),1);putline("");
                        END IF;
                        WAIT UNTIL (integer(State) = StateT1 OR integer(State) = StateT1P);
                        WAIT UNTIL prising(CLK);
                        fWord := X"ZZZZZZ" & EAX(7 downto 0);
                        Data <= fWord after 480;
                        WAIT UNTIL pfalling(READY_n);
                        RequestPending <= NotPending;
                        WAIT UNTIL prising(CLK);
                        Data <= X"ZZZZZZZZ" after 480;
                        WAIT FOR 1;
                        InstAddrPointer := InstAddrPointer + 2;
                        InstQueueRd_Addr := InstQueueRd_Addr + 2;
                        Flush := FALSE;
                        More := FALSE;
                    ELSE
                        Flush := FALSE;
                        More := TRUE;
                        IF Debug THEN
                            putline("DEBUG: Executing OUT-al but ...");
                            putline("DEBUG: the immediate Address is not in queue.");
                        END IF;
                    END IF;
                WHEN ADD_al_b =>
-- To be Implemented (mad/8-23-1988)
                    IF Debug OR Inst THEN
                        putline("DEBUG: {TBD} Executing ADD-al to byte:");
                    END IF;
                    InstAddrPointer := InstAddrPointer + 1;
                    InstQueueRd_Addr := InstQueueRd_Addr + 1;
                    Flush := FALSE;
                    More := FALSE;
                WHEN ADD_ax_w =>
-- To be Implemented (mad/8-23-1988)
                    IF Debug OR Inst THEN
                        putline("DEBUG: {TBD} Executing ADD-ax to word:");
                    END IF;
                    InstAddrPointer := InstAddrPointer + 1;
                    InstQueueRd_Addr := InstQueueRd_Addr + 1;
                    Flush := FALSE;
                    More := FALSE;
                WHEN ROL_al_1 =>
-- To be Implemented (mad/8-23-1988)
                    IF Debug OR Inst THEN
                        putline("DEBUG: {TBD} Executing ROL-al left one bit");
                    END IF;
                    InstAddrPointer := InstAddrPointer + 2;
                    InstQueueRd_Addr := InstQueueRd_Addr + 2;
                    Flush := FALSE;
                    More := FALSE;
                WHEN ROL_al_n =>
-- To be Implemented (mad/8-23-1988)
                    IF Debug OR Inst THEN
                        putline("DEBUG: {TBD} Executing ROL-al by:");
                    END IF;
                    InstAddrPointer := InstAddrPointer + 2;
                    InstQueueRd_Addr := InstQueueRd_Addr + 2;
                    Flush := FALSE;
                    More := FALSE;
                WHEN INC_eax =>
                    EAX <= vlbit_vector(integer(EAX) + 1);
                    WAIT FOR 1;
                    IF Debug OR Inst THEN
                        put("DEBUG: Executing INC-eax by 1 to:");Dummy := tohex(integer(EAX),4);putline("");
                    END IF;
                    InstAddrPointer := InstAddrPointer + 1;
                    InstQueueRd_Addr := InstQueueRd_Addr + 1;
                    Flush := FALSE;
                    More := FALSE;
                WHEN INC_ebx =>
                    EBX <= vlbit_vector(integer(EBX) + 1);
                    WAIT FOR 1;
                    IF Debug OR Inst THEN
                        put("DEBUG: Executing INC-ebx by 1 to:");Dummy := tohex(integer(EBX),4);putline("");
                    END IF;
                    InstAddrPointer := InstAddrPointer + 1;
                    InstQueueRd_Addr := InstQueueRd_Addr + 1;
                    Flush := FALSE;
                    More := FALSE;
                WHEN OTHERS  =>
                    put("ERROR: Invalid Instruction=");Dummy := tohex(integer(InstQueue(InstQueueRd_Addr)),1);putline("");
                    InstAddrPointer := InstAddrPointer + 1;
                    InstQueueRd_Addr := InstQueueRd_Addr + 1;
                    Flush := FALSE;
                    More := FALSE;
            END CASE;
            EXIT WHEN ((InstQueueLimit - InstQueueRd_Addr) < 4) OR Flush OR More;
        END LOOP Decode;
        IF Flush THEN
            InstQueueRd_Addr := 1;
            InstQueueWr_Addr := 1;
            fWord := vlbit_vector(InstAddrPointer);
            IF fWord(0) = '1' THEN
                InstQueueRd_Addr := InstQueueRd_Addr + integer(extendum(fWord(1 downto 0),32));
            END IF;
            IF Debug THEN
                putline("DEBUG: Flushing Instruction Queue");
            END IF;
        END IF;
        IF (InstQueueLimit - InstQueueRd_Addr) < 3 THEN -- The queue is about to be bounded.
            -- This section implements the circular queue.
            IF Debug THEN
                putline("DEBUG: Instruction Queue Length Execeeded");
                putline("DEBUG: Implementing Circular Queue");
            END IF;
            InstQueueWr_Addr := 1;
            Circular: WHILE InstQueueRd_Addr <= InstQueueLimit LOOP
                InstQueue(InstQueueWr_Addr) := InstQueue(InstQueueRd_Addr);
                InstQueueRd_Addr := InstQueueRd_Addr + 1;
                InstQueueWr_Addr := InstQueueWr_Addr + 1;
            END LOOP Circular;
            InstQueueRd_Addr := 1;
        END IF;
        IF Debug THEN
            putline("DEBUG: Request Pending, filling Queue at:",InstQueueWr_Addr);
        END IF;
        rEIP <= vlbit_vector(PhyAddrPointer);
        WAIT UNTIL prising(CLK);
    end PROCESS InstDecode;
    -- Instruction Pre-Fetch, Decode and Execution Unit Begin
       
    -- ByteEnables Begin
    GenByteEnables: PROCESS (rEIP)
    BEGIN
        CASE integer(DataWidth) is
            WHEN WidthByte =>
                CASE integer(rEIP(1 downto 0)) is -- A[1:0]
                    WHEN 0 =>
                        ByteEnable <= B"1110";
                    WHEN 1 =>
                        ByteEnable <= B"1101";
                    WHEN 2 =>
                        ByteEnable <= B"1011";
                    WHEN 3 =>
                        ByteEnable <= B"0111";
                    WHEN OTHERS  => NULL;
                END CASE;
            WHEN WidthWord =>
                CASE integer(rEIP(1 downto 0)) is -- A[1:0]
                    WHEN 0 =>
                        ByteEnable <= B"1100";
                        NonAligned <= NotPending;
                    WHEN 1 =>
                        ByteEnable <= B"1001";
                        NonAligned <= NotPending;
                    WHEN 2 =>
                        ByteEnable <= B"0011";
                        NonAligned <= NotPending;
                    WHEN 3 =>
                        IF Debug THEN
                            putline("DEBUG: Non-Aligned Word");
                        END IF;
                        ByteEnable <= B"0111";
                        NonAligned <= Pending;
                    WHEN OTHERS  => NULL;
                END CASE;
            WHEN WidthDword =>
                CASE integer(rEIP(1 downto 0)) is -- A[1:0]
                    WHEN 0 =>
                        ByteEnable <= B"0000";
                        NonAligned <= NotPending;
                    WHEN 1 =>
                        IF Debug THEN
                            putline("DEBUG: Non-Aligned Dword");
                        END IF;
                        ByteEnable <= B"0001";
                        NonAligned <= Pending;
                    WHEN 2 =>
                        IF Debug THEN
                            putline("DEBUG: Non-Aligned Dword");
                        END IF;
                        NonAligned <= Pending;
                        ByteEnable <= B"0011";
                    WHEN 3 =>
                        IF Debug THEN
                            putline("DEBUG: Non-Aligned Dword");
                        END IF;
                        NonAligned <= Pending;
                        ByteEnable <= B"0111";
                    WHEN OTHERS  => NULL;
                END CASE;
            WHEN OTHERS  =>
                putline("MODEL ERROR: Data Path Width Fault: DataWidth");
                putline("MODEL ERROR: Width Selected was:",integer(DataWidth));
        END CASE;
    end PROCESS GenByteEnables;
    -- ByteEnables End

    -- Bus Interface Unit Begin
    GenBusIntf: PROCESS (State)
    BEGIN
        CASE integer(State) is
            WHEN StateT1 | StateT2P =>
                Address <= rEIP(31 downto 2) after 40ns;
                IF Debug THEN
--                  putline("DEBUG: Next Address=",rEIP);
                END IF;
                BE_n <= ByteEnable after 30ns;
                M_IO_n <= MemoryFetch;
                IF ReadRequest = Pending THEN
                    W_R_n <= '0' after 30ns;
                ELSE
                    W_R_n <= '1' after 30ns;
                END IF;
                IF CodeFetch = Pending THEN
                    D_C_n <= '0' after 30ns;
                ELSE
                    D_C_n <= '1' after 30ns;
                END IF;
            WHEN OTHERS  => NULL;
        END CASE;
        CASE integer(State) is
            WHEN StateT1  => ADS_n <= '0' after 25ns;
            WHEN StateT2  => ADS_n <= '1' after 25ns;
            WHEN StateT1P => ADS_n <= '1' after 25ns;
            WHEN StateT2P => ADS_n <= '0' after 25ns;
            WHEN OTHERS  => NULL;
        END CASE;
    end PROCESS GenBusIntf;

    BS16: PROCESS
    BEGIN
        WAIT UNTIL integer(State) = StateT2 OR integer(State) = StateT1P;
        WHILE integer(State) = StateT2 OR integer(State) = StateT1P LOOP
            WAIT UNTIL prising(CLK);
            StateBS16 <= BS16_n;
            IF BS16_n = '0' THEN
                DataWidth <= vlbit_vector(Widthword); -- WidthByte, WidthWord, WidthDword
            ELSE
                DataWidth <= vlbit_vector(WidthDword);
            END IF;
        END LOOP;
    end PROCESS BS16;

    NA: PROCESS
    BEGIN
        WAIT UNTIL integer(State) = StateT2 OR integer(State) = StateT1P;
        WAIT UNTIL prising(CLK);
        StateNA <= NA_n;
    end PROCESS NA;

    -- Bus Interface Unit End
-- End Behavioral Blocks

end behavior;
