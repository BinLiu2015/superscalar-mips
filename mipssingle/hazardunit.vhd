library IEEE; 
use IEEE.STD_LOGIC_1164.all; use IEEE.NUMERIC_STD_UNSIGNED.all;

entity hazardunit is 
	port(clk, reset: in STD_LOGIC;
			pc               : out  STD_LOGIC_VECTOR(31 downto 0);
			pc1              : in   STD_LOGIC_VECTOR(31 downto 0);
			instr1           : in   STD_LOGIC_VECTOR(31 downto 0);
			jump1            : in   STD_LOGIC;
			pcsrc1           : in   STD_LOGIC;
			writereg1        : in   STD_LOGIC;
			pcaddr1          : out  STD_LOGIC_VECTOR(31 downto 0);
			pc2              : in   STD_LOGIC_VECTOR(31 downto 0);
			instr2           : in   STD_LOGIC_VECTOR(31 downto 0);
			jump2            : in   STD_LOGIC;
			pcsrc2           : in   STD_LOGIC;
			writereg2        : in   STD_LOGIC;
			pcaddr2          : out  STD_LOGIC_VECTOR(31 downto 0);
			stall2           : buffer  STD_LOGIC;
			pc3              : in   STD_LOGIC_VECTOR(31 downto 0);
			instr3           : in   STD_LOGIC_VECTOR(31 downto 0);
			jump3            : in   STD_LOGIC;
			pcsrc3           : in   STD_LOGIC;
			writereg3        : in   STD_LOGIC;
			pcaddr3          : out  STD_LOGIC_VECTOR(31 downto 0);
			stall3           : out  STD_LOGIC);
end;

architecture test of hazardunit is
	component jumpstaller
		port(jump1, pcsrc1: in STD_LOGIC;
			jump2, pcsrc2: in STD_LOGIC;
			jump3, pcsrc3: in STD_LOGIC;
			stall2, stall3: out STD_LOGIC);
	end component;
	component memorystaller
        port(op1    : in  STD_LOGIC_VECTOR(5 downto 0);
            basereg1  : in  STD_LOGIC_VECTOR(4 downto 0);
            offset1   : in  STD_LOGIC_VECTOR(15 downto 0);
            op2       : in  STD_LOGIC_VECTOR(5 downto 0);
            basereg2  : in  STD_LOGIC_VECTOR(4 downto 0);
            offset2   : in  STD_LOGIC_VECTOR(15 downto 0);
            op3       : in  STD_LOGIC_VECTOR(5 downto 0);
            basereg3  : in  STD_LOGIC_VECTOR(4 downto 0);
            offset3   : in  STD_LOGIC_VECTOR(15 downto 0);
            stall2, stall3: out STD_LOGIC);      
  	end component;
	component regstaller
        port(writereg1    : in  STD_LOGIC;
            instr1        : in  STD_LOGIC;
            writereg2     : in  STD_LOGIC;
            instr2        : in  STD_LOGIC;
            writereg3     : in  STD_LOGIC;
            instr3        : in  STD_LOGIC;
            stall2, stall3: out STD_LOGIC);      
  	end component;
    component flopr generic(width :    integer);
		port(clk, reset               : in STD_LOGIC;
		d : in  STD_LOGIC_VECTOR(width-1 downto 0);
		q : out STD_LOGIC_VECTOR(width-1 downto 0));
  	end component;

	signal pc, pcnext, instr,
	readdata, result, srca: STD_LOGIC_VECTOR(31 downto 0);

	signal writereg: STD_LOGIC_VECTOR(4 downto 0);

	signal jump1: STD_LOGIC;
	signal pcsrc1: STD_LOGIC;
	signal regwrite: STD_LOGIC;
	signal jump_stall2, mem_stall2, reg_stall2: STD_LOGIC;
	signal jump_stall3, mem_stall3, reg_stall3: STD_LOGIC;
begin
    stall2 <= jump_stall2 or mem_stall2 or reg_stall2;
    stall3 <= jump_stall3 or mem_stall3 or reg_stall3 or stall2;

    pcnext <= pc1 when stall2 and stall3 else
              pc2 when stall3 else
              pc3;

    pcaddr1 <= pc;
    pcaddr2 <= pc + "100";
    pcaddr3 <= pc + "1000";


    js: jumpstaller port map(
        jump1 => jump1,
        pcsrc1 => pcsrc1,
        jump2 => jump2,
        pcsrc2 => pcsrc2,
        jump3 => jump3,
        pcsrc3 => pcsrc3,
        stall2 => jump_stall2,
        stall3 => jump_stall3
        ); 

    rs: regstaller port map(
        writereg1 => writereg1,
        instr1 => instr1,
        writereg2 => writereg2,
        instr2 => instr2,
        writereg3 => writereg3,
        instr3 => instr3,
        stall2 => reg_stall2,
        stall3 => reg_stall3
        ); 
    
    ms: memorystaller port map(
        op1 => instr1(31 downto 26),
        basereg1 => instr1(25 downto 21),
        offset1 => instr1(15 downto 0),
        op2 => instr2(31 downto 26),
        basereg2 => instr2(25 downto 21),
        offset2 => instr2(15 downto 0),
        op3 => instr3(31 downto 26),
        basereg3 => instr3(25 downto 21),
        offset3 => instr3(15 downto 0),
        stall2 => mem_stall2,
        stall3 => mem_stall3
    );

    pcreg: flopr generic map(32) port map(clk, reset, pcnext, pc); 
end;